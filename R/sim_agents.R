#' Add an AI or human agent to a live exchange
#'
#' AI agents generate ordinary order requests; they do not bypass the exchange
#' command/execution path.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Agent identifier. If omitted, one is generated.
#' @param agent_type Agent type: `human`, `chaos`, `momentum`, `contrarian`,
#'   `mean_reversion`, or `strategy`.
#' @param config Named list of agent settings. Supported values include `qty`,
#'   `order_type`, `lookback`, `asset_policy`, and for strategy agents
#'   `strategy_id` or `strategy_fun`. Strategy parameters can be supplied with
#'   `param_` prefixes, for example `param_fast = 10`.
#' @param status Initial status: `active` or `paused`.
#' @return The agent id.
#' @export
sim_agent_add <- function(exchange,
                          agent_id = NULL,
                          agent_type = c("chaos", "momentum", "contrarian", "mean_reversion", "strategy", "human"),
                          config = list(),
                          status = c("active", "paused")) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  agent_type <- match.arg(agent_type)
  status <- match.arg(status)
  if (is.null(agent_id) || !nzchar(agent_id)) {
    agent_id <- paste0(agent_type, "-", sprintf("%03d", nrow(exchange$agents) + 1L))
  }
  if (agent_id %in% exchange$agents$agent_id) {
    stop("Agent already exists: ", agent_id, call. = FALSE)
  }
  if (identical(agent_type, "strategy")) {
    validation <- sim_strategy_validate_config(exchange, config)
    if (!isTRUE(validation$valid)) stop(validation$message, call. = FALSE)
  }
  row <- data.table::data.table(
    agent_id = as.character(agent_id),
    agent_type = agent_type,
    status = status,
    config = .agent_config_encode(config),
    created_at = Sys.time()
  )
  exchange$agents <- data.table::rbindlist(list(exchange$agents, row), fill = TRUE)
  .ensure_shared_account(exchange, agent_id, config = config)
  if (nrow(exchange$market_events) > 0L) {
    latest <- exchange$market_events[nrow(exchange$market_events)]
    .ensure_agent_account(
      exchange,
      agent_id,
      asset_id = latest$asset_id[1L],
      symbol = latest$symbol[1L],
      agent_type = agent_type,
      config = config,
      status = status
    )
  }
  invisible(agent_id)
}

#' Set an agent status
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Agent identifier.
#' @param status New status: `active`, `paused`, or `removed`.
#' @return Invisibly returns `TRUE` when an agent was updated.
#' @export
sim_agent_set_status <- function(exchange, agent_id, status = c("active", "paused", "removed")) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  status <- match.arg(status)
  idx <- which(exchange$agents$agent_id == agent_id)
  if (length(idx) == 0L) return(invisible(FALSE))
  data.table::set(exchange$agents, i = idx, j = "status", value = status)
  invisible(TRUE)
}

#' Register a strategy function for strategy-backed AI agents
#'
#' Registered functions are runtime-only and are not serialized into exported
#' CSV files. Store the durable strategy name in the agent config via
#' `strategy_id`; register the function again after loading a saved exchange.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param strategy_id Strategy identifier used by agent config.
#' @param strategy_fn Function returning target positions, strategyr action
#'   plans, or order-intent rows.
#' @return The strategy id, invisibly.
#' @export
sim_strategy_register <- function(exchange, strategy_id, strategy_fn) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (!is.function(strategy_fn)) stop("`strategy_fn` must be a function.", call. = FALSE)
  strategy_id <- as.character(strategy_id %||% "")
  if (!nzchar(strategy_id)) stop("`strategy_id` is required.", call. = FALSE)
  if (is.null(exchange$strategy_registry)) exchange$strategy_registry <- new.env(parent = emptyenv())
  assign(strategy_id, strategy_fn, envir = exchange$strategy_registry)
  invisible(strategy_id)
}

#' Unregister a strategy function
#'
#' @inheritParams sim_strategy_register
#' @return Invisibly returns `TRUE` when a strategy was removed.
#' @export
sim_strategy_unregister <- function(exchange, strategy_id) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  strategy_id <- as.character(strategy_id %||% "")
  if (is.null(exchange$strategy_registry) || !exists(strategy_id, envir = exchange$strategy_registry, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  rm(list = strategy_id, envir = exchange$strategy_registry)
  invisible(TRUE)
}

#' List registered strategy ids
#'
#' @inheritParams sim_strategy_register
#' @return Character vector of registered strategy ids.
#' @export
sim_strategy_list <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$strategy_registry)) return(character())
  sort(ls(envir = exchange$strategy_registry, all.names = TRUE))
}

#' Validate strategy-backed agent config
#'
#' Checks that a strategy agent config points to a registered function or a
#' resolvable function name, and that supplied `param_` keys are compatible with
#' the strategy function signature when the function does not accept `...`.
#'
#' @inheritParams sim_strategy_register
#' @param config Agent config list.
#' @return A list with `valid`, `message`, `strategy_id`, `strategy_fun`, and
#'   `params`.
#' @export
sim_strategy_validate_config <- function(exchange, config) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  config <- config %||% list()
  strategy_id <- as.character(config$strategy_id %||% config$strategy %||% "")
  strategy_fun <- as.character(config$strategy_fun %||% "")
  params <- .agent_strategy_params(config)
  result <- function(valid, message = "ok") {
    list(
      valid = isTRUE(valid),
      message = as.character(message),
      strategy_id = strategy_id,
      strategy_fun = strategy_fun,
      params = names(params)
    )
  }
  strategy_fn <- tryCatch(.agent_strategy_resolve(exchange, config), error = function(err) err)
  if (inherits(strategy_fn, "error")) return(result(FALSE, conditionMessage(strategy_fn)))
  fn_formals <- names(formals(strategy_fn))
  if (!"..." %in% fn_formals) {
    context_names <- c("DT", "market", "state", "account", "positions", "history", "bar", "exchange", "agent", "agent_config", "config")
    unknown <- setdiff(names(params), fn_formals)
    if (length(unknown) > 0L) {
      return(result(FALSE, paste0("Unknown strategy parameter(s): ", paste(unknown, collapse = ", "))))
    }
    required <- setdiff(fn_formals[vapply(formals(strategy_fn), identical, logical(1), quote(expr = ))], c(context_names, names(params)))
    if (length(required) > 0L) {
      return(result(FALSE, paste0("Missing required strategy parameter(s): ", paste(required, collapse = ", "))))
    }
  }
  result(TRUE)
}

#' Remove an agent from a live exchange
#'
#' Removed agents stay in the durable registry with status `removed`.
#'
#' @inheritParams sim_agent_set_status
#' @return Invisibly returns `TRUE` when an agent was updated.
#' @export
sim_agent_remove <- function(exchange, agent_id) {
  sim_agent_set_status(exchange, agent_id, "removed")
}

#' Step active AI agents and append their order commands
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bar Optional current bar used for decision timestamps and prices.
#' @return A data.table of generated decisions.
#' @export
sim_agents_step <- function(exchange, bar = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  agents <- exchange$agents[
    exchange$agents$status == "active" &
      exchange$agents$agent_type != "human"
  ]
  if (nrow(agents) == 0L) return(sim_schemas()$agent_decisions[0])

  bar <- .agent_current_bar(exchange, bar)
  decisions <- list()
  for (i in seq_len(nrow(agents))) {
    agent <- agents[i]
    decision <- tryCatch(.agent_decide(exchange, agent, bar), error = function(err) {
      config <- .agent_config_decode(agent$config)
      .append_strategy_event(exchange, agent, .agent_select_asset_bar(exchange, bar, config), config,
        stage = "error", output_type = "error", status = "error", message = conditionMessage(err)
      )
      NULL
    })
    if (is.null(decision)) next
    decision <- data.table::as.data.table(decision)
    if (nrow(decision) == 0L) next
    decision <- decision[!.agent_decision_is_noop(decision)]
    if (nrow(decision) == 0L) next
    for (j in seq_len(nrow(decision))) {
      row <- decision[j]
      command_id <- sim_submit_order(
        exchange = exchange,
        agent_id = agent$agent_id,
        symbol = row$symbol,
        asset_id = row$asset_id,
        timestamp = row$timestamp,
        tgt_pos = if ("tgt_pos" %in% names(row) && is.finite(row$tgt_pos)) row$tgt_pos else NULL,
        order_type = row$order_type,
        side = row$side,
        qty_type = row$qty_type,
        qty = if (is.finite(row$qty)) row$qty else NULL,
        limit_price = row$limit_price,
        client_order_id = paste0("ai-", agent$agent_id, "-", row$asset_id, "-", format(row$timestamp, "%Y%m%d%H%M%S"), "-", j),
        process = FALSE
      )
      row$command_id <- command_id
      row$status <- "submitted"
      .append_strategy_event(exchange, agent, row, .agent_config_decode(agent$config),
        stage = "submitted_order",
        output_type = row$decision_type,
        symbol = row$symbol,
        asset_id = row$asset_id,
        target = if ("tgt_pos" %in% names(row)) row$tgt_pos else NA_real_,
        status = "submitted",
        message = command_id
      )
      decisions[[length(decisions) + 1L]] <- row
    }
  }
  out <- data.table::rbindlist(decisions, fill = TRUE)
  if (nrow(out) == 0L) return(sim_schemas()$agent_decisions[0])
  if (nrow(out) > 0L) {
    data.table::setcolorder(out, names(sim_schemas()$agent_decisions))
    exchange$agent_decisions <- data.table::rbindlist(list(exchange$agent_decisions, out), fill = TRUE)
  }
  out[]
}

#' Compute current agent rankings
#'
#' Rankings combine current per-agent account equity with order activity.
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A data.table of agent rankings.
#' @export
sim_agent_rankings <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  agents <- data.table::copy(exchange$agents)
  if (nrow(agents) == 0L) return(sim_schemas()$agent_rankings[0])
  orders <- data.table::copy(exchange$agent_orders)
  if (nrow(orders) == 0L) {
    accounts <- sim_exchange_account(exchange)
    out <- agents[, .(
      timestamp = Sys.time(),
      agent_id,
      agent_type,
      status,
      equity = NA_real_,
      cash = NA_real_,
      unrealized_pnl = NA_real_,
      orders = 0L,
      filled_orders = 0L,
      net_qty = 0,
      last_side = NA_character_
    )]
    if (nrow(accounts) > 0L && "agent_id" %in% names(accounts)) {
      data.table::setorderv(accounts, "timestamp")
      acct <- accounts[, .SD[.N], by = agent_id]
      out[, c("equity", "cash", "unrealized_pnl") := NULL]
      out <- merge(out, acct[, .(agent_id, equity, cash, unrealized_pnl)], by = "agent_id", all.x = TRUE)
      data.table::setcolorder(out, c("timestamp", "agent_id", "agent_type", "status", "equity", "cash", "unrealized_pnl", "orders", "filled_orders", "net_qty", "last_side"))
    }
  } else {
    orders[, signed_qty := data.table::fcase(
      side == "buy", abs(qty),
      side == "sell", -abs(qty),
      side == "flat", 0,
      default = 0
    )]
    summary <- orders[, .(
      orders = .N,
      filled_orders = sum(status == "filled", na.rm = TRUE),
      net_qty = sum(signed_qty, na.rm = TRUE),
      last_side = tail(side, 1L)
    ), by = agent_id]
    out <- merge(
      agents[, .(agent_id, agent_type, status)],
      summary,
      by = "agent_id",
      all.x = TRUE
    )
    accounts <- sim_exchange_account(exchange)
    if (nrow(accounts) > 0L && "agent_id" %in% names(accounts)) {
      data.table::setorderv(accounts, "timestamp")
      acct <- accounts[, .SD[.N], by = agent_id]
      out <- merge(out, acct[, .(agent_id, equity, cash, unrealized_pnl)], by = "agent_id", all.x = TRUE)
    } else {
      data.table::set(out, j = "equity", value = NA_real_)
      data.table::set(out, j = "cash", value = NA_real_)
      data.table::set(out, j = "unrealized_pnl", value = NA_real_)
    }
    for (col in c("orders", "filled_orders")) data.table::set(out, which(is.na(out[[col]])), col, 0L)
    data.table::set(out, which(is.na(out$net_qty)), "net_qty", 0)
    data.table::set(out, j = "timestamp", value = Sys.time())
    data.table::setcolorder(out, c("timestamp", "agent_id", "agent_type", "status", "equity", "cash", "unrealized_pnl", "orders", "filled_orders", "net_qty", "last_side"))
  }
  for (col in c("equity", "cash", "unrealized_pnl")) {
    bad <- which(!is.finite(out[[col]]))
    if (length(bad) > 0L) data.table::set(out, i = bad, j = col, value = NA_real_)
  }
  out[, ranking_equity := data.table::fifelse(is.finite(equity), equity, -Inf)]
  data.table::setorder(out, -ranking_equity, -filled_orders, -orders, agent_id)
  out[, ranking_equity := NULL]
  out[, rank := seq_len(.N)]
  exchange$agent_rankings <- out[]
  out[]
}

#' @keywords internal
.agent_current_bar <- function(exchange, bar = NULL) {
  if (!is.null(bar) && nrow(data.table::as.data.table(bar)) > 0L) {
    return(as_market_bars(bar))
  }
  if (nrow(exchange$market_events) > 0L) {
    return(exchange$market_events[nrow(exchange$market_events)])
  }
  now <- Sys.time()
  data.table::data.table(timestamp = now, open = NA_real_, high = NA_real_, low = NA_real_, close = NA_real_)
}

#' @keywords internal
.agent_decide <- function(exchange, agent, bar) {
  config <- .agent_config_decode(agent$config)
  if (identical(as.character(agent$agent_type), "strategy")) {
    return(.agent_strategy_decide(exchange, agent, bar, config))
  }
  qty <- as.numeric(config$qty %||% 1)
  order_type <- as.character(config$order_type %||% "market")
  lookback <- as.integer(config$lookback %||% 12L)
  bar <- .agent_select_asset_bar(exchange, bar, config)
  bars <- data.table::rbindlist(list(exchange$market_events, bar), fill = TRUE)
  if ("asset_id" %in% names(bars)) bars <- bars[asset_id == as.integer(bar$asset_id[1L] %||% 0L)]
  if (all(c("timestamp", "asset_id") %in% names(bars))) {
    data.table::setorderv(bars, c("timestamp", "asset_id"))
    bars <- unique(bars, by = c("timestamp", "asset_id"), fromLast = TRUE)
  }
  bars <- bars[!is.na(close)]
  last_ret <- if (nrow(bars) >= 2L) tail(bars$close, 1L) / tail(bars$close, 2L)[1L] - 1 else 0
  mean_gap <- if (nrow(bars) >= 2L) tail(bars$close, 1L) / mean(tail(bars$close, min(lookback, nrow(bars))), na.rm = TRUE) - 1 else 0
  side <- switch(agent$agent_type,
    chaos = sample(c("buy", "sell", "flat"), 1L),
    momentum = if (last_ret > 0) "buy" else if (last_ret < 0) "sell" else "flat",
    contrarian = if (last_ret > 0) "sell" else if (last_ret < 0) "buy" else "flat",
    mean_reversion = if (mean_gap > 0) "sell" else if (mean_gap < 0) "buy" else "flat",
    NULL
  )
  if (is.null(side)) return(NULL)
  reason <- sprintf("type=%s;asset_policy=%s;last_ret=%.6f;mean_gap=%.6f", agent$agent_type, config$asset_policy %||% "random", last_ret, mean_gap)
  intent <- .agent_execution_intent(exchange, agent$agent_id, side, qty, asset_id = bar$asset_id[1L] %||% 0L)
  data.table::data.table(
    timestamp = as.POSIXct(bar$timestamp, origin = "1970-01-01"),
    agent_id = agent$agent_id,
    agent_type = agent$agent_type,
    symbol = as.character(bar$symbol[1L] %||% "default"),
    asset_id = as.integer(bar$asset_id[1L] %||% 0L),
    decision_type = "order",
    side = side,
    intended_action = intent$action,
    intended_dir = intent$dir,
    qty_type = "contracts",
    qty = qty,
    order_type = order_type,
    limit_price = NA_real_,
    tgt_pos = NA_real_,
    reason = reason,
    command_id = NA_character_,
    status = "planned"
  )
}

#' @keywords internal
.agent_decision_is_noop <- function(decision) {
  if (nrow(decision) == 0L) return(logical())
  as.character(decision$intended_action %||% NA_character_) == "no_op" &
    as.character(decision$intended_dir %||% NA_character_) == "flat"
}

#' @keywords internal
.agent_select_asset_bar <- function(exchange, bar, config) {
  bar <- data.table::as.data.table(bar)
  policy <- as.character(config$asset_policy %||% "random")
  if (!identical(policy, "random")) return(bar[1L])
  if (nrow(bar) > 1L && "asset_id" %in% names(bar)) {
    available <- unique(bar$asset_id)
    selected_asset <- sample(available, 1L)
    return(bar[asset_id == selected_asset][1L])
  }
  assets <- exchange$assets[exchange$assets$status == "active"]
  if (nrow(assets) <= 1L || nrow(exchange$market_events) == 0L) return(bar)
  available <- unique(exchange$market_events$asset_id)
  assets <- assets[asset_id %in% available]
  if (nrow(assets) == 0L) return(bar)
  selected <- assets[sample.int(nrow(assets), 1L)]
  selected_bars <- exchange$market_events[asset_id == selected$asset_id[1L]]
  if (nrow(selected_bars) == 0L) return(bar)
  selected_bars[nrow(selected_bars)]
}

#' @keywords internal
.agent_execution_intent <- function(exchange, agent_id, side, qty, asset_id = 0L) {
  state <- exchange$agent_states[[.agent_state_key(as.character(agent_id), asset_id)]]
  if (is.null(state)) state <- sim_state()
  cur_dir <- as.integer(state$pos_dir %||% 0L)
  cur_qty <- as.numeric(state$ctr_unit %||% 0)
  side <- tolower(as.character(side))
  if (side == "flat") {
    if (cur_dir == 0L || cur_qty <= 0 || !is.finite(cur_qty)) {
      return(list(action = "no_op", dir = "flat"))
    }
    return(list(action = "close", dir = "flat"))
  }
  target_dir <- if (side == "buy") 1L else if (side == "sell") -1L else 0L
  if (target_dir == 0L) return(list(action = "no_op", dir = "flat"))
  target_label <- if (target_dir > 0L) "long" else "short"
  if (cur_dir == 0L) return(list(action = "open", dir = target_label))
  if (cur_dir == target_dir) return(list(action = "increase", dir = target_label))
  list(action = "close", dir = "flat")
}

#' @keywords internal
.agent_target_execution_intent <- function(exchange, agent_id, tgt_pos, asset_id = 0L, tol_pos = 0) {
  state <- exchange$agent_states[[.agent_state_key(as.character(agent_id), asset_id)]]
  if (is.null(state)) state <- sim_state()
  cur_dir <- as.integer(state$pos_dir %||% 0L)
  cur_qty <- as.numeric(state$ctr_unit %||% 0)
  current <- if (cur_dir == 0L || !is.finite(cur_qty)) 0 else cur_dir * cur_qty
  target <- as.numeric(tgt_pos %||% 0)
  tol_pos <- as.numeric(tol_pos %||% 0)
  if (!is.finite(target) || abs(target - current) <= tol_pos) {
    return(list(action = "no_op", dir = "flat"))
  }
  target_dir <- if (target > 0) 1L else if (target < 0) -1L else 0L
  if (target_dir == 0L) return(list(action = "close", dir = "flat"))
  target_label <- if (target_dir > 0L) "long" else "short"
  if (current == 0) return(list(action = "open", dir = target_label))
  if (sign(current) != sign(target)) return(list(action = "close", dir = "flat"))
  if (abs(target) > abs(current)) return(list(action = "increase", dir = target_label))
  list(action = "reduce", dir = target_label)
}

#' @keywords internal
.agent_strategy_decide <- function(exchange, agent, bar, config) {
  strategy_fn <- .agent_strategy_resolve(exchange, config)
  selected_bar <- .agent_select_asset_bar(exchange, bar, config)
  market <- data.table::rbindlist(list(exchange$market_events, bar), fill = TRUE)
  if (nrow(market) > 0L && all(c("timestamp", "asset_id") %in% names(market))) {
    data.table::setorderv(market, c("timestamp", "asset_id"))
    market <- unique(market, by = c("timestamp", "asset_id"), fromLast = TRUE)
  }
  params <- .agent_strategy_params(config)
  state <- .agent_strategy_state(exchange, agent$agent_id, selected_bar, config)
  call_args <- c(
    list(
      DT = .agent_strategy_market_dt(market, selected_bar),
      market = market,
      state = state,
      account = sim_exchange_account(exchange),
      positions = sim_exchange_positions(exchange),
      history = data.table::copy(exchange$market_events),
      bar = selected_bar,
      exchange = exchange,
      agent = as.list(agent),
      agent_config = config,
      config = config
    ),
    params
  )
  out <- .agent_strategy_call(strategy_fn, call_args)
  .append_strategy_event(exchange, agent, selected_bar, config,
    stage = "signal",
    output_type = .agent_strategy_output_type(out),
    target = .agent_strategy_output_target(out),
    status = "ok",
    raw = .agent_strategy_raw(out)
  )
  normalize_bar <- if (is.data.frame(out) || data.table::is.data.table(out)) bar else selected_bar
  .agent_strategy_normalize(exchange, agent, normalize_bar, out, config)
}

#' @keywords internal
.agent_strategy_resolve <- function(exchange, config) {
  strategy_id <- as.character(config$strategy_id %||% config$strategy %||% "")
  strategy_fun <- as.character(config$strategy_fun %||% "")
  if (nzchar(strategy_id) && !is.null(exchange$strategy_registry) &&
      exists(strategy_id, envir = exchange$strategy_registry, inherits = FALSE)) {
    return(get(strategy_id, envir = exchange$strategy_registry, inherits = FALSE))
  }
  fn_name <- if (nzchar(strategy_fun)) strategy_fun else strategy_id
  if (!nzchar(fn_name)) stop("Strategy agent requires `strategy_id` or `strategy_fun` in config.", call. = FALSE)
  .agent_strategy_resolve_name(fn_name)
}

#' @keywords internal
.agent_strategy_resolve_name <- function(fn_name) {
  parts <- strsplit(fn_name, "::", fixed = TRUE)[[1L]]
  if (length(parts) == 2L) {
    if (!requireNamespace(parts[[1L]], quietly = TRUE)) {
      stop("Strategy package is not installed: ", parts[[1L]], call. = FALSE)
    }
    return(getExportedValue(parts[[1L]], parts[[2L]]))
  }
  fn <- get(fn_name, mode = "function", inherits = TRUE)
  if (!is.function(fn)) stop("Strategy function not found: ", fn_name, call. = FALSE)
  fn
}

#' @keywords internal
.agent_strategy_params <- function(config) {
  if (!length(config)) return(list())
  param_names <- grep("^param_", names(config), value = TRUE)
  params <- stats::setNames(lapply(param_names, function(nm) .agent_config_value(config[[nm]])), sub("^param_", "", param_names))
  if (is.list(config$params)) params <- utils::modifyList(params, config$params)
  params
}

#' @keywords internal
.agent_config_value <- function(x) {
  if (length(x) != 1L) return(x)
  if (is.na(x)) return(x)
  lx <- tolower(as.character(x))
  if (lx %in% c("true", "false")) return(identical(lx, "true"))
  nx <- suppressWarnings(as.numeric(x))
  if (!is.na(nx) && grepl("^-?[0-9.]+([eE][-+]?[0-9]+)?$", as.character(x))) return(nx)
  x
}

#' @keywords internal
.agent_strategy_call <- function(strategy_fn, call_args) {
  fn_formals <- names(formals(strategy_fn))
  if ("..." %in% fn_formals) return(do.call(strategy_fn, call_args))
  do.call(strategy_fn, call_args[intersect(names(call_args), fn_formals)])
}

#' @keywords internal
.agent_strategy_market_dt <- function(market, selected_bar) {
  out <- data.table::copy(market)
  if (nrow(out) > 0L && "asset_id" %in% names(out) && "asset_id" %in% names(selected_bar)) {
    out <- out[asset_id == as.integer(selected_bar$asset_id[1L])]
  }
  if (!"datetime" %in% names(out) && "timestamp" %in% names(out)) {
    out[, datetime := timestamp]
  }
  out[]
}

#' @keywords internal
.agent_strategy_state <- function(exchange, agent_id, bar, config) {
  asset_id <- as.integer(bar$asset_id[1L] %||% 0L)
  symbol <- as.character(bar$symbol[1L] %||% "default")
  state <- .ensure_agent_account(exchange, agent_id, asset_id = asset_id, symbol = symbol, agent_type = "strategy", config = config)
  state <- .sync_state_cash_from_account(exchange, agent_id, state)
  state$ctr_size <- as.numeric(exchange$config$ctr_size %||% 1)
  state$ctr_step <- as.numeric(exchange$config$ctr_step %||% 1)
  state$lev <- as.numeric(exchange$config$lev %||% 10)
  state$tol_pos <- as.numeric(config$tol_pos %||% 0)
  state$strat_id <- as.integer(config$strat_id %||% state$strat %||% 0L)
  state
}

#' @keywords internal
.agent_strategy_normalize <- function(exchange, agent, bar, out, config) {
  if (is.null(out)) return(sim_schemas()$agent_decisions[0])
  if (is.list(out) && !is.data.frame(out) && !is.null(out$actions)) {
    return(.agent_strategy_normalize_action_plan(exchange, agent, bar, out, config))
  }
  if (is.numeric(out) && is.null(dim(out))) {
    tgt <- .agent_latest_finite(out)
    row <- .agent_strategy_decision_row(exchange, agent, bar, side = "target", qty_type = "target_pos", qty = abs(tgt), tgt_pos = tgt, config = config, reason = "strategy_target_vector")
    .append_strategy_event(exchange, agent, row, config,
      stage = "target", output_type = "target_vector", symbol = row$symbol,
      asset_id = row$asset_id, target = tgt, status = "planned"
    )
    return(row)
  }
  if (is.data.frame(out) || data.table::is.data.table(out)) {
    return(.agent_strategy_normalize_table(exchange, agent, bar, out, config))
  }
  if (is.list(out) && length(out) && all(vapply(out, is.list, logical(1)))) {
    return(.agent_strategy_normalize_table(exchange, agent, bar, data.table::rbindlist(out, fill = TRUE), config))
  }
  stop("Unsupported strategy output. Return target-position vector, action plan, or order-intent table.", call. = FALSE)
}

#' @keywords internal
.agent_strategy_normalize_action_plan <- function(exchange, agent, bar, out, config) {
  actions <- out$actions %||% list()
  if (!length(actions)) return(sim_schemas()$agent_decisions[0])
  rows <- lapply(seq_along(actions), function(i) {
    action <- actions[[i]]
    action_label <- .agent_action_code_label(action$action %||% NA_integer_)
    dir_label <- .agent_dir_code_label(action$dir %||% NA_integer_)
    side <- if (dir_label == "long") "buy" else if (dir_label == "short") "sell" else "flat"
    .agent_strategy_decision_row(
      exchange, agent, bar,
      side = side,
      qty_type = "contracts",
      qty = as.numeric(action$ctr_qty %||% NA_real_),
      order_type = .agent_order_type_code_label(action$type %||% 0L),
      limit_price = as.numeric(action$px %||% NA_real_),
      config = config,
      intended_action = action_label,
      intended_dir = dir_label,
      reason = sprintf("strategy_action_plan;strat=%s;action_id=%s", action$strat %||% NA, action$action_id %||% i)
    )
  })
  out <- data.table::rbindlist(rows, fill = TRUE)
  .append_strategy_rows(exchange, agent, out, config, stage = "order_intent", output_type = "action_plan")
  out
}

#' @keywords internal
.agent_strategy_normalize_table <- function(exchange, agent, bar, out, config) {
  DT <- data.table::as.data.table(out)
  if (nrow(DT) == 0L) return(sim_schemas()$agent_decisions[0])
  if ("asset" %in% names(DT) && !"symbol" %in% names(DT)) data.table::setnames(DT, "asset", "symbol")
  if ("units" %in% names(DT) && !"qty" %in% names(DT)) data.table::setnames(DT, "units", "qty")
  if ("pricing_method" %in% names(DT) && !"order_type" %in% names(DT)) data.table::setnames(DT, "pricing_method", "order_type")
  if ("reference_price" %in% names(DT) && !"limit_price" %in% names(DT)) data.table::setnames(DT, "reference_price", "limit_price")
  if ("target_position" %in% names(DT) && !"tgt_pos" %in% names(DT)) data.table::setnames(DT, "target_position", "tgt_pos")
  rows <- lapply(seq_len(nrow(DT)), function(i) {
    row <- DT[i]
    row_bar <- .agent_strategy_row_bar(exchange, bar, row)
    tgt_pos <- if ("tgt_pos" %in% names(row)) as.numeric(row$tgt_pos) else NA_real_
    side <- as.character(row$side %||% if (is.finite(tgt_pos)) "target" else "flat")
    side <- tolower(side)
    if (side %in% c("long", "buy_long")) side <- "buy"
    if (side %in% c("short", "sell_short")) side <- "sell"
    qty_type <- as.character(row$qty_type %||% if (identical(side, "target") || is.finite(tgt_pos)) "target_pos" else "contracts")
    qty <- as.numeric(row$qty %||% if (is.finite(tgt_pos)) abs(tgt_pos) else NA_real_)
    if (identical(side, "target") && !is.finite(tgt_pos) && is.finite(qty)) tgt_pos <- qty
    order_type <- as.character(row$order_type %||% "market")
    limit_price <- as.numeric(row$limit_price %||% NA_real_)
    .agent_strategy_decision_row(
      exchange, agent, row_bar,
      side = side,
      qty_type = qty_type,
      qty = qty,
      tgt_pos = tgt_pos,
      order_type = order_type,
      limit_price = limit_price,
      config = config,
      reason = as.character(row$reason %||% row$intent_type %||% "strategy_table")
    )
  })
  out <- data.table::rbindlist(rows, fill = TRUE)
  .append_strategy_rows(exchange, agent, out, config, stage = "order_intent", output_type = .agent_table_output_type(DT))
  out
}

#' @keywords internal
.agent_strategy_row_bar <- function(exchange, bar, row) {
  row_symbol <- as.character(row$symbol %||% NA_character_)
  row_asset_id <- suppressWarnings(as.integer(row$asset_id %||% NA_integer_))
  if (!is.na(row_asset_id) && "asset_id" %in% names(bar)) {
    matched <- bar[asset_id == row_asset_id]
    if (nrow(matched) > 0L) return(matched[1L])
  }
  if (!is.na(row_symbol) && nzchar(row_symbol) && "symbol" %in% names(bar)) {
    matched <- bar[symbol == row_symbol]
    if (nrow(matched) > 0L) return(matched[1L])
  }
  if (!is.na(row_symbol) && nzchar(row_symbol)) {
    asset <- .asset_require_registered(exchange, symbol = row_symbol, asset_id = row_asset_id, context = "strategy order asset")
    latest <- exchange$market_events[asset_id == asset$asset_id]
    if (nrow(latest) > 0L) return(latest[nrow(latest)])
  }
  bar[1L]
}

#' @keywords internal
.agent_strategy_decision_row <- function(exchange, agent, bar, side, qty_type, qty, config, tgt_pos = NA_real_, order_type = NULL, limit_price = NA_real_, intended_action = NULL, intended_dir = NULL, reason = "strategy") {
  side <- tolower(as.character(side %||% "flat"))
  if (!side %in% c("target", "buy", "sell", "flat")) side <- "flat"
  qty_type <- as.character(qty_type %||% if (side == "target") "target_pos" else "contracts")
  if ((qty_type == "target_pos" || side == "target") && !is.finite(tgt_pos) && is.finite(qty)) tgt_pos <- qty
  if ((qty_type == "target_pos" || side == "target") && is.finite(tgt_pos) && !is.finite(qty)) qty <- abs(tgt_pos)
  order_type <- as.character(order_type %||% config$order_type %||% "market")
  if (!order_type %in% c("market", "limit")) order_type <- "market"
  asset_id <- as.integer(bar$asset_id[1L] %||% 0L)
  if (is.null(intended_action) || is.null(intended_dir)) {
    intent <- if (qty_type == "target_pos" || side == "target") {
      .agent_target_execution_intent(exchange, agent$agent_id, tgt_pos, asset_id = asset_id, tol_pos = as.numeric(config$tol_pos %||% 0))
    } else {
      .agent_execution_intent(exchange, agent$agent_id, side, qty, asset_id = asset_id)
    }
    intended_action <- intent$action
    intended_dir <- intent$dir
  }
  data.table::data.table(
    timestamp = as.POSIXct(bar$timestamp, origin = "1970-01-01"),
    agent_id = agent$agent_id,
    agent_type = agent$agent_type,
    symbol = as.character(bar$symbol[1L] %||% "default"),
    asset_id = asset_id,
    decision_type = if (qty_type == "target_pos" || side == "target") "target" else "order",
    side = side,
    intended_action = as.character(intended_action),
    intended_dir = as.character(intended_dir),
    qty_type = qty_type,
    qty = as.numeric(qty),
    order_type = order_type,
    limit_price = as.numeric(limit_price),
    tgt_pos = as.numeric(tgt_pos),
    reason = as.character(reason),
    command_id = NA_character_,
    status = "planned"
  )
}

#' @keywords internal
.agent_latest_finite <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(0)
  tail(x, 1L)
}

#' @keywords internal
.agent_action_code_label <- function(x) {
  x <- as.integer(x)
  if (is.na(x)) return("no_op")
  c("no_op", "open", "close", "increase", "reduce")[pmin(pmax(x + 1L, 1L), 5L)]
}

#' @keywords internal
.agent_dir_code_label <- function(x) {
  x <- as.integer(x)
  if (is.na(x)) return("flat")
  if (x > 0L) "long" else if (x < 0L) "short" else "flat"
}

#' @keywords internal
.agent_order_type_code_label <- function(x) {
  x <- as.integer(x)
  if (!is.na(x) && x == 1L) "limit" else "market"
}

#' @keywords internal
.append_strategy_rows <- function(exchange, agent, rows, config, stage, output_type) {
  if (nrow(rows) == 0L) return(invisible(FALSE))
  for (i in seq_len(nrow(rows))) {
    row <- rows[i]
    .append_strategy_event(exchange, agent, row, config,
      stage = stage,
      output_type = output_type,
      symbol = row$symbol,
      asset_id = row$asset_id,
      target = if ("tgt_pos" %in% names(row)) row$tgt_pos else NA_real_,
      status = row$status %||% "planned",
      message = paste(c(row$side, row$intended_action, row$intended_dir), collapse = "/")
    )
  }
  invisible(TRUE)
}

#' @keywords internal
.append_strategy_event <- function(exchange, agent, bar, config, stage, output_type, symbol = NULL, asset_id = NULL, target = NA_real_, status = "ok", message = NA_character_, raw = NA_character_) {
  if (is.null(exchange$agent_strategy_events)) exchange$agent_strategy_events <- sim_schemas()$agent_strategy_events[0]
  timestamp <- if (!is.null(bar) && "timestamp" %in% names(bar)) bar$timestamp[1L] else Sys.time()
  symbol <- as.character(symbol %||% if (!is.null(bar) && "symbol" %in% names(bar)) bar$symbol[1L] else NA_character_)
  asset_id <- as.integer(asset_id %||% if (!is.null(bar) && "asset_id" %in% names(bar)) bar$asset_id[1L] else NA_integer_)
  row <- data.table::data.table(
    timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
    agent_id = as.character(agent$agent_id),
    strategy_id = as.character(config$strategy_id %||% config$strategy %||% NA_character_),
    strategy_fun = as.character(config$strategy_fun %||% NA_character_),
    stage = as.character(stage),
    output_type = as.character(output_type),
    symbol = symbol,
    asset_id = asset_id,
    signal = as.character(message),
    target = as.numeric(target),
    status = as.character(status),
    message = as.character(message),
    raw = as.character(raw)
  )
  exchange$agent_strategy_events <- data.table::rbindlist(list(exchange$agent_strategy_events, row), fill = TRUE)
  invisible(row)
}

#' @keywords internal
.agent_strategy_output_type <- function(out) {
  if (is.null(out)) return("none")
  if (is.numeric(out) && is.null(dim(out))) return("target_vector")
  if (is.list(out) && !is.data.frame(out) && !is.null(out$actions)) return("action_plan")
  if (is.data.frame(out) || data.table::is.data.table(out)) return(.agent_table_output_type(data.table::as.data.table(out)))
  if (is.list(out)) return("list")
  class(out)[1L]
}

#' @keywords internal
.agent_table_output_type <- function(DT) {
  names_dt <- names(DT)
  if (any(c("tgt_pos", "target_position") %in% names_dt)) return("target_table")
  if (any(c("units", "qty", "side", "pricing_method", "reference_price") %in% names_dt)) return("order_intent_table")
  "table"
}

#' @keywords internal
.agent_strategy_output_target <- function(out) {
  if (is.numeric(out) && is.null(dim(out))) return(.agent_latest_finite(out))
  if (is.data.frame(out) || data.table::is.data.table(out)) {
    DT <- data.table::as.data.table(out)
    col <- intersect(c("tgt_pos", "target_position"), names(DT))[1L]
    if (!is.na(col)) return(.agent_latest_finite(DT[[col]]))
  }
  NA_real_
}

#' @keywords internal
.agent_strategy_raw <- function(out) {
  paste(utils::capture.output(utils::str(out, give.attr = FALSE, vec.len = 5L)), collapse = " ")
}

#' @keywords internal
.agent_config_encode <- function(config) {
  if (!length(config)) return("")
  paste(names(config), unlist(config, use.names = FALSE), sep = "=", collapse = ";")
}

#' @keywords internal
.agent_config_decode <- function(config) {
  if (is.null(config) || is.na(config) || !nzchar(config)) return(list())
  parts <- strsplit(config, ";", fixed = TRUE)[[1L]]
  out <- list()
  for (part in parts) {
    kv <- strsplit(part, "=", fixed = TRUE)[[1L]]
    if (length(kv) >= 2L) out[[kv[[1L]]]] <- kv[[2L]]
  }
  out
}
