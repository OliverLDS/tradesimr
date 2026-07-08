#' Add an AI or human agent to a live exchange
#'
#' AI agents generate ordinary order requests; they do not bypass the exchange
#' command/execution path.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Agent identifier. If omitted, one is generated.
#' @param agent_type Agent type: `human`, `chaos`, `momentum`, `contrarian`, or
#'   `mean_reversion`.
#' @param config Named list of agent settings. Supported values include `qty`,
#'   `order_type`, and `lookback`.
#' @param status Initial status: `active` or `paused`.
#' @return The agent id.
#' @export
sim_agent_add <- function(exchange,
                          agent_id = NULL,
                          agent_type = c("chaos", "momentum", "contrarian", "mean_reversion", "human"),
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
  row <- data.table::data.table(
    agent_id = as.character(agent_id),
    agent_type = agent_type,
    status = status,
    config = .agent_config_encode(config),
    created_at = Sys.time()
  )
  exchange$agents <- data.table::rbindlist(list(exchange$agents, row), fill = TRUE)
  .ensure_agent_account(exchange, agent_id, agent_type = agent_type, config = config, status = status)
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
  decisions <- vector("list", nrow(agents))
  for (i in seq_len(nrow(agents))) {
    agent <- agents[i]
    decision <- .agent_decide(exchange, agent, bar)
    if (is.null(decision)) next
    command_id <- sim_submit_order(
      exchange = exchange,
      agent_id = agent$agent_id,
      symbol = decision$symbol,
      asset_id = decision$asset_id,
      timestamp = decision$timestamp,
      order_type = decision$order_type,
      side = decision$side,
      qty_type = decision$qty_type,
      qty = decision$qty,
      limit_price = decision$limit_price,
      client_order_id = paste0("ai-", agent$agent_id, "-", format(decision$timestamp, "%Y%m%d%H%M%S")),
      process = FALSE
    )
    decision$command_id <- command_id
    decision$status <- "submitted"
    decisions[[i]] <- decision
  }
  out <- data.table::rbindlist(decisions, fill = TRUE)
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
    return(as_market_bars(bar)[1L])
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
  qty <- as.numeric(config$qty %||% 1)
  order_type <- as.character(config$order_type %||% "market")
  lookback <- as.integer(config$lookback %||% 12L)
  bars <- data.table::rbindlist(list(exchange$market_events, bar), fill = TRUE)
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
  reason <- sprintf("type=%s;last_ret=%.6f;mean_gap=%.6f", agent$agent_type, last_ret, mean_gap)
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
    reason = reason,
    command_id = NA_character_,
    status = "planned"
  )
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
