#' Create an in-memory simulated exchange state
#'
#' @param config Named list of simulation parameters passed to `sim_backtest()`.
#' @return A mutable environment containing market, intent, order, and result
#'   tables.
#' @export
sim_exchange_new <- function(config = list()) {
  state <- new.env(parent = emptyenv())
  state$config <- config
  state$market_events <- sim_schemas()$market_events[0]
  state$intents <- sim_schemas()$intents[0]
  state$agent_orders <- sim_schemas()$agent_orders[0]
  state$portfolio_targets <- sim_schemas()$portfolio_targets[0]
  state$portfolio_rebalances <- sim_schemas()$portfolio_rebalances[0]
  state$portfolio_fills <- sim_schemas()$portfolio_fills[0]
  state$portfolio_market_boundaries <- sim_schemas()$portfolio_market_boundaries[0]
  state$agent_commands <- sim_schemas()$agent_commands[0]
  state$order_requests <- sim_schemas()$order_requests[0]
  state$order_cancellations <- sim_schemas()$order_cancellations[0]
  state$agents <- sim_schemas()$agents[0]
  state$agent_decisions <- sim_schemas()$agent_decisions[0]
  state$agent_strategy_events <- sim_schemas()$agent_strategy_events[0]
  state$agent_rankings <- sim_schemas()$agent_rankings[0]
  state$assets <- sim_schemas()$assets[0]
  state$agent_states <- list()
  state$agent_accounts <- list()
  state$asset_symbols <- list()
  state$strategy_registry <- new.env(parent = emptyenv())
  state$feeds <- list()
  state$market_model <- sim_market_model_config()
  state$event_log <- data.table::data.table(
    timestamp = as.POSIXct(character()),
    source = character(),
    event = character(),
    ref_id = character()
  )
  state$result <- NULL
  state$last_result <- NULL
  state$last_events <- data.table::data.table()
  state$new_events <- data.table::data.table()
  state_config <- config
  if (!is.null(state_config$init_cash) && is.null(state_config$cash)) {
    state_config$cash <- state_config$init_cash
  }
  state$step_state <- do.call(sim_state, state_config[intersect(names(state_config), names(formals(sim_state)))])
  state$step_snapshots <- data.table::data.table()
  state$step_events <- data.table::data.table()
  state$last_bar_count <- 0L
  state$next_order_id <- 1L
  state$next_command_id <- 1L
  state$next_rebalance_id <- 1L
  state$next_fill_id <- 1L
  state$feed <- sim_feed_config()
  class(state) <- c("tradesimr_exchange", "environment")
  state
}

#' Append market bars to a simulated exchange
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars Market bars coercible by `as_market_bars()`.
#' @return The exchange, invisibly.
#' @export
sim_exchange_add_bars <- function(exchange, bars) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  new_bars <- as_market_bars(bars)
  new_bars <- .validate_market_bar_assets(exchange, new_bars)
  exchange$market_events <- data.table::rbindlist(list(exchange$market_events, new_bars), fill = TRUE)
  invisible(exchange)
}

#' Place an order into a simulated exchange
#'
#' Explicit orders use `qty_type = "contracts"` by default for buy/sell/flat
#' orders. Use `qty_type = "target_pos"` or `side = "target"` for exposure
#' targets consumed by replay-style backtests.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Agent identifier.
#' @param timestamp Order timestamp.
#' @param symbol Registered asset symbol.
#' @param asset_id Registered asset identifier. Provide `symbol`, `asset_id`,
#'   or both when they identify the same asset.
#' @param tgt_pos Target exposure. Kept for compatibility with earlier
#'   intent-level calls.
#' @param tol_pos Target-position tolerance.
#' @param order_type Order type: `market` or `limit`.
#' @param side Order side: `target`, `buy`, `sell`, or `flat`.
#' @param qty Order quantity. Meaning is controlled by `qty_type`.
#' @param qty_type Quantity semantics: `contracts` or `target_pos`.
#' @param limit_price Optional limit price for limit orders.
#' @param time_in_force Time-in-force label.
#' @param client_order_id Optional client order id.
#' @return The generated order id.
#' @export
sim_exchange_place_order <- function(exchange,
                                     agent_id,
                                     timestamp,
                                     symbol = NULL,
                                     asset_id = NULL,
                                     tgt_pos = NULL,
                                     tol_pos = 0,
                                     order_type = c("market", "limit"),
                                     side = c("target", "buy", "sell", "flat"),
                                     qty_type = NULL,
                                     qty = NULL,
                                     limit_price = NA_real_,
                                     time_in_force = "gtc",
                                     client_order_id = NA_character_) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  asset <- .asset_require_registered(exchange, symbol = symbol, asset_id = asset_id, context = "order asset")
  .ensure_agent_account(exchange, agent_id, asset_id = asset$asset_id, symbol = asset$symbol, agent_type = "human")
  order_type <- match.arg(order_type)
  side <- match.arg(side)
  if (is.null(qty_type)) qty_type <- if (side == "target") "target_pos" else "contracts"
  qty_type <- match.arg(qty_type, c("contracts", "target_pos"))
  if (qty_type == "contracts" && side == "target") {
    stop("`side = 'target'` requires `qty_type = 'target_pos'`.", call. = FALSE)
  }
  if (qty_type == "contracts" && is.null(qty) && side != "flat") {
    stop("`qty` is required for contract orders.", call. = FALSE)
  }
  order_id <- paste0("ORD", sprintf("%06d", exchange$next_order_id))
  exchange$next_order_id <- exchange$next_order_id + 1L
  if (is.null(qty)) qty <- if (is.null(tgt_pos)) NA_real_ else abs(as.numeric(tgt_pos))
  target <- .order_to_target_pos(side = side, qty = qty, qty_type = qty_type, tgt_pos = tgt_pos)
  row <- data.table::data.table(
    order_id = order_id,
    client_order_id = as.character(client_order_id),
    agent_id = as.character(agent_id),
    symbol = asset$symbol,
    asset_id = asset$asset_id,
    timestamp = timestamp,
    eligible_after = as.POSIXct(NA),
    rebalance_id = NA_character_,
    target_weight = NA_real_,
    decision_price = NA_real_,
    order_type = order_type,
    side = side,
    qty_type = qty_type,
    qty = as.numeric(qty),
    limit_price = as.numeric(limit_price),
    time_in_force = as.character(time_in_force),
    tgt_pos = target,
    tol_pos = as.numeric(tol_pos),
    status = "accepted",
    price = NA_real_,
    fee = NA_real_,
    realized_pnl = NA_real_,
    message = NA_character_
  )
  exchange$agent_orders <- data.table::rbindlist(list(exchange$agent_orders, row), fill = TRUE)
  exchange$event_log <- data.table::rbindlist(list(exchange$event_log, data.table::data.table(
    timestamp = timestamp,
    source = "agent_order",
    event = "accepted",
    ref_id = order_id
  )), fill = TRUE)
  invisible(order_id)
}

#' Cancel an intent-level order in a simulated exchange
#'
#' @param exchange A `tradesimr_exchange`.
#' @param order_id Order id returned by `sim_exchange_place_order()`.
#' @return Invisibly returns `TRUE` if an order was cancelled.
#' @export
sim_exchange_cancel_order <- function(exchange, order_id) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  idx <- which(exchange$agent_orders$order_id == order_id & exchange$agent_orders$status == "accepted")
  if (length(idx) == 0L) return(invisible(FALSE))
  data.table::set(exchange$agent_orders, i = idx, j = "status", value = "cancelled")
  exchange$event_log <- data.table::rbindlist(list(exchange$event_log, data.table::data.table(
    timestamp = Sys.time(),
    source = "agent_order",
    event = "cancelled",
    ref_id = order_id
  )), fill = TRUE)
  invisible(TRUE)
}

#' Run or refresh a simulated exchange replay
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A simulation result returned by `sim_backtest()`.
#' @export
sim_exchange_run <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  bars <- data.table::copy(exchange$market_events)
  if (nrow(bars) == 0L) stop("No market bars have been added.", call. = FALSE)

  active_orders <- exchange$agent_orders[
    exchange$agent_orders$status == "accepted" &
      exchange$agent_orders$qty_type == "target_pos"
  ]
  if (nrow(active_orders) == 0L) {
    data.table::set(bars, j = "tgt_pos", value = rep.int(0, nrow(bars)))
    data.table::set(bars, j = "tol_pos", value = rep.int(0, nrow(bars)))
  } else {
    order_intents <- active_orders[, .SD, .SDcols = c("timestamp", "tgt_pos", "tol_pos", "order_type", "limit_price")]
    data.table::setorderv(order_intents, "timestamp")
    data.table::set(bars, j = "tgt_pos", value = rep.int(0, nrow(bars)))
    data.table::set(bars, j = "tol_pos", value = rep.int(0, nrow(bars)))
    data.table::set(bars, j = "order_type", value = rep.int("market", nrow(bars)))
    data.table::set(bars, j = "limit_price", value = rep.int(NA_real_, nrow(bars)))
    for (i in seq_len(nrow(order_intents))) {
      idx <- which(bars$timestamp >= order_intents$timestamp[i])
      if (length(idx) > 0L) {
        data.table::set(bars, i = idx, j = "tgt_pos", value = order_intents$tgt_pos[i])
        data.table::set(bars, i = idx, j = "tol_pos", value = order_intents$tol_pos[i])
        data.table::set(bars, i = idx, j = "order_type", value = order_intents$order_type[i])
        data.table::set(bars, i = idx, j = "limit_price", value = order_intents$limit_price[i])
      }
    }
  }

  exchange$last_result <- exchange$result
  args <- c(list(
    data = bars,
    tol_pos_col = "tol_pos",
    order_type_col = if ("order_type" %in% names(bars)) "order_type" else NULL,
    limit_price_col = if ("limit_price" %in% names(bars)) "limit_price" else NULL
  ), exchange$config[intersect(names(exchange$config), names(formals(sim_backtest)))])
  exchange$result <- do.call(sim_backtest, args)
  current_events <- sim_events(exchange$result)
  if (nrow(exchange$last_events) == 0L) {
    exchange$new_events <- current_events
  } else {
    old_max <- max(exchange$last_events$event_id, na.rm = TRUE)
    exchange$new_events <- current_events[current_events$event_id > old_max]
  }
  exchange$last_events <- current_events
  exchange$last_bar_count <- nrow(exchange$market_events)
  exchange$result
}

#' Step a simulated exchange with one or more bars
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars Market bars coercible by `as_market_bars()`.
#' @return The incremental simulation snapshots.
#' @export
sim_exchange_step <- function(exchange, bars) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  new_bars <- as_market_bars(bars)
  new_bars <- .validate_market_bar_assets(exchange, new_bars)
  if (isTRUE(exchange$config$portfolio_margin %||% FALSE)) {
    return(.sim_exchange_step_portfolio(exchange, new_bars))
  }
  exchange$market_events <- data.table::rbindlist(list(exchange$market_events, new_bars), fill = TRUE)
  step_results <- vector("list", nrow(new_bars))
  new_event_list <- list()

  for (i in seq_len(nrow(new_bars))) {
    bar <- new_bars[i]
    asset <- .bar_asset_key(bar)
    agents <- .exchange_agents_to_step(exchange, bar$timestamp[1L], asset_id = asset$asset_id)
    for (j in seq_along(agents)) {
      agent_id <- agents[[j]]
      if (!.portfolio_agent_asset_allowed(exchange, agent_id, asset$asset_id)) {
        .portfolio_reject_forbidden_orders(exchange, agent_id, asset$asset_id)
        next
      }
      state_key <- .agent_state_key(agent_id, asset$asset_id)
      .ensure_agent_account(exchange, agent_id, asset_id = asset$asset_id, symbol = asset$symbol, agent_type = "human")
      orders <- .exchange_orders_for_bar(exchange, bar$timestamp[1L], agent_id = agent_id, asset_id = asset$asset_id)
      exchange$agent_states[[state_key]] <- .sync_state_cash_from_account(exchange, agent_id, exchange$agent_states[[state_key]])
      cash_before <- as.numeric(exchange$agent_states[[state_key]]$cash %||% 0)
      step_config <- exchange$config[intersect(names(exchange$config), names(formals(sim_step)))]
      step_config$init_cash <- NULL
      step_config$fill_model <- NULL
      step_config$order_type_col <- NULL
      step_config$limit_price_col <- NULL
      step_config$asset <- asset$asset_id
      # Contract precision belongs to the registered asset, not to the
      # exchange-wide fallback. This preserves fractional crypto contracts.
      asset_spec <- exchange$assets[exchange$assets$asset_id == asset$asset_id]
      if (nrow(asset_spec) > 0L) {
        if (is.finite(asset_spec$qty_step[1L]) && asset_spec$qty_step[1L] > 0) {
          step_config$ctr_step <- as.numeric(asset_spec$qty_step[1L])
        }
        if (is.finite(asset_spec$contract_size[1L]) && asset_spec$contract_size[1L] > 0) {
          step_config$ctr_size <- as.numeric(asset_spec$contract_size[1L])
        }
      }
      step_args <- c(list(
        state = exchange$agent_states[[state_key]],
        bar = bar,
        orders = orders
      ), step_config)
      step <- do.call(sim_step, step_args)
      cash_after <- as.numeric(step$state$cash %||% cash_before)
      .update_shared_cash(exchange, agent_id, cash_after - cash_before)
      step$state <- .sync_state_cash_from_account(exchange, agent_id, step$state)
      exchange$agent_states[[state_key]] <- step$state
      if (j == 1L) exchange$step_state <- step$state
      if (nrow(step$events) > 0L) {
        data.table::set(step$events, j = "agent_id", value = agent_id)
        data.table::set(step$events, j = "symbol", value = asset$symbol)
        data.table::set(step$events, j = "asset_id", value = asset$asset_id)
        new_event_list[[length(new_event_list) + 1L]] <- step$events
        .mark_orders_from_events(exchange, orders, step$events)
      }
      .enforce_cross_margin(exchange, agent_id, bar$timestamp[1L])
    }
    account_snapshots <- lapply(agents, function(agent_id) .agent_position_snapshots(exchange, agent_id, bar$timestamp[1L]))
    step_results[[i]] <- data.table::rbindlist(account_snapshots, fill = TRUE)
  }

  new_snapshots <- data.table::rbindlist(step_results, fill = TRUE)
  exchange$step_snapshots <- data.table::rbindlist(list(exchange$step_snapshots, new_snapshots), fill = TRUE)
  exchange$new_events <- data.table::rbindlist(new_event_list, fill = TRUE)
  exchange$step_events <- data.table::rbindlist(list(exchange$step_events, exchange$new_events), fill = TRUE)
  exchange$result <- exchange$step_snapshots
  data.table::setattr(exchange$result, "market_events", exchange$market_events)
  data.table::setattr(exchange$result, "events", exchange$step_events)
  data.table::setattr(exchange$result, "orders", sim_orders(exchange$step_events))
  exchange$last_events <- exchange$step_events
  exchange$last_bar_count <- nrow(exchange$market_events)
  exchange$result
}

#' @keywords internal
.sim_exchange_step_portfolio <- function(exchange, new_bars) {
  exchange$market_events <- data.table::rbindlist(list(exchange$market_events, new_bars), fill = TRUE)
  data.table::setorderv(new_bars, intersect(c("timestamp", "asset_id"), names(new_bars)))
  step_results <- list()
  new_event_list <- list()
  timestamps <- unique(new_bars$timestamp)

  for (ts_val in timestamps) {
    batch <- new_bars[timestamp == ts_val]
    asset_ids <- as.integer(batch$asset_id)
    agents <- unique(unlist(lapply(asset_ids, function(asset_id) {
      .exchange_agents_to_step(exchange, batch$timestamp[1L], asset_id = asset_id)
    }), use.names = FALSE))
    if (!length(agents)) next

    for (agent_id in agents) {
      permitted <- vapply(asset_ids, function(asset_id) .portfolio_agent_asset_allowed(exchange, agent_id, asset_id), logical(1L))
      for (asset_id in asset_ids[!permitted]) .portfolio_reject_forbidden_orders(exchange, agent_id, asset_id)
      agent_batch <- batch[permitted]
      if (nrow(agent_batch) == 0L) next
      # A registered account with neither a position state nor an eligible
      # order has nothing to mark. Deferring its state creation avoids an
      # O(agents x assets) C++ warm-up before its first decision.
      state_or_order <- vapply(seq_len(nrow(agent_batch)), function(i) {
        asset_id <- as.integer(agent_batch$asset_id[i])
        state_key <- .agent_state_key(agent_id, asset_id)
        has_state <- !is.null(exchange$agent_states[[state_key]])
        has_order <- any(
          exchange$agent_orders$agent_id == agent_id &
            exchange$agent_orders$asset_id == asset_id &
            exchange$agent_orders$status == "accepted" &
            exchange$agent_orders$qty_type == "contracts"
        )
        has_state || has_order
      }, logical(1L))
      agent_batch <- agent_batch[state_or_order]
      if (nrow(agent_batch) == 0L) next
      states <- list()
      orders_all <- list()
      for (i in seq_len(nrow(agent_batch))) {
        asset <- .bar_asset_key(agent_batch[i])
        .ensure_agent_account(exchange, agent_id, asset_id = asset$asset_id, symbol = asset$symbol, agent_type = "human")
        state_key <- .agent_state_key(agent_id, asset$asset_id)
        states[[as.character(asset$asset_id)]] <- .sync_state_cash_from_account(exchange, agent_id, exchange$agent_states[[state_key]])
        orders <- .exchange_orders_for_bar(exchange, agent_batch$timestamp[i], agent_id = agent_id, asset_id = asset$asset_id)
        if (nrow(orders) > 0L) {
          data.table::set(orders, j = "asset_id", value = asset$asset_id)
          data.table::set(orders, j = "symbol", value = asset$symbol)
          orders_all[[length(orders_all) + 1L]] <- orders
        }
      }

      orders <- data.table::rbindlist(orders_all, fill = TRUE)
      agent_asset_ids <- as.integer(agent_batch$asset_id)
      cov <- .cross_asset_covariance(exchange, agent_asset_ids)
      step_config <- exchange$config[intersect(names(exchange$config), names(formals(sim_portfolio_step)))]
      asset_specs <- exchange$assets[match(agent_asset_ids, exchange$assets$asset_id)]
      if (anyNA(asset_specs$asset_id)) stop("Missing registered asset specification for portfolio step.", call. = FALSE)
      step_config$ctr_step <- as.numeric(asset_specs$qty_step)
      step_config$ctr_size <- as.numeric(asset_specs$contract_size)
      step_config$cov <- cov
      step_config$shared_cash <- .shared_cash(exchange, agent_id)
      step_config$portfolio_margin_floor <- as.numeric(exchange$config$portfolio_margin_floor %||% exchange$config$mmr %||% 0.02)
      step_args <- c(list(states = states, bars = agent_batch, orders = orders), step_config)
      step <- do.call(sim_portfolio_step, step_args)

      exchange$agent_accounts[[as.character(agent_id)]]$cash <- as.numeric(step$cash %||% 0)
      exchange$agent_accounts[[as.character(agent_id)]]$liquidated <- isTRUE(step$liquidated)
      for (asset_id in names(step$states)) {
        state_key <- .agent_state_key(agent_id, as.integer(asset_id))
        exchange$agent_states[[state_key]] <- step$states[[asset_id]]
        exchange$agent_states[[state_key]]$cash <- as.numeric(step$cash %||% 0)
      }
      if (nrow(step$events) > 0L) {
        data.table::set(step$events, j = "agent_id", value = agent_id)
        if ("asset_id" %in% names(step$events)) {
          symbols <- vapply(as.integer(step$events$asset_id), function(asset_id) {
            exchange$asset_symbols[[as.character(asset_id)]] %||% paste0("asset-", asset_id)
          }, character(1L))
          data.table::set(step$events, j = "symbol", value = symbols)
        }
        new_event_list[[length(new_event_list) + 1L]] <- step$events
        .mark_orders_from_events(exchange, orders, step$events)
      }
      account_snapshots <- .agent_position_snapshots(exchange, agent_id, agent_batch$timestamp[1L])
      if (nrow(account_snapshots) > 0L) {
        data.table::set(account_snapshots, j = "maintenance_margin", value = as.numeric(step$maintenance_margin %||% 0))
      }
      step_results[[length(step_results) + 1L]] <- account_snapshots
    }
  }

  new_snapshots <- data.table::rbindlist(step_results, fill = TRUE)
  exchange$step_snapshots <- data.table::rbindlist(list(exchange$step_snapshots, new_snapshots), fill = TRUE)
  exchange$new_events <- data.table::rbindlist(new_event_list, fill = TRUE)
  exchange$step_events <- data.table::rbindlist(list(exchange$step_events, exchange$new_events), fill = TRUE)
  exchange$result <- exchange$step_snapshots
  data.table::setattr(exchange$result, "market_events", exchange$market_events)
  data.table::setattr(exchange$result, "events", exchange$step_events)
  data.table::setattr(exchange$result, "orders", sim_orders(exchange$step_events))
  exchange$last_events <- exchange$step_events
  exchange$last_bar_count <- nrow(exchange$market_events)
  exchange$result
}

#' Get simulated exchange orders
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A data.table of accepted/cancelled intent-level orders.
#' @export
sim_exchange_orders <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  data.table::copy(exchange$agent_orders)
}

#' Get new events since the previous exchange run
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A data.table of newly observed simulation events.
#' @export
sim_exchange_new_events <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  data.table::copy(exchange$new_events)
}

#' Get simulated exchange account state
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A one-row data.table with the latest account snapshot.
#' @export
sim_exchange_account <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$result)) {
    snapshots <- .agent_states_snapshot(exchange)
    shared <- .shared_accounts_snapshot(exchange, latest = TRUE)
    if (nrow(snapshots) == 0L) return(shared)
    if (nrow(shared) > 0L) {
      missing_agents <- setdiff(shared$agent_id, snapshots$agent_id)
      if (length(missing_agents) > 0L) {
        snapshots <- data.table::rbindlist(list(snapshots, shared[agent_id %in% missing_agents]), fill = TRUE)
      }
    }
    return(.aggregate_account_snapshots(sim_account(snapshots), latest = TRUE))
  }
  account <- sim_account(exchange$result)
  if (nrow(account) == 0L) return(account)
  if ("agent_id" %in% names(account)) {
    data.table::setorderv(account, "timestamp")
    latest <- if ("asset_id" %in% names(account)) account[, .SD[.N], by = .(agent_id, asset_id)] else account[, .SD[.N], by = agent_id]
    account <- .aggregate_account_snapshots(latest, latest = TRUE)
    shared <- .shared_accounts_snapshot(exchange, latest = TRUE)
    missing_agents <- setdiff(shared$agent_id, account$agent_id)
    if (length(missing_agents)) {
      account <- data.table::rbindlist(list(account, shared[shared$agent_id %in% missing_agents]), fill = TRUE)
    }
    return(account[])
  }
  tail(account, 1L)
}

#' Get simulated exchange positions
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A one-row data.table with the latest position snapshot.
#' @export
sim_exchange_positions <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$result)) {
    snapshots <- .agent_states_snapshot(exchange)
    return(sim_positions(snapshots))
  }
  positions <- sim_positions(exchange$result)
  if (nrow(positions) == 0L) return(positions)
  if (all(c("agent_id", "asset_id") %in% names(positions))) {
    keep <- mapply(
      function(agent_id, asset_id) .portfolio_agent_asset_allowed(exchange, agent_id, asset_id),
      positions$agent_id, positions$asset_id
    )
    positions <- positions[keep]
  }
  if (all(c("agent_id", "asset_id") %in% names(positions))) {
    data.table::setorderv(positions, "timestamp")
    return(positions[, .SD[.N], by = .(agent_id, asset_id)])
  }
  if ("agent_id" %in% names(positions)) {
    data.table::setorderv(positions, "timestamp")
    return(positions[, .SD[.N], by = agent_id])
  }
  tail(positions, 1L)
}

#' Export exchange events and state
#'
#' @param exchange A `tradesimr_exchange`.
#' @param path Output directory.
#' @param format File format, either `csv` or `fst`.
#' @return Invisibly returns written file paths.
#' @export
sim_exchange_save <- function(exchange, path, format = c("csv", "fst")) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  format <- match.arg(format)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  if (!is.null(exchange$result) && nrow(exchange$result) > 0L && ncol(exchange$result) > 0L) {
    paths <- sim_export(exchange$result, path, format = format)
  } else {
    paths <- character()
  }
  state_tables <- list(
    exchange_config = data.table::data.table(config = .serialize_field(exchange$config)),
    market_events = exchange$market_events,
    agent_orders = exchange$agent_orders,
    portfolio_targets = exchange$portfolio_targets,
    portfolio_rebalances = exchange$portfolio_rebalances,
    portfolio_fills = exchange$portfolio_fills,
    portfolio_market_boundaries = exchange$portfolio_market_boundaries,
    agent_commands = exchange$agent_commands,
    order_requests = exchange$order_requests,
    order_cancellations = exchange$order_cancellations,
    agents = exchange$agents,
    assets = exchange$assets,
    agent_decisions = exchange$agent_decisions,
    agent_strategy_events = exchange$agent_strategy_events,
    agent_rankings = sim_agent_rankings(exchange),
    market_model = .market_model_table(exchange),
    cross_asset_risk = sim_cross_asset_risk(exchange),
    feed_status = .feed_status_scalar_table(exchange),
    feed_configs = .feed_status_table(exchange),
    exchange_event_log = exchange$event_log
  )
  for (nm in names(state_tables)) {
    file <- file.path(path, paste0(nm, ".", format))
    if (format == "csv") {
      data.table::fwrite(state_tables[[nm]], file)
    } else {
      if (!requireNamespace("fst", quietly = TRUE)) stop("Package `fst` is required for fst export.", call. = FALSE)
      fst::write_fst(as.data.frame(state_tables[[nm]]), file)
    }
    paths[[nm]] <- file
  }
  invisible(paths)
}

#' Load exchange state from disk
#'
#' @param path Directory produced by `sim_exchange_save()`.
#' @return A `tradesimr_exchange`.
#' @export
sim_exchange_load <- function(path) {
  config_file <- file.path(path, "exchange_config.csv")
  saved_config <- if (file.exists(config_file)) {
    config_table <- data.table::fread(config_file)
    if (nrow(config_table) > 0L && "config" %in% names(config_table)) {
      .unserialize_field(config_table$config[1L]) %||% list()
    } else {
      list()
    }
  } else {
    list()
  }
  exchange <- sim_exchange_new(saved_config)
  manifest_file <- file.path(path, "manifest.csv")
  if (file.exists(manifest_file)) {
    imported <- sim_import(path)
    exchange$result <- imported$simulation
    if (!is.null(exchange$result) && "timestamp" %in% names(exchange$result)) {
      data.table::set(exchange$result, j = "timestamp", value = as.POSIXct(exchange$result$timestamp, tz = "UTC"))
    }
    exchange$last_events <- if (!is.null(imported$events)) sim_events(imported$events) else data.table::data.table()
    if ("timestamp" %in% names(exchange$last_events)) {
      data.table::set(exchange$last_events, j = "timestamp", value = as.POSIXct(exchange$last_events$timestamp, tz = "UTC"))
    }
    exchange$step_events <- exchange$last_events
    exchange$step_snapshots <- if (!is.null(imported$simulation)) imported$simulation else data.table::data.table()
    if (!is.null(imported$simulation) && nrow(imported$simulation) > 0L) {
      last <- imported$simulation[nrow(imported$simulation)]
      exchange$step_state <- sim_state(
        cash = last$cash,
        pos_dir = last$pos_dir,
        ctr_unit = last$ctr_unit,
        avg_price = last$avg_price,
        last_px = last$last_px,
        old_timestamp = as.numeric(last$timestamp)
      )
    }
  }
  if (file.exists(file.path(path, "market_events.csv"))) exchange$market_events <- data.table::fread(file.path(path, "market_events.csv"))
  if (file.exists(file.path(path, "agent_orders.csv"))) {
    exchange$agent_orders <- data.table::fread(file.path(path, "agent_orders.csv"))
    numeric_columns <- intersect(c("qty", "limit_price", "tgt_pos", "tol_pos", "price", "fee", "realized_pnl", "target_weight", "decision_price"), names(exchange$agent_orders))
    for (column in numeric_columns) data.table::set(exchange$agent_orders, j = column, value = as.numeric(exchange$agent_orders[[column]]))
    if ("asset_id" %in% names(exchange$agent_orders)) data.table::set(exchange$agent_orders, j = "asset_id", value = as.integer(exchange$agent_orders$asset_id))
    for (column in intersect(c("timestamp", "eligible_after"), names(exchange$agent_orders))) {
      data.table::set(exchange$agent_orders, j = column, value = as.POSIXct(exchange$agent_orders[[column]], tz = "UTC"))
    }
  }
  if (file.exists(file.path(path, "portfolio_targets.csv"))) {
    exchange$portfolio_targets <- data.table::fread(file.path(path, "portfolio_targets.csv"))
    for (column in intersect(c("timestamp", "eligible_after"), names(exchange$portfolio_targets))) {
      data.table::set(exchange$portfolio_targets, j = column, value = as.POSIXct(exchange$portfolio_targets[[column]], tz = "UTC"))
    }
  }
  if (file.exists(file.path(path, "portfolio_rebalances.csv"))) {
    exchange$portfolio_rebalances <- data.table::fread(file.path(path, "portfolio_rebalances.csv"))
    if ("timestamp" %in% names(exchange$portfolio_rebalances)) {
      data.table::set(exchange$portfolio_rebalances, j = "timestamp", value = as.POSIXct(exchange$portfolio_rebalances$timestamp, tz = "UTC"))
    }
  }
  if (file.exists(file.path(path, "portfolio_fills.csv"))) {
    exchange$portfolio_fills <- data.table::fread(file.path(path, "portfolio_fills.csv"))
    for (column in intersect(c("timestamp"), names(exchange$portfolio_fills))) {
      data.table::set(exchange$portfolio_fills, j = column, value = as.POSIXct(exchange$portfolio_fills[[column]], tz = "UTC"))
    }
    for (column in intersect(c("asset_id", "event_id", "action_id"), names(exchange$portfolio_fills))) {
      data.table::set(exchange$portfolio_fills, j = column, value = as.integer(exchange$portfolio_fills[[column]]))
    }
    for (column in intersect(c("qty", "price", "fee", "realized_pnl", "target_weight"), names(exchange$portfolio_fills))) {
      data.table::set(exchange$portfolio_fills, j = column, value = as.numeric(exchange$portfolio_fills[[column]]))
    }
  }
  if (file.exists(file.path(path, "portfolio_market_boundaries.csv"))) {
    exchange$portfolio_market_boundaries <- data.table::fread(file.path(path, "portfolio_market_boundaries.csv"))
    if ("timestamp" %in% names(exchange$portfolio_market_boundaries)) {
      data.table::set(exchange$portfolio_market_boundaries, j = "timestamp", value = as.POSIXct(exchange$portfolio_market_boundaries$timestamp, tz = "UTC"))
    }
    if ("asset_id" %in% names(exchange$portfolio_market_boundaries)) {
      data.table::set(exchange$portfolio_market_boundaries, j = "asset_id", value = as.integer(exchange$portfolio_market_boundaries$asset_id))
    }
  }
  if (file.exists(file.path(path, "agent_commands.csv"))) exchange$agent_commands <- data.table::fread(file.path(path, "agent_commands.csv"))
  if (file.exists(file.path(path, "order_requests.csv"))) exchange$order_requests <- data.table::fread(file.path(path, "order_requests.csv"))
  if (file.exists(file.path(path, "order_cancellations.csv"))) exchange$order_cancellations <- data.table::fread(file.path(path, "order_cancellations.csv"))
  if (file.exists(file.path(path, "agents.csv"))) {
    exchange$agents <- data.table::fread(file.path(path, "agents.csv"))
    if ("created_at" %in% names(exchange$agents)) {
      data.table::set(exchange$agents, j = "created_at", value = as.POSIXct(exchange$agents$created_at, tz = "UTC"))
    }
  }
  if (file.exists(file.path(path, "assets.csv"))) exchange$assets <- data.table::fread(file.path(path, "assets.csv"))
  if (file.exists(file.path(path, "agent_decisions.csv"))) exchange$agent_decisions <- data.table::fread(file.path(path, "agent_decisions.csv"))
  if (file.exists(file.path(path, "agent_strategy_events.csv"))) exchange$agent_strategy_events <- data.table::fread(file.path(path, "agent_strategy_events.csv"))
  if (file.exists(file.path(path, "agent_rankings.csv"))) exchange$agent_rankings <- data.table::fread(file.path(path, "agent_rankings.csv"))
  if (file.exists(file.path(path, "market_model.csv"))) exchange$market_model <- .market_model_from_table(data.table::fread(file.path(path, "market_model.csv")))
  if (file.exists(file.path(path, "feed_configs.csv"))) {
    feed_configs <- data.table::fread(file.path(path, "feed_configs.csv"))
    if (nrow(feed_configs) > 0L) {
      for (i in seq_len(nrow(feed_configs))) {
        row <- feed_configs[i]
        value <- function(name, default = NA) if (name %in% names(row)) row[[name]][1L] else default
        ar_value <- as.character(value("ar", ""))
        ar_coef <- if (nzchar(ar_value)) as.numeric(strsplit(ar_value, ",", fixed = TRUE)[[1L]]) else numeric()
        config <- list(
          symbol = value("symbol", "default"),
          asset_id = as.integer(value("asset_id", 0L)),
          timeframe = value("timeframe", "4h"),
          tz = value("tz", "UTC"),
          feed_mode = value("feed_mode", "simulation"),
          simulation_model = value("simulation_model", "random_walk") %||% "random_walk",
          random_walk = list(
            start_price = as.numeric(value("start_price", 100)),
            drift = as.numeric(value("drift", 0)),
            vol = as.numeric(value("vol", 0.02)),
            seed = as.integer(value("seed", 1L))
          ),
          simulation = list(
            ar = list(a = ar_coef),
            garch11 = list(
              alpha1 = as.numeric(value("alpha1", NA_real_)),
              beta1 = as.numeric(value("beta1", NA_real_)),
              z_dist = value("z_dist", "norm")
            ),
            shock = list(
              jump_intensity = as.numeric(value("jump_intensity", 0)),
              jump_mean = as.numeric(value("jump_mean", 0)),
              jump_sd = as.numeric(value("jump_sd", 0))
            ),
            ohlc = list(
              model = value("ohlc_model", "wiggle"),
              bridge_steps = as.integer(value("bridge_steps", 12L))
            )
          ),
          simulation_state = .unserialize_field(value("simulation_state", NA_character_)),
          running = .truthy(value("running", FALSE)),
          last_completed_end = if (!is.na(value("last_completed_end", NA))) as.POSIXct(value("last_completed_end", NA), tz = value("tz", "UTC") %||% "UTC") else NULL,
          last_price = as.numeric(value("last_price", NA_real_))
        )
        sim_feed_configure(exchange, config)
        if (!is.null(config$simulation_state)) {
          key <- as.character(config$asset_id)
          exchange$feeds[[key]]$simulation_state <- config$simulation_state
        }
      }
    }
  }
  if (nrow(exchange$agents) > 0L) {
    for (agent_id in exchange$agents$agent_id) {
      config <- .agent_config_decode(exchange$agents$config[match(agent_id, exchange$agents$agent_id)])
      .ensure_shared_account(exchange, agent_id, config = config)
    }
    if (nrow(exchange$step_snapshots) > 0L && "agent_id" %in% names(exchange$step_snapshots)) {
      by_cols <- intersect(c("agent_id", "asset_id"), names(exchange$step_snapshots))
      latest <- exchange$step_snapshots[order(timestamp), .SD[.N], by = by_cols]
      for (i in seq_len(nrow(latest))) {
        asset_id <- as.integer(latest$asset_id[i] %||% 0L)
        symbol <- as.character(latest$symbol[i] %||% paste0("asset-", asset_id))
        exchange$asset_symbols[[as.character(asset_id)]] <- symbol
        if (!symbol %in% exchange$assets$symbol) {
          sim_asset_add(exchange, symbol = symbol, asset_id = asset_id)
        }
        exchange$agent_states[[.agent_state_key(latest$agent_id[i], asset_id)]] <- sim_state(
          cash = latest$cash[i],
          pos_dir = latest$pos_dir[i],
          ctr_unit = latest$ctr_unit[i],
          avg_price = latest$avg_price[i],
          last_px = latest$last_px[i],
          asset = asset_id,
          old_timestamp = as.numeric(latest$timestamp[i])
        )
        exchange$agent_accounts[[as.character(latest$agent_id[i])]]$cash <- as.numeric(latest$cash[i])
      }
    }
  }
  if (file.exists(file.path(path, "feed_status.csv"))) {
    feed_status <- data.table::fread(file.path(path, "feed_status.csv"))
    if (nrow(feed_status) > 0L) {
      feed <- exchange$feed
      for (nm in intersect(names(feed_status), names(feed))) feed[[nm]] <- feed_status[[nm]][1L]
      if (!is.null(feed$last_completed_end)) feed$last_completed_end <- as.POSIXct(feed$last_completed_end, tz = feed$tz %||% "UTC")
      exchange$feed <- feed
    }
  }
  if (file.exists(file.path(path, "exchange_event_log.csv"))) {
    exchange$event_log <- data.table::fread(file.path(path, "exchange_event_log.csv"))
    if ("timestamp" %in% names(exchange$event_log)) {
      data.table::set(exchange$event_log, j = "timestamp", value = as.POSIXct(exchange$event_log$timestamp, tz = "UTC"))
    }
  }
  if (nrow(exchange$agent_orders) > 0L) {
    numeric_ids <- suppressWarnings(as.integer(sub("^ORD", "", exchange$agent_orders$order_id)))
    exchange$next_order_id <- max(numeric_ids, na.rm = TRUE) + 1L
  }
  if (nrow(exchange$agent_commands) > 0L) {
    numeric_command_ids <- suppressWarnings(as.integer(sub("^CMD", "", exchange$agent_commands$command_id)))
    exchange$next_command_id <- max(numeric_command_ids, na.rm = TRUE) + 1L
  }
  if (nrow(exchange$portfolio_rebalances) > 0L) {
    numeric_rebalance_ids <- suppressWarnings(as.integer(sub("^RB", "", exchange$portfolio_rebalances$rebalance_id)))
    exchange$next_rebalance_id <- max(numeric_rebalance_ids, na.rm = TRUE) + 1L
  }
  if (nrow(exchange$portfolio_fills) > 0L) {
    numeric_fill_ids <- suppressWarnings(as.integer(sub("^FILL", "", exchange$portfolio_fills$fill_id)))
    exchange$next_fill_id <- max(numeric_fill_ids, na.rm = TRUE) + 1L
  }
  exchange
}

#' Export exchange simulation events
#'
#' @param exchange A `tradesimr_exchange`.
#' @param path Output directory.
#' @param format File format, either `csv` or `fst`.
#' @return Invisibly returns written file paths.
#' @export
sim_exchange_export_events <- function(exchange, path, format = c("csv", "fst")) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$result)) stop("Exchange has no simulation result to export.", call. = FALSE)
  sim_export(exchange$result, path, format = match.arg(format), tables = c("events", "orders", "fills", "account", "risk"))
}

#' @keywords internal
.order_to_target_pos <- function(side, qty, qty_type, tgt_pos) {
  if (qty_type == "contracts") return(NA_real_)
  if (side == "target") {
    if (is.null(tgt_pos)) stop("`tgt_pos` is required when side = 'target'.", call. = FALSE)
    return(as.numeric(tgt_pos))
  }
  if (side == "buy") return(abs(as.numeric(qty)))
  if (side == "sell") return(-abs(as.numeric(qty)))
  if (side == "flat") return(0)
  stop("Unsupported order side: ", side, call. = FALSE)
}

#' @keywords internal
.exchange_orders_for_bar <- function(exchange, timestamp, agent_id = NULL, asset_id = NULL) {
  bar_timestamp <- timestamp
  orders <- exchange$agent_orders[
    exchange$agent_orders$status == "accepted" &
      exchange$agent_orders$qty_type == "contracts" &
      exchange$agent_orders$timestamp <= bar_timestamp
  ]
  if ("eligible_after" %in% names(orders)) {
    orders <- orders[is.na(eligible_after) | eligible_after < bar_timestamp]
  }
  if (!is.null(agent_id)) {
    requested_agent_id <- as.character(agent_id)
    orders <- orders[orders[["agent_id"]] == requested_agent_id]
  }
  if (!is.null(asset_id) && "asset_id" %in% names(orders)) {
    requested_asset_id <- as.integer(asset_id)
    orders <- orders[orders[["asset_id"]] == requested_asset_id]
  }
  if (nrow(orders) == 0L) {
    return(data.table::data.table(
      order_id = character(),
      action = character(),
      dir = character(),
      order_type = character(),
      ctr_qty = numeric(),
      price = numeric(),
      strat_id = integer(),
      action_id = integer(),
      fee_aware_target = logical(),
      rebalance_id = character(),
      target_weight = numeric()
    ))
  }
  state_key <- .agent_state_key(agent_id %||% "default", asset_id %||% 0L)
  step_state <- if (!is.null(agent_id) && !is.null(exchange$agent_states[[state_key]])) exchange$agent_states[[state_key]] else exchange$step_state
  cur_dir <- as.integer(step_state$pos_dir %||% 0L)
  cur_qty <- as.numeric(step_state$ctr_unit %||% 0)
  side <- tolower(as.character(orders$side))
  action <- character()
  dir <- character()
  ctr_qty <- numeric()
  order_id <- character()
  order_type <- character()
  price <- numeric()
  action_id <- integer()
  fee_aware_target <- logical()
  rebalance_id <- character()
  target_weight <- numeric()
  next_action_id <- as.integer(step_state$action_id_now %||% 1L)
  for (i in seq_len(nrow(orders))) {
    direct_action <- tolower(as.character(orders$intended_action[i]))
    direct_dir <- tolower(as.character(orders$intended_dir[i]))
    if (is.na(direct_action)) direct_action <- ""
    if (is.na(direct_dir)) direct_dir <- ""
    if (direct_action %in% c("open", "increase", "reduce", "close") && direct_dir %in% c("long", "short", "flat")) {
      this_qty <- abs(as.numeric(orders$qty[i]))
      if (!is.finite(this_qty) || this_qty <= 0) {
        .mark_orders_noop(exchange, orders$order_id[i])
        next
      }
      this_action <- direct_action
      this_dir <- direct_dir
      if (this_action %in% c("open", "increase")) {
        cur_dir <- if (this_dir == "long") 1L else -1L
        cur_qty <- if (this_action == "open") this_qty else cur_qty + this_qty
      } else if (this_action == "close") {
        cur_dir <- 0L
        cur_qty <- 0
      } else {
        cur_qty <- max(0, cur_qty - this_qty)
        if (cur_qty == 0) cur_dir <- 0L
      }
      action <- c(action, this_action)
      dir <- c(dir, this_dir)
      ctr_qty <- c(ctr_qty, this_qty)
      order_id <- c(order_id, orders$order_id[i])
      order_type <- c(order_type, orders$order_type[i])
      price <- c(price, orders$limit_price[i])
      action_id <- c(action_id, next_action_id)
      fee_aware_target <- c(
        fee_aware_target,
        "rebalance_id" %in% names(orders) && !is.na(orders$rebalance_id[i])
      )
      rebalance_id <- c(rebalance_id, as.character(orders$rebalance_id[i] %||% NA_character_))
      target_weight <- c(target_weight, as.numeric(orders$target_weight[i] %||% NA_real_))
      next_action_id <- next_action_id + 1L
      next
    }
    if (!side[i] %in% c("buy", "sell", "flat")) {
      stop("Contract orders require side `buy`, `sell`, or `flat`.", call. = FALSE)
    }
    target_dir <- if (side[i] == "buy") 1L else if (side[i] == "sell") -1L else 0L
    target_qty <- abs(as.numeric(orders$qty[i]))
    if (target_dir == 0L) {
      if (cur_dir == 0L || cur_qty <= 0 || !is.finite(cur_qty)) {
        .mark_orders_noop(exchange, orders$order_id[i])
        next
      }
      this_action <- "close"
      this_dir <- "flat"
      this_qty <- cur_qty
      cur_dir <- 0L
      cur_qty <- 0
    } else if (cur_dir == 0L) {
      this_action <- "open"
      this_dir <- if (target_dir > 0L) "long" else "short"
      this_qty <- target_qty
      cur_dir <- target_dir
      cur_qty <- target_qty
    } else if (cur_dir == target_dir) {
      this_action <- "increase"
      this_dir <- if (target_dir > 0L) "long" else "short"
      this_qty <- target_qty
      cur_qty <- cur_qty + target_qty
    } else {
      this_action <- "close"
      this_dir <- "flat"
      this_qty <- cur_qty
      cur_dir <- 0L
      cur_qty <- 0
    }
    if (!is.finite(this_qty) || this_qty <= 0) {
      .mark_orders_noop(exchange, orders$order_id[i])
      next
    }
    action <- c(action, this_action)
    dir <- c(dir, this_dir)
    ctr_qty <- c(ctr_qty, this_qty)
    order_id <- c(order_id, orders$order_id[i])
    order_type <- c(order_type, orders$order_type[i])
    price <- c(price, orders$limit_price[i])
    action_id <- c(action_id, next_action_id)
    fee_aware_target <- c(fee_aware_target, FALSE)
    rebalance_id <- c(rebalance_id, as.character(orders$rebalance_id[i] %||% NA_character_))
    target_weight <- c(target_weight, as.numeric(orders$target_weight[i] %||% NA_real_))
    next_action_id <- next_action_id + 1L
  }
  data.table::data.table(
    order_id = order_id,
    action = action,
    dir = dir,
    order_type = order_type,
    ctr_qty = ctr_qty,
    price = price,
    strat_id = rep.int(0L, length(action_id)),
    action_id = action_id,
    fee_aware_target = fee_aware_target,
    rebalance_id = rebalance_id,
    target_weight = target_weight
  )
}

#' @keywords internal
.portfolio_reject_forbidden_orders <- function(exchange, agent_id, asset_id) {
  requested_agent_id <- as.character(agent_id)
  index <- which(
    exchange$agent_orders$agent_id == requested_agent_id &
      exchange$agent_orders$asset_id == as.integer(asset_id) &
      exchange$agent_orders$status == "accepted"
  )
  if (length(index)) {
    data.table::set(exchange$agent_orders, i = index, j = "status", value = "rejected")
    if (!"message" %in% names(exchange$agent_orders)) {
      data.table::set(exchange$agent_orders, j = "message", value = NA_character_)
    }
    data.table::set(
      exchange$agent_orders,
      i = index,
      j = "message",
      value = "Order asset is outside the agent allowed universe."
    )
  }
  invisible(index)
}

#' @keywords internal
.exchange_agents_to_step <- function(exchange, timestamp, asset_id = NULL) {
  bar_timestamp <- timestamp
  registered <- if (nrow(exchange$agents) > 0L) exchange$agents$agent_id[exchange$agents$status != "removed"] else character()
  order_rows <- exchange$agent_orders[
    exchange$agent_orders$status == "accepted" &
      exchange$agent_orders$qty_type == "contracts" &
      exchange$agent_orders$timestamp <= bar_timestamp
  ]
  if (!is.null(asset_id) && "asset_id" %in% names(order_rows)) {
    requested_asset_id <- as.integer(asset_id)
    order_rows <- order_rows[order_rows[["asset_id"]] == requested_asset_id]
  }
  order_agents <- if (nrow(order_rows) > 0L) unique(order_rows$agent_id) else character()
  agents <- unique(c(registered, order_agents))
  agents
}

#' @keywords internal
.ensure_agent_account <- function(exchange, agent_id, asset_id = NULL, symbol = NULL, agent_type = "human", config = list(), status = "active") {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  agent_id <- as.character(agent_id %||% "agent")
  asset <- .asset_require_registered(exchange, symbol = symbol, asset_id = asset_id, context = "agent account asset")
  state_key <- .agent_state_key(agent_id, asset$asset_id)
  exchange$asset_symbols[[as.character(asset$asset_id)]] <- asset$symbol
  .ensure_shared_account(exchange, agent_id, config = config)
  if (is.null(exchange$agent_states)) exchange$agent_states <- list()
  if (!agent_id %in% exchange$agents$agent_id) {
    row <- data.table::data.table(
      agent_id = agent_id,
      agent_type = as.character(agent_type %||% "human"),
      status = as.character(status %||% "active"),
      config = .agent_config_encode(config),
      created_at = Sys.time()
    )
    exchange$agents <- data.table::rbindlist(list(exchange$agents, row), fill = TRUE)
  }
  if (is.null(exchange$agent_states[[state_key]])) {
    state_config <- exchange$config
    if (!is.null(state_config$init_cash) && is.null(state_config$cash)) {
      state_config$cash <- state_config$init_cash
    }
    if (!is.null(config$initial_cash)) {
      state_config$cash <- as.numeric(exchange$agent_accounts[[agent_id]]$cash)
    }
    state_config$cash <- as.numeric(exchange$agent_accounts[[agent_id]]$cash)
    asset_events <- exchange$market_events
    if (nrow(asset_events) > 0L && "asset_id" %in% names(asset_events)) asset_events <- asset_events[asset_id == asset$asset_id]
    if (nrow(asset_events) > 0L && is.null(state_config$last_px)) {
      state_config$last_px <- as.numeric(tail(asset_events$close, 1L))
    }
    state_config$asset <- asset$asset_id
    exchange$agent_states[[state_key]] <- do.call(sim_state, state_config[intersect(names(state_config), names(formals(sim_state)))])
  }
  invisible(exchange$agent_states[[state_key]])
}

#' @keywords internal
.ensure_shared_account <- function(exchange, agent_id, config = list()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  agent_id <- as.character(agent_id %||% "agent")
  if (is.null(exchange$agent_accounts)) exchange$agent_accounts <- list()
  if (is.null(exchange$agent_accounts[[agent_id]])) {
    account_config <- exchange$config
    if (!is.null(account_config$init_cash) && is.null(account_config$cash)) account_config$cash <- account_config$init_cash
    if (!is.null(config$initial_cash)) account_config$cash <- as.numeric(config$initial_cash)
    cash <- as.numeric(account_config$cash %||% 10000)
    exchange$agent_accounts[[agent_id]] <- list(
      cash = cash,
      initial_cash = cash,
      liquidated = FALSE
    )
  }
  invisible(exchange$agent_accounts[[agent_id]])
}

#' @keywords internal
.mark_orders_from_events <- function(exchange, orders, events) {
  if (nrow(orders) == 0L || nrow(events) == 0L) return(invisible(NULL))
  filled_events <- events[events$status_label == "filled"]
  failed_events <- events[events$status_label == "failed"]
  for (col in c("price", "fee", "realized_pnl")) {
    if (!col %in% names(exchange$agent_orders)) data.table::set(exchange$agent_orders, j = col, value = NA_real_)
  }
  if (!"message" %in% names(exchange$agent_orders)) {
    data.table::set(exchange$agent_orders, j = "message", value = NA_character_)
  }
  for (i in seq_len(nrow(orders))) {
    order_idx <- match(orders$order_id[i], exchange$agent_orders$order_id)
    if (is.na(order_idx)) next
    canonical_order <- exchange$agent_orders[order_idx]
    if (!.portfolio_agent_asset_allowed(exchange, canonical_order$agent_id[1L], canonical_order$asset_id[1L])) {
      .portfolio_reject_forbidden_orders(exchange, canonical_order$agent_id[1L], canonical_order$asset_id[1L])
      next
    }
    event_idx <- .match_order_event(orders[i], filled_events)
    if (is.na(event_idx)) next
    if (is.na(order_idx) || is.na(event_idx)) next
    data.table::set(exchange$agent_orders, i = order_idx, j = "status", value = "filled")
    for (col in c("price", "fee", "realized_pnl")) {
      if (col %in% names(filled_events)) {
        data.table::set(exchange$agent_orders, i = order_idx, j = col, value = as.numeric(filled_events[[col]][event_idx]))
      }
    }
    .append_portfolio_fill(exchange, orders[i], filled_events[event_idx])
  }
  for (i in seq_len(nrow(orders))) {
    event_idx <- .match_order_event(orders[i], failed_events)
    if (is.na(event_idx)) next
    order_idx <- match(orders$order_id[i], exchange$agent_orders$order_id)
    if (is.na(order_idx) || is.na(event_idx)) next
    data.table::set(exchange$agent_orders, i = order_idx, j = "status", value = "rejected")
    data.table::set(
      exchange$agent_orders,
      i = order_idx,
      j = "message",
      value = "Execution rejected at the eligible market boundary."
    )
    if ("price" %in% names(failed_events)) {
      data.table::set(exchange$agent_orders, i = order_idx, j = "price", value = as.numeric(failed_events$price[event_idx]))
    }
  }
  invisible(NULL)
}

#' @keywords internal
.match_order_event <- function(order, events) {
  if (nrow(events) == 0L) return(NA_integer_)
  idx <- which(events$action_id == as.integer(order$action_id[1L]))
  if ("asset_id" %in% names(order) && "asset_id" %in% names(events)) {
    idx <- idx[events$asset_id[idx] == as.integer(order$asset_id[1L])]
  }
  if (length(idx) == 1L) as.integer(idx) else NA_integer_
}

#' @keywords internal
.append_portfolio_fill <- function(exchange, order, event) {
  requested_order_id <- as.character(order$order_id[1L])
  canonical_order <- exchange$agent_orders[exchange$agent_orders$order_id == requested_order_id]
  if (nrow(canonical_order) != 1L || is.na(canonical_order$rebalance_id[1L])) return(invisible(NULL))
  if (requested_order_id %in% exchange$portfolio_fills$order_id) return(invisible(NULL))
  fill_id <- paste0("FILL", sprintf("%06d", exchange$next_fill_id %||% 1L))
  exchange$next_fill_id <- as.integer(exchange$next_fill_id %||% 1L) + 1L
  row <- data.table::data.table(
    fill_id = fill_id,
    timestamp = event$timestamp[1L],
    agent_id = as.character(canonical_order$agent_id[1L]),
    order_id = requested_order_id,
    rebalance_id = as.character(canonical_order$rebalance_id[1L]),
    symbol = as.character(canonical_order$symbol[1L]),
    asset_id = as.integer(canonical_order$asset_id[1L]),
    event_id = as.integer(event$event_id[1L] %||% NA_integer_),
    action_id = as.integer(event$action_id[1L]),
    side = as.character(canonical_order$side[1L]),
    action = as.character(event$action_label[1L]),
    status = "filled",
    action_label = as.character(event$action_label[1L]),
    status_label = "filled",
    dir_label = as.character(event$dir_label[1L]),
    qty = as.numeric(event$ctr_qty[1L]),
    ctr_qty = as.numeric(event$ctr_qty[1L]),
    price = as.numeric(event$price[1L]),
    fee = as.numeric(event$fee[1L] %||% 0),
    realized_pnl = as.numeric(event$realized_pnl[1L] %||% 0),
    target_weight = as.numeric(canonical_order$target_weight[1L])
  )
  exchange$portfolio_fills <- data.table::rbindlist(list(exchange$portfolio_fills, row), fill = TRUE)
  invisible(NULL)
}

#' @keywords internal
.mark_orders_noop <- function(exchange, order_id) {
  idx <- match(order_id, exchange$agent_orders$order_id)
  idx <- idx[!is.na(idx)]
  if (length(idx) > 0L) {
    data.table::set(exchange$agent_orders, i = idx, j = "status", value = "no_op")
    if (!"message" %in% names(exchange$agent_orders)) {
      data.table::set(exchange$agent_orders, j = "message", value = NA_character_)
    }
    data.table::set(exchange$agent_orders, i = idx, j = "message", value = "Order requires no executable position change.")
  }
  invisible(NULL)
}

#' @keywords internal
.state_to_snapshot <- function(state, timestamp, agent_id = NA_character_, symbol = "default", asset_id = 0L) {
  cash <- as.numeric(state$cash %||% 0)
  pos_dir <- as.integer(state$pos_dir %||% 0L)
  ctr_unit <- as.numeric(state$ctr_unit %||% 0)
  avg_price <- as.numeric(state$avg_price %||% NA_real_)
  last_px <- as.numeric(state$last_px %||% 0)
  notional <- as.numeric(state$notional %||% (pos_dir * ctr_unit * last_px))
  abs_notional <- as.numeric(state$abs_notional %||% abs(notional))
  unrealized_pnl <- as.numeric(state$unrealized_pnl %||% 0)
  maintenance_margin <- as.numeric(state$maintenance_margin %||% 0)
  equity <- as.numeric(state$equity %||% (cash + unrealized_pnl))
  data.table::data.table(
    timestamp = timestamp,
    agent_id = as.character(agent_id),
    symbol = as.character(symbol),
    asset_id = as.integer(asset_id),
    equity = equity,
    cash = cash,
    pos_dir = pos_dir,
    ctr_unit = ctr_unit,
    avg_price = avg_price,
    last_px = last_px,
    notional = notional,
    abs_notional = abs_notional,
    unrealized_pnl = unrealized_pnl,
    maintenance_margin = maintenance_margin
  )
}

#' @keywords internal
.agent_states_snapshot <- function(exchange, timestamp = Sys.time()) {
  if (is.null(exchange$agent_states) || !length(exchange$agent_states)) {
    return(data.table::data.table())
  }
  rows <- lapply(names(exchange$agent_states), function(key) {
    parsed <- .parse_agent_state_key(key)
    symbol <- exchange$asset_symbols[[as.character(parsed$asset_id)]] %||% parsed$symbol
    .state_to_snapshot(
      .sync_state_cash_from_account(exchange, parsed$agent_id, exchange$agent_states[[key]]),
      timestamp,
      agent_id = parsed$agent_id,
      symbol = symbol,
      asset_id = parsed$asset_id
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

#' @keywords internal
.shared_accounts_snapshot <- function(exchange, timestamp = Sys.time(), latest = FALSE) {
  if (is.null(exchange$agent_accounts) || !length(exchange$agent_accounts)) {
    return(sim_schemas()$account_snapshots[0])
  }
  rows <- lapply(names(exchange$agent_accounts), function(agent_id) {
    account <- exchange$agent_accounts[[agent_id]]
    data.table::data.table(
      timestamp = timestamp,
      agent_id = agent_id,
      symbol = NA_character_,
      asset_id = NA_integer_,
      equity = as.numeric(account$cash %||% 0),
      cash = as.numeric(account$cash %||% 0),
      notional = 0,
      abs_notional = 0,
      unrealized_pnl = 0,
      maintenance_margin = 0
    )
  })
  out <- data.table::rbindlist(rows, fill = TRUE)
  if (isTRUE(latest)) return(out)
  out[]
}

#' @keywords internal
.aggregate_account_snapshots <- function(account, latest = FALSE) {
  if (nrow(account) == 0L || !"agent_id" %in% names(account)) return(account)
  data.table::setDT(account)
  for (col in c("equity", "cash", "notional", "abs_notional", "unrealized_pnl", "maintenance_margin")) {
    if (!col %in% names(account)) data.table::set(account, j = col, value = NA_real_)
  }
  by_cols <- intersect(c("timestamp", "agent_id"), names(account))
  sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  first_or_na <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) NA_real_ else x[1L]
  }
  out <- account[, .(
    cash = first_or_na(cash),
    notional = sum_or_na(notional),
    abs_notional = sum_or_na(abs_notional),
    unrealized_pnl = sum_or_na(unrealized_pnl),
    maintenance_margin = sum_or_na(maintenance_margin)
  ), by = by_cols]
  out[, equity := cash + unrealized_pnl]
  if (isTRUE(latest) && "timestamp" %in% names(out)) {
    data.table::setorderv(out, "timestamp")
    out <- out[, .SD[.N], by = agent_id]
  }
  out[]
}

#' @keywords internal
.validate_market_bar_assets <- function(exchange, bars) {
  if (nrow(bars) == 0L) return(bars)
  for (i in seq_len(nrow(bars))) {
    asset <- .asset_require_registered(
      exchange,
      symbol = bars$symbol[i],
      asset_id = bars$asset_id[i],
      context = "market bar asset"
    )
    data.table::set(bars, i = i, j = "symbol", value = asset$symbol)
    data.table::set(bars, i = i, j = "asset_id", value = asset$asset_id)
  }
  bars[]
}

#' @keywords internal
.shared_cash <- function(exchange, agent_id) {
  account <- exchange$agent_accounts[[as.character(agent_id)]]
  as.numeric(account$cash %||% exchange$config$cash %||% exchange$config$init_cash %||% 10000)
}

#' @keywords internal
.sync_state_cash_from_account <- function(exchange, agent_id, state) {
  state$cash <- .shared_cash(exchange, agent_id)
  state
}

#' @keywords internal
.update_shared_cash <- function(exchange, agent_id, delta) {
  agent_id <- as.character(agent_id)
  if (is.null(exchange$agent_accounts[[agent_id]])) {
    exchange$agent_accounts[[agent_id]] <- list(cash = as.numeric(exchange$config$cash %||% exchange$config$init_cash %||% 10000), initial_cash = as.numeric(exchange$config$cash %||% exchange$config$init_cash %||% 10000), liquidated = FALSE)
  }
  cash <- as.numeric(exchange$agent_accounts[[agent_id]]$cash %||% 0) + as.numeric(delta %||% 0)
  exchange$agent_accounts[[agent_id]]$cash <- cash
  invisible(cash)
}

#' @keywords internal
.agent_position_snapshots <- function(exchange, agent_id, timestamp) {
  keys <- names(exchange$agent_states)
  if (!length(keys)) return(data.table::data.table())
  rows <- lapply(keys, function(key) {
    parsed <- .parse_agent_state_key(key)
    if (!identical(parsed$agent_id, as.character(agent_id))) return(NULL)
    if (!.portfolio_agent_asset_allowed(exchange, parsed$agent_id, parsed$asset_id)) return(NULL)
    symbol <- exchange$asset_symbols[[as.character(parsed$asset_id)]] %||% parsed$symbol
    .state_to_snapshot(
      .sync_state_cash_from_account(exchange, agent_id, exchange$agent_states[[key]]),
      timestamp,
      agent_id = parsed$agent_id,
      symbol = symbol,
      asset_id = parsed$asset_id
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

#' @keywords internal
.enforce_cross_margin <- function(exchange, agent_id, timestamp) {
  snapshots <- .agent_position_snapshots(exchange, agent_id, timestamp)
  if (nrow(snapshots) == 0L) return(invisible(FALSE))
  account <- .aggregate_account_snapshots(snapshots, latest = TRUE)
  if (nrow(account) == 0L) return(invisible(FALSE))
  equity <- as.numeric(account$equity[1L] %||% NA_real_)
  maintenance_margin <- if (isTRUE(exchange$config$portfolio_margin %||% FALSE)) {
    .portfolio_margin_required(exchange, agent_id, snapshots)
  } else {
    as.numeric(account$maintenance_margin[1L] %||% 0)
  }
  if (!is.finite(equity) || equity >= maintenance_margin) return(invisible(FALSE))
  agent_id <- as.character(agent_id)
  exchange$agent_accounts[[agent_id]]$cash <- 0
  exchange$agent_accounts[[agent_id]]$liquidated <- TRUE
  keys <- names(exchange$agent_states)
  for (key in keys) {
    parsed <- .parse_agent_state_key(key)
    if (!identical(parsed$agent_id, agent_id)) next
    state <- exchange$agent_states[[key]]
    state$cash <- 0
    state$pos_dir <- 0L
    state$ctr_unit <- 0
    state$avg_price <- NA_real_
    state$notional <- 0
    state$abs_notional <- 0
    state$unrealized_pnl <- 0
    state$maintenance_margin <- 0
    state$equity <- 0
    state$liquidated <- TRUE
    exchange$agent_states[[key]] <- state
  }
  exchange$event_log <- data.table::rbindlist(list(exchange$event_log, data.table::data.table(
    timestamp = timestamp,
    source = "risk",
    event = "cross_margin_liquidation",
    ref_id = agent_id
  )), fill = TRUE)
  invisible(TRUE)
}

#' @keywords internal
.portfolio_margin_required <- function(exchange, agent_id, snapshots) {
  snapshots <- data.table::as.data.table(snapshots)
  snapshots <- snapshots[abs(as.numeric(notional)) > 0]
  if (nrow(snapshots) == 0L) return(0)
  assets <- sort(unique(as.integer(snapshots$asset_id)))
  cov <- .cross_asset_covariance(exchange, assets)
  exposure <- numeric(length(assets))
  idx <- match(as.integer(snapshots$asset_id), assets)
  exposure[idx] <- as.numeric(snapshots$notional)
  covariance_margin <- as.numeric(exchange$config$portfolio_margin_sigma %||% 3) * sqrt(max(0, as.numeric(t(exposure) %*% cov %*% exposure)))
  floor_rate <- as.numeric(exchange$config$portfolio_margin_floor %||% exchange$config$mmr %||% 0.02)
  floor_margin <- floor_rate * sum(abs(exposure), na.rm = TRUE)
  max(covariance_margin, floor_margin, na.rm = TRUE)
}

#' @keywords internal
.normalize_asset_key <- function(symbol = NULL, asset_id = NULL, exchange = NULL, validate = TRUE) {
  if (is.null(symbol) && is.null(asset_id) && !is.null(exchange) && nrow(exchange$market_events) > 0L) {
    latest <- exchange$market_events[nrow(exchange$market_events)]
    symbol <- latest$symbol[1L] %||% NULL
    asset_id <- latest$asset_id[1L] %||% NULL
  }
  if (is.null(asset_id) && !is.null(symbol) && !is.null(exchange) && isTRUE(validate)) {
    return(.asset_require_registered(exchange, symbol = symbol))
  }
  if (is.null(symbol) && !is.null(asset_id) && !is.null(exchange) && isTRUE(validate)) {
    return(.asset_require_registered(exchange, asset_id = asset_id))
  }
  if (is.null(asset_id) && !is.null(symbol)) {
    asset_id <- .asset_id_from_symbol(symbol)
  }
  if (is.null(symbol) && !is.null(asset_id)) {
    symbol <- paste0("asset-", as.integer(asset_id))
  }
  if (is.null(asset_id)) asset_id <- .asset_id_from_symbol(symbol %||% "default")
  if (is.null(symbol)) symbol <- "default"
  asset <- list(symbol = as.character(symbol), asset_id = as.integer(asset_id))
  if (isTRUE(validate) && !is.null(exchange)) {
    return(.asset_require_registered(exchange, symbol = asset$symbol, asset_id = asset$asset_id))
  }
  asset
}

#' @keywords internal
.asset_id_from_symbol <- function(symbol) {
  symbol <- as.character(symbol %||% "default")
  if (identical(symbol, "default")) return(0L)
  as.integer(sum(utf8ToInt(symbol)) %% .Machine$integer.max)
}

#' @keywords internal
.bar_asset_key <- function(bar) {
  .normalize_asset_key(symbol = bar$symbol[1L] %||% "default", asset_id = bar$asset_id[1L] %||% 0L)
}

#' @keywords internal
.agent_state_key <- function(agent_id, asset_id) {
  paste(as.character(agent_id), as.integer(asset_id), sep = "\r")
}

#' @keywords internal
.parse_agent_state_key <- function(key) {
  parts <- strsplit(key, "\r", fixed = TRUE)[[1L]]
  agent_id <- parts[[1L]]
  asset_id <- if (length(parts) >= 2L) as.integer(parts[[2L]]) else 0L
  list(agent_id = agent_id, asset_id = asset_id, symbol = paste0("asset-", asset_id))
}
