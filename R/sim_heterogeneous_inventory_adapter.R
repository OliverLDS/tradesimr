# Heterogeneous inventory execution is deliberately an exchange-private adapter.
# It converts durable explicit orders to the normalized C++ batch contract and
# is the sole place that projects proposed C++ state back into exchange ledgers.

.heterogeneous_inventory_agents <- function(exchange, bars) {
  asset_ids <- unique(as.integer(bars$asset_id))
  order_agents <- exchange$agent_orders[
    status == "accepted" & qty_type == "contracts" & is.na(rebalance_id) & asset_id %in% asset_ids,
    unique(as.character(agent_id))
  ]
  state_agents <- vapply(names(exchange$spot_states %||% list()), function(key) {
    parsed <- .parse_agent_state_key(key)
    if (parsed$asset_id %in% asset_ids) parsed$agent_id else NA_character_
  }, character(1L))
  unique(c(order_agents, stats::na.omit(state_agents)))
}

.heterogeneous_inventory_orders <- function(exchange, agent_id, bars) {
  boundary <- as.POSIXct(bars$timestamp[1L], tz = "UTC")
  bar_assets <- unique(as.integer(bars$asset_id))
  candidates <- exchange$agent_orders[
    status == "accepted" & qty_type == "contracts" &
      agent_id == as.character(agent_id) & asset_id %in% bar_assets &
      is.na(rebalance_id) & (
        (!is.na(eligible_after) & eligible_after < boundary) |
          (is.na(eligible_after) & timestamp <= boundary)
      )
  ]
  if (!nrow(candidates)) return(candidates)
  candidates <- candidates[vapply(asset_id, function(id) .asset_uses_spot_inventory(exchange, id), logical(1L))]
  if (!nrow(candidates)) return(candidates)
  if (!"atomic_group_id" %in% names(candidates)) candidates[, atomic_group_id := order_id]
  candidates[is.na(atomic_group_id) | !nzchar(atomic_group_id), atomic_group_id := order_id]
  group_ids <- unique(candidates$atomic_group_id)
  complete <- vapply(group_ids, function(group_id) {
    group <- candidates[atomic_group_id == group_id]
    all(unique(as.integer(group$asset_id)) %in% bar_assets)
  }, logical(1L))
  candidates[atomic_group_id %in% group_ids[complete]]
}

.heterogeneous_inventory_account_input <- function(exchange, agent_id, bars) {
  for (i in seq_len(nrow(bars))) {
    asset <- .bar_asset_key(bars[i])
    .ensure_spot_account(exchange, agent_id, asset$asset_id, asset$symbol)
  }
  states <- lapply(names(exchange$spot_states %||% list()), function(key) {
    parsed <- .parse_agent_state_key(key)
    if (!identical(parsed$agent_id, as.character(agent_id))) return(NULL)
    spec <- exchange$assets[asset_id == parsed$asset_id]
    if (nrow(spec) != 1L) return(NULL)
    state <- exchange$spot_states[[key]]
    data.table::data.table(
      asset_id = parsed$asset_id,
      currency = .profile_currency(exchange, state$currency %||% spec$quote_ccy[1L]),
      units = as.numeric(state$units %||% 0),
      average_cost = as.numeric(state$avg_cost %||% NA_real_),
      last_price = as.numeric(state$last_price %||% NA_real_),
      contract_size = as.numeric(spec$contract_size[1L] %||% 1)
    )
  })
  inventory <- data.table::rbindlist(states, fill = TRUE)
  if (!nrow(inventory)) inventory <- data.table::data.table(
    asset_id = integer(), currency = character(), units = numeric(),
    average_cost = numeric(), last_price = numeric(), contract_size = numeric()
  )
  balances <- sim_exchange_cash_balances(exchange, agent_id)
  currencies <- unique(c(.profile_base_currency(exchange), balances$currency, inventory$currency))
  list(
    cash_balances = data.frame(currency = balances$currency, settled = balances$amount, unsettled = 0),
    inventory_positions = data.frame(inventory),
    margin_positions = data.frame(asset_id = integer(), currency = character(), signed_units = numeric(),
      settlement_price = numeric(), last_price = numeric(), contract_size = numeric(), maintenance_rate = numeric()),
    fx_rates = data.frame(currency = currencies, rate_to_base = vapply(currencies, function(currency) {
      .profile_fx_rate(exchange, currency, .profile_base_currency(exchange))
    }, numeric(1L)))
  )
}

.heterogeneous_inventory_normalize_orders <- function(exchange, orders, bars) {
  if (!nrow(orders)) return(sim_heterogeneous_order_batch_schema())
  bars <- data.table::as.data.table(bars)
  specs <- exchange$assets[, .(asset_id, instrument_profile, contract_size, qty_step, quote_ccy)]
  out <- merge(data.table::copy(orders), specs, by = "asset_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, bars[, .(asset_id, open)], by = "asset_id", all.x = TRUE, sort = FALSE)
  if (anyNA(out$instrument_profile) || any(!is.finite(out$open))) {
    stop("Every heterogeneous inventory order requires a registered asset and current open price.", call. = FALSE)
  }
  out[, `:=`(
    execution_price = ifelse(order_type == "limit", limit_price, open),
    fee_rt = as.numeric(exchange$config$fee_rt %||% 0),
    target_derived = !is.na(rebalance_id),
    atomic_group_id = as.character(atomic_group_id %||% order_id)
  )]
  out[is.na(eligible_after), eligible_after := bars$timestamp[1L] - 1e-6]
  # Target weights are decisions, not explicit contract guarantees. At the
  # next eligible open, clip a long-only inventory target to settled cash so a
  # small gap or fee cannot turn a near-100% allocation into a rejected group.
  target_buys <- which(out$target_derived & out$side == "buy")
  for (i in target_buys) {
    available <- .profile_cash_balance(exchange, out$agent_id[i], out$quote_ccy[i])
    unit_cost <- out$execution_price[i] * out$contract_size[i] * (1 + out$fee_rt[i])
    max_qty <- floor((available / unit_cost) / out$qty_step[i] + 1e-10) * out$qty_step[i]
    if (is.finite(max_qty) && max_qty < out$qty[i]) out$qty[i] <- max(0, max_qty)
  }
  .normalize_heterogeneous_orders(out, bars$timestamp[1L])
}

.heterogeneous_inventory_commit_state <- function(exchange, agent_id, proposed, bars) {
  for (i in seq_len(nrow(proposed$cash_balances))) {
    row <- proposed$cash_balances[i, ]
    .profile_set_cash_balance(exchange, agent_id, row$currency, row$settled)
  }
  for (i in seq_len(nrow(proposed$inventory_positions))) {
    row <- proposed$inventory_positions[i, ]
    spec <- exchange$assets[asset_id == as.integer(row$asset_id)]
    if (nrow(spec) != 1L) next
    .ensure_spot_account(exchange, agent_id, row$asset_id, spec$symbol[1L])
    key <- .agent_state_key(agent_id, row$asset_id)
    state <- exchange$spot_states[[key]]
    state$units <- as.numeric(row$units)
    state$avg_cost <- as.numeric(row$average_cost)
    state$last_price <- as.numeric(row$last_price)
    state$currency <- .profile_currency(exchange, row$currency)
    exchange$spot_states[[key]] <- state
  }
  invisible(NULL)
}

.heterogeneous_inventory_message <- function(status, reason) {
  if (identical(status, "cancelled") && identical(reason, "limit_not_eligible")) return("Limit order expired without an eligible bar.")
  if (identical(reason, "atomic_group_rejected")) return("Atomic order group rejected; no leg was committed.")
  if (identical(reason, "insufficient_cash")) return("Order could not be funded by settled cash.")
  if (identical(reason, "insufficient_inventory")) return("Order exceeds available fully paid inventory.")
  if (identical(status, "no_op")) return("Order requires no inventory change.")
  "Inventory order could not be executed."
}

.heterogeneous_inventory_event <- function(exchange, order, fill, timestamp) {
  asset <- exchange$assets[asset_id == as.integer(order$asset_id[1L])]
  key <- .agent_state_key(order$agent_id[1L], order$asset_id[1L])
  is_inventory <- .asset_uses_spot_inventory(exchange, order$asset_id[1L])
  state <- if (is_inventory) exchange$spot_states[[key]] else exchange$agent_states[[key]]
  signed_quantity <- if (is_inventory) as.numeric(state$units %||% 0) else {
    as.numeric(state$pos_dir %||% 0) * as.numeric(state$ctr_unit %||% 0)
  }
  currency <- if (is_inventory) state$currency else asset$quote_ccy[1L]
  event_id <- .next_spot_event_id(exchange)
  data.table::data.table(
    timestamp = timestamp, event_id = event_id, event_type = 1L,
    event_type_label = "fill", action_id = event_id, status_label = "filled",
    action_label = as.character(order$side[1L]),
    dir_label = if (signed_quantity > 0) "long" else if (signed_quantity < 0) "short" else "flat",
    ctr_qty = as.numeric(fill$qty[1L]), price = as.numeric(fill$price[1L]),
    cash = .profile_cash_balance(exchange, order$agent_id[1L], currency),
    equity = .profile_agent_equity(exchange, order$agent_id[1L]),
    fee = as.numeric(fill$fee[1L]), realized_pnl = as.numeric(fill$realized_pnl[1L]),
    agent_id = as.character(order$agent_id[1L]), symbol = as.character(asset$symbol[1L]),
    asset_id = as.integer(order$asset_id[1L]), order_id = as.character(order$order_id[1L])
  )
}

.heterogeneous_inventory_apply_fill <- function(exchange, order, fill, timestamp) {
  order_id <- as.character(order$order_id[1L])
  spec <- exchange$assets[asset_id == as.integer(order$asset_id[1L])]
  key <- .agent_state_key(order$agent_id[1L], order$asset_id[1L])
  is_inventory <- .asset_uses_spot_inventory(exchange, order$asset_id[1L])
  state <- if (is_inventory) exchange$spot_states[[key]] else exchange$agent_states[[key]]
  currency <- .profile_currency(exchange, if (is_inventory) state$currency %||% spec$quote_ccy[1L] else spec$quote_ccy[1L])
  if (!is_inventory) {
    # Margin products debit fees only at entry/exit. Variation margin is
    # emitted separately by the C++ heterogeneous account step.
    .profile_record_cash(exchange, timestamp, order$agent_id[1L], currency, -as.numeric(fill$fee[1L]),
      .profile_cash_balance(exchange, order$agent_id[1L], currency), "margin_trade_fee", order$asset_id[1L],
      order$symbol[1L], order_id, message = "Margin order filled through heterogeneous execution.")
    event <- .heterogeneous_inventory_event(exchange, order, fill, timestamp)
    .spot_mark_order_terminal(exchange, order_id, "filled", "filled", "Margin order filled.", timestamp,
      price = fill$price[1L], fee = fill$fee[1L], realized_pnl = fill$realized_pnl[1L])
    .append_portfolio_fill(exchange, order, event)
    return(event)
  }
  is_sale <- identical(as.character(order$side[1L]), "sell")
  lag_days <- as.integer(spec$settlement_lag_days[1L] %||% 0L)
  cash_amount <- if (is_sale) {
    as.numeric(fill$qty[1L]) * as.numeric(fill$price[1L]) * as.numeric(spec$contract_size[1L]) - as.numeric(fill$fee[1L])
  } else {
    -(as.numeric(fill$qty[1L]) * as.numeric(fill$price[1L]) * as.numeric(spec$contract_size[1L]) + as.numeric(fill$fee[1L]))
  }
  if (is_sale && lag_days > 0L) {
    balance <- .profile_cash_balance(exchange, order$agent_id[1L], currency) - cash_amount
    .profile_set_cash_balance(exchange, order$agent_id[1L], currency, balance)
    state$unsettled_cash <- as.numeric(state$unsettled_cash %||% 0) + cash_amount
    exchange$spot_states[[key]] <- state
    .profile_record_settlement(exchange, timestamp, order$agent_id[1L], currency, cash_amount,
      order$asset_id[1L], order$symbol[1L], order_id, lag_days, "Sale proceeds pending settlement.")
    cash_amount <- 0
  }
  .profile_record_cash(exchange, timestamp, order$agent_id[1L], currency, cash_amount,
    .profile_cash_balance(exchange, order$agent_id[1L], currency), "spot_trade", order$asset_id[1L],
    order$symbol[1L], order_id, message = "Inventory order filled through heterogeneous execution.")
  event <- .heterogeneous_inventory_event(exchange, order, fill, timestamp)
  .spot_mark_order_terminal(exchange, order_id, "filled", "filled", "Inventory order filled.", timestamp,
    price = fill$price[1L], fee = fill$fee[1L], realized_pnl = fill$realized_pnl[1L])
  .append_portfolio_fill(exchange, order, event)
  event
}

.heterogeneous_inventory_apply_outcomes <- function(exchange, orders, proposed, timestamp) {
  events <- list()
  fills <- data.table::as.data.table(proposed$fills)
  for (i in seq_len(nrow(fills))) {
    fill <- fills[i]
    order <- orders[order_id == fill$order_id]
    if (nrow(order) != 1L) next
    status <- as.character(fill$status[1L])
    reason <- as.character(fill$reason_code[1L])
    if (identical(status, "filled")) {
      events[[length(events) + 1L]] <- .heterogeneous_inventory_apply_fill(exchange, order, fill, timestamp)
    } else if (identical(status, "pending")) {
      next
    } else {
      .spot_mark_order_terminal(exchange, order$order_id[1L], status, reason,
        .heterogeneous_inventory_message(status, reason), timestamp,
        price = fill$price[1L], fee = fill$fee[1L], realized_pnl = fill$realized_pnl[1L])
    }
  }
  data.table::rbindlist(events, fill = TRUE)
}

.sim_exchange_step_heterogeneous_inventory <- function(exchange, bars) {
  bars <- data.table::as.data.table(bars)
  timestamps <- unique(as.POSIXct(bars$timestamp, tz = "UTC"))
  events <- list()
  for (timestamp_index in seq_along(timestamps)) {
    boundary_timestamp <- as.POSIXct(timestamps[[timestamp_index]], origin = "1970-01-01", tz = "UTC")
    boundary_bars <- bars[timestamp == boundary_timestamp]
    inventory_bars <- boundary_bars[vapply(asset_id, function(id) .asset_uses_spot_inventory(exchange, id), logical(1L))]
    if (!nrow(inventory_bars)) next
    .profile_settle_due(exchange, boundary_timestamp)
    for (i in seq_len(nrow(inventory_bars))) .profile_apply_corporate_actions(exchange, boundary_timestamp, inventory_bars$asset_id[i])
    for (agent_id in .heterogeneous_inventory_agents(exchange, inventory_bars)) {
      input <- .heterogeneous_inventory_account_input(exchange, agent_id, inventory_bars)
      accepted <- .heterogeneous_inventory_orders(exchange, agent_id, inventory_bars)
      normalized <- .heterogeneous_inventory_normalize_orders(exchange, accepted, inventory_bars)
      proposed <- heterogeneous_order_preflight_rcpp(
        .profile_base_currency(exchange), input$cash_balances, input$inventory_positions,
        input$margin_positions, data.frame(inventory_bars[, .(asset_id, open, high, low, close)]),
        input$fx_rates, data.frame(normalized), as.numeric(boundary_timestamp)
      )
      proposed$cash_balances <- data.table::as.data.table(proposed$cash_balances)
      proposed$inventory_positions <- data.table::as.data.table(proposed$inventory_positions)
      proposed$fills <- data.table::as.data.table(proposed$fills)
      proposed$groups <- data.table::as.data.table(proposed$groups)
      # The C++ proposal rolls rejected groups back internally. Committing its
      # state therefore applies valuation marks and committed groups only.
      .heterogeneous_inventory_commit_state(exchange, agent_id, proposed, inventory_bars)
      if (nrow(accepted)) {
        adapter_events <- .heterogeneous_inventory_apply_outcomes(exchange, accepted, proposed, boundary_timestamp)
        if (nrow(adapter_events)) events[[length(events) + 1L]] <- adapter_events
      }
    }
  }
  data.table::rbindlist(events, fill = TRUE)
}

# The target-weight API may combine fully paid inventory with margin products.
# Keep this path separate from the explicit-inventory adapter above: a target
# rebalance is one atomic group and must cross the C++ boundary as one account.
.heterogeneous_portfolio_agents <- function(exchange, bars) {
  asset_ids <- unique(as.integer(bars$asset_id))
  order_agents <- exchange$agent_orders[
    status == "accepted" & qty_type == "contracts" & asset_id %in% asset_ids,
    unique(as.character(agent_id))
  ]
  state_agents <- c(
    vapply(names(exchange$spot_states %||% list()), function(key) .parse_agent_state_key(key)$agent_id, character(1L)),
    vapply(names(exchange$agent_states %||% list()), function(key) .parse_agent_state_key(key)$agent_id, character(1L))
  )
  unique(c(order_agents, state_agents))
}

.heterogeneous_portfolio_orders <- function(exchange, agent_id, bars) {
  boundary <- as.POSIXct(bars$timestamp[1L], tz = "UTC")
  bar_assets <- unique(as.integer(bars$asset_id))
  orders <- exchange$agent_orders[
    status == "accepted" & qty_type == "contracts" & agent_id == as.character(agent_id) &
      asset_id %in% bar_assets & (
        (!is.na(eligible_after) & eligible_after < boundary) |
          (is.na(eligible_after) & !target_derived & timestamp <= boundary)
      )
  ]
  if (!nrow(orders)) return(orders)
  if (!"atomic_group_id" %in% names(orders)) orders[, atomic_group_id := order_id]
  orders[is.na(atomic_group_id) | !nzchar(atomic_group_id), atomic_group_id := order_id]
  group_ids <- unique(orders$atomic_group_id)
  complete <- vapply(group_ids, function(group_id) {
    all(unique(as.integer(orders[atomic_group_id == group_id, asset_id])) %in% bar_assets)
  }, logical(1L))
  orders[atomic_group_id %in% group_ids[complete]]
}

.heterogeneous_portfolio_account_input <- function(exchange, agent_id, bars) {
  for (i in seq_len(nrow(bars))) {
    asset <- .bar_asset_key(bars[i])
    if (.asset_uses_spot_inventory(exchange, asset$asset_id)) {
      .ensure_spot_account(exchange, agent_id, asset$asset_id, asset$symbol)
    } else {
      .ensure_agent_account(exchange, agent_id, asset$asset_id, asset$symbol)
    }
  }
  inventory <- data.table::rbindlist(lapply(names(exchange$spot_states %||% list()), function(key) {
    parsed <- .parse_agent_state_key(key)
    if (!identical(parsed$agent_id, as.character(agent_id))) return(NULL)
    spec <- exchange$assets[asset_id == parsed$asset_id]
    if (nrow(spec) != 1L) return(NULL)
    state <- exchange$spot_states[[key]]
    data.table::data.table(
      asset_id = parsed$asset_id, currency = .profile_currency(exchange, state$currency %||% spec$quote_ccy[1L]),
      units = as.numeric(state$units %||% 0), average_cost = as.numeric(state$avg_cost %||% NA_real_),
      last_price = as.numeric(state$last_price %||% NA_real_), contract_size = as.numeric(spec$contract_size[1L])
    )
  }), fill = TRUE)
  if (!ncol(inventory)) inventory <- data.table::data.table(
    asset_id = integer(), currency = character(), units = numeric(), average_cost = numeric(),
    last_price = numeric(), contract_size = numeric()
  )
  margin <- data.table::rbindlist(lapply(names(exchange$agent_states %||% list()), function(key) {
    parsed <- .parse_agent_state_key(key)
    if (!identical(parsed$agent_id, as.character(agent_id))) return(NULL)
    spec <- exchange$assets[asset_id == parsed$asset_id]
    if (nrow(spec) != 1L || .asset_uses_spot_inventory(exchange, parsed$asset_id)) return(NULL)
    state <- exchange$agent_states[[key]]
    data.table::data.table(
      asset_id = parsed$asset_id, currency = .profile_currency(exchange, spec$quote_ccy[1L]),
      signed_units = as.numeric(state$pos_dir %||% 0) * as.numeric(state$ctr_unit %||% 0),
      settlement_price = as.numeric(state$settlement_price %||% state$avg_price %||% state$last_px %||% NA_real_),
      last_price = as.numeric(state$last_px %||% NA_real_), contract_size = as.numeric(spec$contract_size[1L]),
      maintenance_rate = as.numeric(exchange$config$mmr %||% 0.02)
    )
  }), fill = TRUE)
  if (!ncol(margin)) margin <- data.table::data.table(
    asset_id = integer(), currency = character(), signed_units = numeric(), settlement_price = numeric(),
    last_price = numeric(), contract_size = numeric(), maintenance_rate = numeric()
  )
  balances <- sim_exchange_cash_balances(exchange, agent_id)
  currencies <- unique(c(.profile_base_currency(exchange), balances$currency, inventory$currency, margin$currency))
  list(
    cash_balances = data.frame(currency = balances$currency, settled = balances$amount, unsettled = 0),
    inventory_positions = data.frame(inventory), margin_positions = data.frame(margin),
    fx_rates = data.frame(currency = currencies, rate_to_base = vapply(currencies, function(currency) {
      .profile_fx_rate(exchange, currency, .profile_base_currency(exchange))
    }, numeric(1L)))
  )
}

.heterogeneous_portfolio_normalize_orders <- function(exchange, orders, bars) {
  if (!nrow(orders)) return(sim_heterogeneous_order_batch_schema())
  bars <- data.table::as.data.table(bars)
  specs <- exchange$assets[, .(asset_id, instrument_profile, contract_size, qty_step, quote_ccy)]
  out <- merge(data.table::copy(orders), specs, by = "asset_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, bars[, .(asset_id, open)], by = "asset_id", all.x = TRUE, sort = FALSE)
  if (anyNA(out$instrument_profile) || any(!is.finite(out$open))) {
    stop("Every heterogeneous portfolio order requires a registered asset and current open price.", call. = FALSE)
  }
  out[, `:=`(
    execution_price = ifelse(order_type == "limit", limit_price, open),
    fee_rt = as.numeric(exchange$config$fee_rt %||% 0),
    target_derived = !is.na(rebalance_id),
    atomic_group_id = as.character(atomic_group_id %||% order_id)
  )]
  out[is.na(eligible_after), eligible_after := bars$timestamp[1L] - 1e-6]
  # Execute inventory reductions before purchases within a target rebalance.
  # That is still one atomic C++ group, but lets a sell fund a simultaneous
  # fully-paid buy without fabricating a margin loan.
  out[, .execution_priority := data.table::fifelse(
    target_derived & instrument_profile %in% c("equity", "etf", "crypto_spot", "fx_spot", "bond") & side %in% c("sell", "flat"),
    0L, 1L
  )]
  data.table::setorderv(out, c("atomic_group_id", ".execution_priority", "order_id"))
  out[, .execution_priority := NULL]
  .normalize_heterogeneous_orders(out, bars$timestamp[1L])
}

.heterogeneous_portfolio_commit_state <- function(exchange, agent_id, proposed) {
  .heterogeneous_inventory_commit_state(exchange, agent_id, proposed, data.table::data.table())
  for (asset_id in as.integer(proposed$inventory_positions$asset_id)) {
    exchange$agent_states[[.agent_state_key(agent_id, asset_id)]] <- NULL
  }
  margin <- data.table::as.data.table(proposed$margin_positions)
  for (i in seq_len(nrow(margin))) {
    row <- margin[i]
    spec <- exchange$assets[asset_id == as.integer(row$asset_id)]
    if (nrow(spec) != 1L) next
    .ensure_agent_account(exchange, agent_id, row$asset_id, spec$symbol[1L])
    key <- .agent_state_key(agent_id, row$asset_id)
    state <- exchange$agent_states[[key]]
    signed <- as.numeric(row$signed_units)
    state$pos_dir <- if (signed > 0) 1L else if (signed < 0) -1L else 0L
    state$ctr_unit <- abs(signed)
    state$settlement_price <- as.numeric(row$settlement_price)
    state$avg_price <- as.numeric(row$settlement_price)
    state$last_px <- as.numeric(row$last_price)
    state$notional <- signed * state$last_px * as.numeric(row$contract_size)
    state$abs_notional <- abs(state$notional)
    state$unrealized_pnl <- 0
    state$cash <- .shared_cash(exchange, agent_id)
    exchange$agent_states[[key]] <- state
  }
  account <- exchange$agent_accounts[[as.character(agent_id)]]
  if (!is.null(account)) account$liquidated <- isTRUE(proposed$liquidated)
  exchange$agent_accounts[[as.character(agent_id)]] <- account
  invisible(NULL)
}

.heterogeneous_portfolio_apply_outcomes <- function(exchange, orders, proposed, timestamp) {
  events <- list()
  fills <- data.table::as.data.table(proposed$fills)
  for (i in seq_len(nrow(fills))) {
    fill <- fills[i]
    order <- orders[order_id == fill$order_id]
    if (nrow(order) != 1L || identical(as.character(fill$status), "pending")) next
    if (identical(as.character(fill$status), "filled")) {
      events[[length(events) + 1L]] <- .heterogeneous_inventory_apply_fill(exchange, order, fill, timestamp)
    } else {
      .spot_mark_order_terminal(exchange, order$order_id[1L], as.character(fill$status),
        as.character(fill$reason_code), .heterogeneous_inventory_message(as.character(fill$status), as.character(fill$reason_code)),
        timestamp, price = fill$price[1L], fee = fill$fee[1L], realized_pnl = fill$realized_pnl[1L])
    }
  }
  data.table::rbindlist(events, fill = TRUE)
}

.heterogeneous_portfolio_variation_events <- function(exchange, agent_id, proposed, timestamp) {
  variations <- data.table::as.data.table(proposed$events)
  if (!nrow(variations)) return(data.table::data.table())
  first_event_id <- .next_spot_event_id(exchange)
  rows <- lapply(seq_len(nrow(variations)), function(i) {
    row <- variations[i]
    asset <- exchange$assets[asset_id == as.integer(row$asset_id)]
    if (nrow(asset) != 1L) return(NULL)
    balance <- proposed$cash_balances[
      currency == as.character(row$currency), settled
    ]
    .profile_record_cash(exchange, timestamp, agent_id, row$currency, row$amount,
      if (length(balance)) balance[1L] else .profile_cash_balance(exchange, agent_id, row$currency),
      "variation_margin", row$asset_id, asset$symbol[1L],
      message = "Futures variation margin settled through heterogeneous execution.")
    data.table::data.table(
      timestamp = timestamp, event_id = first_event_id + i - 1L, event_type = 4L,
      event_type_label = "variation_margin", action_id = 0L, status_label = "filled",
      action_label = "variation_margin", dir_label = "flat", ctr_qty = 0,
      price = as.numeric(row$settlement_price), cash = .profile_cash_balance(exchange, agent_id, row$currency),
      equity = .profile_agent_equity(exchange, agent_id), fee = 0,
      realized_pnl = as.numeric(row$amount), agent_id = as.character(agent_id),
      symbol = asset$symbol[1L], asset_id = as.integer(row$asset_id), order_id = NA_character_
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

.sim_exchange_step_heterogeneous_portfolio <- function(exchange, bars) {
  bars <- data.table::as.data.table(bars)
  events <- list()
  snapshots <- list()
  boundaries <- unique(as.POSIXct(bars$timestamp, tz = "UTC"))
  for (timestamp_value in boundaries) {
    boundary_timestamp <- as.POSIXct(timestamp_value, origin = "1970-01-01", tz = "UTC")
    boundary_bars <- bars[timestamp == boundary_timestamp]
    .profile_settle_due(exchange, boundary_timestamp)
    for (i in seq_len(nrow(boundary_bars))) .profile_apply_corporate_actions(exchange, boundary_timestamp, boundary_bars$asset_id[i])
    for (agent_id in .heterogeneous_portfolio_agents(exchange, boundary_bars)) {
      input <- .heterogeneous_portfolio_account_input(exchange, agent_id, boundary_bars)
      accepted <- .heterogeneous_portfolio_orders(exchange, agent_id, boundary_bars)
      normalized <- .heterogeneous_portfolio_normalize_orders(exchange, accepted, boundary_bars)
      proposed <- heterogeneous_order_preflight_rcpp(
        .profile_base_currency(exchange), input$cash_balances, input$inventory_positions, input$margin_positions,
        data.frame(merge(boundary_bars[, .(asset_id, open, high, low, close)],
          exchange$assets[, .(asset_id, instrument_profile)], by = "asset_id", all.x = TRUE, sort = FALSE)),
        input$fx_rates, data.frame(normalized), as.numeric(boundary_timestamp)
      )
      proposed$cash_balances <- data.table::as.data.table(proposed$cash_balances)
      proposed$inventory_positions <- data.table::as.data.table(proposed$inventory_positions)
      proposed$margin_positions <- data.table::as.data.table(proposed$margin_positions)
      proposed$fills <- data.table::as.data.table(proposed$fills)
      proposed$events <- data.table::as.data.table(proposed$events)
      .heterogeneous_portfolio_commit_state(exchange, agent_id, proposed)
      outcome_events <- .heterogeneous_portfolio_apply_outcomes(exchange, accepted, proposed, boundary_timestamp)
      variation_events <- .heterogeneous_portfolio_variation_events(exchange, agent_id, proposed, boundary_timestamp)
      if (nrow(variation_events)) {
        data.table::set(variation_events, j = "event_id", value = max(c(
          0L, exchange$step_events$event_id %||% integer(), outcome_events$event_id %||% integer()
        ), na.rm = TRUE) + seq_len(nrow(variation_events)))
      }
      if (nrow(outcome_events)) events[[length(events) + 1L]] <- outcome_events
      if (nrow(variation_events)) events[[length(events) + 1L]] <- variation_events
      .enforce_cross_margin(exchange, agent_id, boundary_timestamp)
      snapshots[[length(snapshots) + 1L]] <- .agent_position_snapshots(exchange, agent_id, boundary_timestamp)
    }
  }
  list(events = data.table::rbindlist(events, fill = TRUE), snapshots = data.table::rbindlist(snapshots, fill = TRUE))
}
