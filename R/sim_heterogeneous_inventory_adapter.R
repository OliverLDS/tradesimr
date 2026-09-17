# Heterogeneous inventory execution is deliberately an exchange-private adapter.
# It converts durable explicit orders to the normalized C++ batch contract and
# is the sole place that projects proposed C++ state back into exchange ledgers.

#' @keywords internal
.heterogeneous_derivatives_account_step <- function(exchange,
                                                    agent_id,
                                                    margin_positions,
                                                    bars,
                                                    orders,
                                                    covariance) {
  bars <- data.table::as.data.table(bars)
  specs <- exchange$assets[match(as.integer(bars$asset_id), exchange$assets$asset_id)]
  if (anyNA(specs$asset_id)) stop("Missing registered asset specification for derivative portfolio step.", call. = FALSE)
  orders <- .normalize_portfolio_step_orders(orders)
  action_to_side <- function(action, dir) {
    if (action == -1L) return("flat")
    if (action == -2L) return(if (dir > 0L) "sell" else "buy")
    if (dir > 0L) "buy" else "sell"
  }
  normalized <- data.table::as.data.table(orders)
  if (nrow(normalized)) {
    normalized[, `:=`(
      instrument_profile = "future",
      side = vapply(seq_len(.N), function(i) action_to_side(action[i], dir[i]), character(1L)),
      qty = as.numeric(ctr_qty), execution_price = as.numeric(price),
      fee_rt = as.numeric(exchange$config$fee_rt %||% 0),
      eligible_after = as.POSIXct(as.numeric(bars$timestamp[1L]) - 1e-6, origin = "1970-01-01", tz = "UTC"),
      atomic_group_id = ifelse(is.na(order_id) | !nzchar(order_id), paste0("derivative-", seq_len(.N)), order_id),
      target_derived = as.logical(fee_aware_target), time_in_force = "next_eligible_bar",
      action_code = as.integer(action), dir_code = as.integer(dir), order_type_code = as.integer(order_type),
      ctr_step = as.numeric(specs$qty_step[match(asset_id, bars$asset_id)])
    )]
  } else {
    normalized <- sim_heterogeneous_order_batch_schema()
  }
  covariance_long <- data.table::as.data.table(as.data.frame(as.table(covariance)))
  data.table::setnames(covariance_long, c("asset_i_index", "asset_j_index", "covariance"))
  covariance_long[, `:=`(
    asset_i = as.integer(bars$asset_id)[as.integer(asset_i_index)],
    asset_j = as.integer(bars$asset_id)[as.integer(asset_j_index)]
  )]
  old_timestamp <- if (nrow(margin_positions)) as.numeric(margin_positions$old_timestamp[1L] %||% NA_real_) else NA_real_
  settings <- data.frame(
    execution_mode = "derivatives_native", shared_cash = .shared_cash(exchange, agent_id),
    lev = as.numeric(exchange$config$lev %||% 10), fee_rt = as.numeric(exchange$config$fee_rt %||% 0),
    maker_fee_rt = as.numeric(exchange$config$maker_fee_rt %||% NA_real_),
    taker_fee_rt = as.numeric(exchange$config$taker_fee_rt %||% NA_real_),
    fund_rt = as.numeric(exchange$config$fund_rt %||% 0),
    funding_interval_hours = as.numeric(exchange$config$funding_interval_hours %||% 8),
    mmr = as.numeric(exchange$config$mmr %||% 0.02),
    portfolio_margin_sigma = as.numeric(exchange$config$portfolio_margin_sigma %||% 3),
    portfolio_margin_floor = as.numeric(exchange$config$portfolio_margin_floor %||% exchange$config$mmr %||% 0.02),
    old_timestamp = old_timestamp, slippage = as.numeric(exchange$config$slippage %||% 0),
    spread = as.numeric(exchange$config$spread %||% 0), rec = TRUE,
    # Generic legacy assets use derivative position accounting for backwards
    # compatibility, but they are not exchange-traded futures.  Restrict daily
    # cash variation settlement to the explicit futures profile.
    settle_variation_margin = all(as.character(specs$instrument_profile) == "future")
  )
  heterogeneous_account_step_rcpp(
    .profile_base_currency(exchange),
    data.frame(currency = .profile_base_currency(exchange), settled = .shared_cash(exchange, agent_id), unsettled = 0),
    data.frame(asset_id = integer(), currency = character(), units = numeric(), average_cost = numeric(), last_price = numeric(), contract_size = numeric()),
    data.frame(margin_positions),
    data.frame(asset_id = as.integer(bars$asset_id), timestamp = as.numeric(bars$timestamp), open = as.numeric(bars$open), high = as.numeric(bars$high), low = as.numeric(bars$low), close = as.numeric(bars$close), instrument_profile = "future", ctr_step = as.numeric(specs$qty_step)),
    data.frame(currency = .profile_base_currency(exchange), rate_to_base = 1), settings,
    data.frame(covariance_long[, .(asset_i, asset_j, covariance)]), data.frame(normalized),
    as.numeric(bars$timestamp[1L])
  )
}

#' @keywords internal
.heterogeneous_derivative_account_input <- function(exchange, agent_id, bars) {
  asset_ids <- as.integer(bars$asset_id)
  requested_agent_id <- as.character(agent_id)
  # v2 typed margin rows are authoritative. `margin_positions` remains a
  # compatibility projection for callers that still consume sim_state.
  authoritative <- exchange$typed_margin_positions %||% data.table::data.table()
  existing <- authoritative[agent_id == requested_agent_id & asset_id %in% asset_ids]
  if (!nrow(existing) && !.exchange_uses_heterogeneous_v2(exchange)) {
    existing <- exchange$margin_positions[agent_id == requested_agent_id & asset_id %in% asset_ids]
  }
  if (!"old_timestamp" %in% names(existing)) {
    # Typed rows carry their last committed boundary as `timestamp`; that is
    # the authoritative funding interval cursor in v2.
    existing[, old_timestamp := if ("timestamp" %in% names(existing)) as.numeric(timestamp) else NA_real_]
  }
  missing_asset_ids <- setdiff(asset_ids, as.integer(existing$asset_id))
  if (!length(missing_asset_ids)) {
    return(data.table::copy(existing[, .(asset_id, currency, signed_units, settlement_price, last_price, contract_size, maintenance_rate, old_timestamp)]))
  }
  # Backward-compatible seed for exchanges saved before margin_position_state.
  rows <- lapply(missing_asset_ids, function(requested_asset_id) {
    asset <- .bar_asset_key(bars[asset_id == requested_asset_id][1L])
    .ensure_agent_account(exchange, agent_id, asset$asset_id, asset$symbol)
    state <- exchange$agent_states[[.agent_state_key(agent_id, asset$asset_id)]]
    spec <- exchange$assets[asset_id == requested_asset_id]
    quote_ccy <- as.character(spec$quote_ccy[1L])
    if (is.na(quote_ccy) || !nzchar(quote_ccy)) quote_ccy <- .profile_base_currency(exchange)
    bar_mark <- as.numeric(bars[asset_id == requested_asset_id, close][1L])
    signed_units <- as.numeric(state$pos_dir %||% 0) * as.numeric(state$ctr_unit %||% 0)
    mark <- if (abs(signed_units) <= 1e-12) bar_mark else as.numeric(state$last_px %||% bar_mark)
    if (!is.finite(mark)) mark <- as.numeric(bars[asset_id == requested_asset_id, close][1L])
    reference <- if (abs(signed_units) <= 1e-12) mark else as.numeric(state$settlement_price %||% state$avg_price %||% mark)
    if (!is.finite(reference)) reference <- mark
    data.table::data.table(
      asset_id = asset$asset_id, currency = .profile_currency(exchange, quote_ccy),
      signed_units = signed_units,
      settlement_price = reference,
      last_price = mark,
      contract_size = as.numeric(spec$contract_size[1L]), maintenance_rate = as.numeric(exchange$config$mmr %||% 0.02),
      old_timestamp = as.numeric(state$old_timestamp %||% NA_real_)
    )
  })
  data.table::rbindlist(list(
    existing[, .(asset_id, currency, signed_units, settlement_price, last_price, contract_size, maintenance_rate, old_timestamp)],
    data.table::rbindlist(rows)
  ), fill = TRUE)
}

#' @keywords internal
.heterogeneous_derivative_commit_state <- function(exchange, agent_id, proposed, timestamp) {
  margin <- data.table::as.data.table(proposed$margin_positions)
  if (!nrow(margin)) return(invisible(NULL))
  requested_agent_id <- as.character(agent_id)
  margin[, `:=`(agent_id = requested_agent_id, old_timestamp = as.numeric(timestamp))]
  # A complete portfolio boundary updates the same account rows repeatedly.
  # Replace matching rows in place rather than rebuilding the full durable
  # margin table once per agent and boundary.
  existing_keys <- paste(exchange$margin_positions$agent_id, exchange$margin_positions$asset_id, sep = "\r")
  proposed_keys <- paste(margin$agent_id, margin$asset_id, sep = "\r")
  existing_index <- match(proposed_keys, existing_keys)
  matched <- !is.na(existing_index)
  update_columns <- intersect(names(margin), names(exchange$margin_positions))
  if (any(matched)) {
    for (column in update_columns) {
      data.table::set(
        exchange$margin_positions,
        i = existing_index[matched],
        j = column,
        value = margin[[column]][matched]
      )
    }
  }
  if (any(!matched)) {
    exchange$margin_positions <- data.table::rbindlist(
      list(exchange$margin_positions, margin[!matched]),
      fill = TRUE
    )
  }
  balances <- data.table::as.data.table(proposed$cash_balances)
  for (i in seq_len(nrow(balances))) .profile_set_cash_balance(exchange, agent_id, balances$currency[i], balances$settled[i])
  for (i in seq_len(nrow(margin))) {
    row <- margin[i]
    asset_index <- match(as.integer(row$asset_id), exchange$assets$asset_id)
    if (is.na(asset_index)) stop("Missing registered asset specification for derivative state commit.", call. = FALSE)
    .ensure_agent_account(exchange, agent_id, row$asset_id, exchange$assets$symbol[asset_index])
    compatibility_state <- sim_state(
      cash = .shared_cash(exchange, agent_id), pos_dir = sign(row$signed_units), ctr_unit = abs(row$signed_units),
      avg_price = row$settlement_price, last_px = row$last_price, asset = row$asset_id,
      old_timestamp = as.numeric(timestamp)
    )
    # Generic margin products retain floating P&L between fills. Futures have
    # their reference price reset by authoritative variation settlement, so the
    # same calculation correctly becomes zero after settlement.
    signed_units <- as.numeric(row$signed_units %||% 0)
    contract_size <- as.numeric(row$contract_size %||% 1)
    last_price <- as.numeric(row$last_price %||% 0)
    settlement_price <- as.numeric(row$settlement_price %||% NA_real_)
    if (!is.finite(last_price)) last_price <- 0
    if (!is.finite(settlement_price)) settlement_price <- last_price
    compatibility_state$unrealized_pnl <- signed_units *
      (last_price - settlement_price) *
      contract_size
    compatibility_state$notional <- signed_units * last_price * contract_size
    compatibility_state$abs_notional <- abs(compatibility_state$notional)
    compatibility_state$maintenance_margin <- abs(compatibility_state$notional) *
      as.numeric(row$maintenance_rate %||% exchange$config$mmr %||% 0.02)
    compatibility_state$equity <- as.numeric(compatibility_state$cash) + compatibility_state$unrealized_pnl
    exchange$agent_states[[.agent_state_key(agent_id, row$asset_id)]] <- compatibility_state
  }
  exchange$agent_accounts[[as.character(agent_id)]]$liquidated <- isTRUE(proposed$liquidated)
  if (.exchange_uses_heterogeneous_v2(exchange)) {
    # Derivatives-only portfolio replay uses the typed C++ kernel directly.
    # Persist its position/cash projection even though it retains the legacy
    # event projection for downstream portfolio-ledger compatibility.
    typed_proposed <- proposed
    # Account events are projected once by
    # `.heterogeneous_portfolio_variation_events()`, which also produces the
    # matching public step event and cash-ledger row.
    typed_proposed$account_events <- sim_schemas()$account_events[0]
    typed_proposed$events <- data.table::data.table()
    .heterogeneous_v2_record_state(exchange, agent_id, typed_proposed, timestamp)
  }
  invisible(NULL)
}

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
      contract_size = as.numeric(spec$contract_size[1L] %||% 1),
      accrued_interest = as.numeric(state$accrued_interest %||% 0)
    )
  })
  inventory <- data.table::rbindlist(states, fill = TRUE)
  if (!nrow(inventory)) inventory <- data.table::data.table(
    asset_id = integer(), currency = character(), units = numeric(),
    average_cost = numeric(), last_price = numeric(), contract_size = numeric(), accrued_interest = numeric()
  )
  balances <- sim_exchange_cash_balances(exchange, agent_id)
  currencies <- unique(c(.profile_base_currency(exchange), balances$currency, inventory$currency))
  list(
    cash_balances = .profile_cash_kernel_input(exchange, agent_id),
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
    # Targets above 100% are intentional leveraged/allocation requests. Keep
    # them atomic so an infeasible inventory leg rejects the group rather than
    # silently resizing the user's explicit over-allocation request.
    if (is.finite(out$target_weight[i]) && out$target_weight[i] > 1 + 1e-12) next
    available <- .profile_cash_balance(exchange, out$agent_id[i], out$quote_ccy[i])
    unit_cost <- out$execution_price[i] * out$contract_size[i] * (1 + out$fee_rt[i])
    max_qty <- floor((available / unit_cost) / out$qty_step[i] + 1e-10) * out$qty_step[i]
    if (is.finite(max_qty) && max_qty < out$qty[i]) out$qty[i] <- max(0, max_qty)
  }
  .normalize_heterogeneous_orders(out, bars$timestamp[1L])
}

.heterogeneous_inventory_commit_state <- function(exchange, agent_id, proposed, bars,
                                                   record_typed_state = TRUE) {
  for (i in seq_len(nrow(proposed$cash_balances))) {
    row <- proposed$cash_balances[i, ]
    .profile_set_cash_balance(exchange, agent_id, row$currency, row$settled)
    if (!.exchange_uses_heterogeneous_v2(exchange)) {
      .profile_typed_cash_upsert(exchange, agent_id, row$currency,
        unsettled = as.numeric(row$unsettled %||% 0),
        timestamp = bars$timestamp[1L] %||% Sys.time())
    }
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
    state$accrued_interest <- as.numeric(row$accrued_interest %||% 0)
    state$currency <- .profile_currency(exchange, row$currency)
    exchange$spot_states[[key]] <- state
  }
  # Keep the typed v2 ledger authoritative even for an inventory-only boundary.
  # Fill and account events are persisted by the normal adapter outcome path;
  # avoid recording those event rows twice while projecting only state here.
  if (isTRUE(record_typed_state)) {
    typed_state <- proposed
    typed_state$account_events <- sim_schemas()$account_events[0]
    typed_state$events <- sim_schemas()$account_events[0]
    typed_state$fills <- sim_schemas()$portfolio_fills[0]
    .heterogeneous_v2_record_state(exchange, agent_id, typed_state, bars$timestamp[1L] %||% Sys.time())
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

.heterogeneous_inventory_event <- function(exchange, order, fill, timestamp,
                                           force_margin = FALSE) {
  asset <- exchange$assets[asset_id == as.integer(order$asset_id[1L])]
  key <- .agent_state_key(order$agent_id[1L], order$asset_id[1L])
  is_inventory <- !isTRUE(force_margin) && .asset_uses_spot_inventory(exchange, order$asset_id[1L])
  state <- if (is_inventory) exchange$spot_states[[key]] else exchange$agent_states[[key]]
  signed_quantity <- if (is_inventory) as.numeric(state$units %||% 0) else {
    as.numeric(state$pos_dir %||% 0) * as.numeric(state$ctr_unit %||% 0)
  }
  currency <- if (is_inventory) state$currency else asset$quote_ccy[1L]
  planned_action <- as.character(order$intended_action[1L] %||% NA_character_)
  action_label <- if (!is.na(planned_action) && nzchar(planned_action)) planned_action else as.character(order$side[1L])
  event_id <- .next_spot_event_id(exchange)
  data.table::data.table(
    timestamp = timestamp, event_id = event_id, event_type = 1L,
    event_type_label = "fill", action_id = event_id, status_label = "filled",
    # Target-derived fills retain their action-plan lifecycle (`open`,
    # `increase`, `close`, `reduce`) rather than collapsing it to buy/sell.
    # Execution-quality replay uses this durable field to reconstruct the
    # post-fill signed target quantity.
    action_label = action_label,
    dir_label = if (signed_quantity > 0) "long" else if (signed_quantity < 0) "short" else "flat",
    ctr_qty = as.numeric(fill$qty[1L]), price = as.numeric(fill$price[1L]),
    cash = .profile_cash_balance(exchange, order$agent_id[1L], currency),
    equity = .profile_agent_equity(exchange, order$agent_id[1L]),
    fee = as.numeric(fill$fee[1L]), realized_pnl = as.numeric(fill$realized_pnl[1L]),
    agent_id = as.character(order$agent_id[1L]), symbol = as.character(asset$symbol[1L]),
    asset_id = as.integer(order$asset_id[1L]), order_id = as.character(order$order_id[1L])
  )
}

.heterogeneous_inventory_apply_fill <- function(exchange, order, fill, timestamp,
                                                force_margin = FALSE) {
  order_id <- as.character(order$order_id[1L])
  spec <- exchange$assets[asset_id == as.integer(order$asset_id[1L])]
  key <- .agent_state_key(order$agent_id[1L], order$asset_id[1L])
  is_inventory <- !isTRUE(force_margin) && .asset_uses_spot_inventory(exchange, order$asset_id[1L])
  state <- if (is_inventory) exchange$spot_states[[key]] else exchange$agent_states[[key]]
  currency <- .profile_currency(exchange, if (is_inventory) state$currency %||% spec$quote_ccy[1L] else spec$quote_ccy[1L])
  if (!is_inventory) {
    # Margin products debit fees only at entry/exit. Variation margin is
    # emitted separately by the C++ heterogeneous account step.
    .profile_record_cash(exchange, timestamp, order$agent_id[1L], currency, -as.numeric(fill$fee[1L]),
      .profile_cash_balance(exchange, order$agent_id[1L], currency), "margin_trade_fee", order$asset_id[1L],
      order$symbol[1L], order_id, message = "Margin order filled through heterogeneous execution.")
    event <- .heterogeneous_inventory_event(exchange, order, fill, timestamp,
      force_margin = force_margin)
    reason_code <- as.character(fill$reason_code[1L] %||% "filled")
    message <- if (identical(reason_code, "margin_clipped")) {
      "Target-derived order was clipped to available portfolio-margin capacity."
    } else {
      "Margin order filled through heterogeneous execution."
    }
    .spot_mark_order_terminal(exchange, order_id, "filled", reason_code, message, timestamp,
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
  event <- .heterogeneous_inventory_event(exchange, order, fill, timestamp,
    force_margin = force_margin)
  inventory_reason <- as.character(order$reason_code[1L] %||% "filled")
  inventory_message <- if (identical(inventory_reason, "fee_scaled")) {
    "Target-derived group was scaled to reserve execution fees."
  } else {
    "Inventory order filled."
  }
  .spot_mark_order_terminal(exchange, order_id, "filled", inventory_reason, inventory_message, timestamp,
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
  # Typed v2 tables are authoritative. Scanning every compatibility state key
  # for every boundary grows quadratically after a many-agent inventory fill.
  state_agents <- if (.exchange_uses_heterogeneous_v2(exchange)) {
    c(
      as.character((exchange$inventory_positions %||% data.table::data.table())$agent_id),
      as.character((exchange$typed_margin_positions %||% data.table::data.table())$agent_id),
      as.character((exchange$margin_positions %||% data.table::data.table())$agent_id)
    )
  } else {
    c(
      vapply(names(exchange$spot_states %||% list()), function(key) .parse_agent_state_key(key)$agent_id, character(1L)),
      as.character(exchange$margin_positions$agent_id %||% character())
    )
  }
  unique(c(order_agents, state_agents))
}

.heterogeneous_portfolio_orders <- function(exchange, agent_id, bars) {
  requested_agent_id <- as.character(agent_id)
  boundary <- as.POSIXct(bars$timestamp[1L], tz = "UTC")
  bar_assets <- unique(as.integer(bars$asset_id))
  orders <- exchange$agent_orders[
    status == "accepted" & qty_type == "contracts" & agent_id == requested_agent_id &
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

.heterogeneous_portfolio_account_input <- function(exchange, agent_id, bars,
                                                    margin_asset_ids = integer(),
                                                    active_asset_ids = integer()) {
  requested_agent_id <- as.character(agent_id)
  typed_inventory <- data.table::as.data.table(exchange$inventory_positions %||% data.table::data.table())[
    agent_id == requested_agent_id,
    .(asset_id, currency, units, average_cost, last_price, contract_size, accrued_interest)
  ]
  typed_margin <- data.table::as.data.table(exchange$typed_margin_positions %||% data.table::data.table())[
    agent_id == requested_agent_id,
    .(asset_id)
  ]
  active_asset_ids <- unique(c(
    as.integer(active_asset_ids), as.integer(margin_asset_ids),
    as.integer(typed_inventory$asset_id), as.integer(typed_margin$asset_id)
  ))
  active_asset_ids <- intersect(active_asset_ids, as.integer(bars$asset_id))
  # Do not initialize eight zero inventory states for every single-asset Arena
  # account. A state is needed only for an existing position or an order that
  # can execute at this boundary; the typed kernel accepts absent zero rows.
  if (!.exchange_uses_heterogeneous_v2(exchange)) {
    for (asset_id in active_asset_ids) {
      asset <- .bar_asset_key(bars[asset_id == as.integer(asset_id)][1L])
      if (.asset_uses_spot_inventory(exchange, asset$asset_id)) {
        .ensure_spot_account(exchange, agent_id, asset$asset_id, asset$symbol)
      } else {
        .ensure_agent_account(exchange, agent_id, asset$asset_id, asset$symbol)
      }
    }
  }
  inventory <- if (.exchange_uses_heterogeneous_v2(exchange)) {
    data.table::copy(typed_inventory)
  } else {
    data.table::rbindlist(lapply(names(exchange$spot_states %||% list()), function(key) {
      parsed <- .parse_agent_state_key(key)
      if (!identical(parsed$agent_id, requested_agent_id)) return(NULL)
      spec <- exchange$assets[asset_id == parsed$asset_id]
      if (nrow(spec) != 1L) return(NULL)
      state <- exchange$spot_states[[key]]
      data.table::data.table(
        asset_id = parsed$asset_id, currency = .profile_currency(exchange, state$currency %||% spec$quote_ccy[1L]),
        units = as.numeric(state$units %||% 0), average_cost = as.numeric(state$avg_cost %||% NA_real_),
        last_price = as.numeric(state$last_price %||% NA_real_), contract_size = as.numeric(spec$contract_size[1L]),
        accrued_interest = as.numeric(state$accrued_interest %||% 0)
      )
    }), fill = TRUE)
  }
  if (!ncol(inventory)) inventory <- data.table::data.table(
    asset_id = integer(), currency = character(), units = numeric(), average_cost = numeric(),
    last_price = numeric(), contract_size = numeric(), accrued_interest = numeric()
  )
  if (.exchange_uses_heterogeneous_v2(exchange)) {
    # Preserve the durable v1/v2 projection shape: every account receives one
    # typed zero row for each inventory-profile asset in the boundary. These
    # rows are cheap data.table values and do not recreate compatibility
    # spot_states or trigger per-asset account initialization.
    inventory_asset_ids <- as.integer(bars$asset_id[vapply(
      as.integer(bars$asset_id), function(id) .asset_uses_spot_inventory(exchange, id), logical(1L)
    )])
    missing_inventory_ids <- setdiff(inventory_asset_ids, as.integer(inventory$asset_id))
    if (length(missing_inventory_ids)) {
      missing_specs <- exchange$assets[asset_id %in% missing_inventory_ids,
        .(asset_id, currency = quote_ccy, units = 0, average_cost = NA_real_,
          last_price = NA_real_, contract_size, accrued_interest = 0)]
      inventory <- data.table::rbindlist(list(inventory, missing_specs), fill = TRUE)
    }
  }
  # The typed table is the durable source after save/load. Keep compatibility
  # rows only for assets not yet represented by typed state.
  if (.exchange_uses_heterogeneous_v2(exchange) && nrow(typed_inventory) && !"accrued_interest" %in% names(typed_inventory)) {
    typed_inventory[, accrued_interest := 0]
    inventory <- typed_inventory
  }
  # `margin_positions` is the authoritative derivatives input.  The legacy
  # `agent_states` list is only a compatibility projection for older callers.
  # A target-weight portfolio decision may use the margin account for an
  # otherwise inventory-profile instrument.  This preserves the historical
  # portfolio API's ability to express short ETF/equity targets under an
  # explicit portfolio-margin configuration.  Explicit inventory orders do
  # not opt into this route.
  existing_margin_ids <- unique(c(
    as.integer((exchange$typed_margin_positions %||% data.table::data.table())[agent_id == requested_agent_id, asset_id]),
    as.integer((exchange$margin_positions %||% data.table::data.table())[agent_id == requested_agent_id, asset_id])
  ))
  margin_asset_ids <- unique(c(
    as.integer(margin_asset_ids), existing_margin_ids,
    as.integer(bars$asset_id[!vapply(as.integer(bars$asset_id), function(asset_id) {
      .asset_uses_spot_inventory(exchange, asset_id)
    }, logical(1L))])
  ))
  derivative_bars <- bars[asset_id %in% margin_asset_ids]
  margin <- if (nrow(derivative_bars)) {
    .heterogeneous_derivative_account_input(exchange, agent_id, derivative_bars)
  } else {
    data.table::data.table(
      asset_id = integer(), currency = character(), signed_units = numeric(),
      settlement_price = numeric(), last_price = numeric(), contract_size = numeric(),
      maintenance_rate = numeric(), old_timestamp = numeric()
    )
  }
  if (!ncol(margin)) margin <- data.table::data.table(
    asset_id = integer(), currency = character(), signed_units = numeric(), settlement_price = numeric(),
    last_price = numeric(), contract_size = numeric(), maintenance_rate = numeric()
  )
  balances <- if (.exchange_uses_heterogeneous_v2(exchange)) {
    data.table::as.data.table(exchange$cash_balances)[
      agent_id == requested_agent_id,
      .(currency, amount = settled, unsettled)
    ]
  } else {
    sim_exchange_cash_balances(exchange, agent_id)
  }
  base_currency <- .profile_base_currency(exchange)
  # Older asset registrations may omit quote_ccy. Treat missing currency
  # metadata as the account base currency rather than constructing an invalid
  # FX-rate row or allowing NA to become a data.frame row name.
  inventory[is.na(currency) | !nzchar(as.character(currency)), currency := base_currency]
  margin[is.na(currency) | !nzchar(as.character(currency)), currency := base_currency]
  balances[is.na(currency) | !nzchar(as.character(currency)), currency := base_currency]
  currencies <- unique(c(base_currency, balances$currency, inventory$currency, margin$currency))
  currencies <- currencies[!is.na(currencies) & nzchar(as.character(currencies))]
  cash_input <- if (.exchange_uses_heterogeneous_v2(exchange)) {
    balances[, .(currency, settled = amount, unsettled)]
  } else {
    data.table::as.data.table(.profile_cash_kernel_input(exchange, agent_id))
  }
  missing_currencies <- setdiff(currencies, cash_input$currency)
  if (length(missing_currencies)) {
    cash_input <- data.table::rbindlist(list(cash_input, data.table::data.table(
      currency = missing_currencies, settled = 0, unsettled = 0
    )), fill = TRUE)
  }
  list(
    cash_balances = data.frame(cash_input),
    inventory_positions = data.frame(inventory), margin_positions = data.frame(margin),
    fx_rates = data.frame(currency = currencies, rate_to_base = vapply(currencies, function(currency) {
      .profile_fx_rate(exchange, currency, base_currency)
    }, numeric(1L)))
  )
}

.heterogeneous_portfolio_normalize_orders <- function(exchange, orders, bars,
                                                       margin_asset_ids = integer(),
                                                       specs = NULL, bar_prices = NULL) {
  if (!nrow(orders)) return(sim_heterogeneous_order_batch_schema())
  bars <- data.table::as.data.table(bars)
  specs <- specs %||% exchange$assets[, .(asset_id, instrument_profile, contract_size, qty_step, quote_ccy)]
  bar_prices <- bar_prices %||% bars[, .(asset_id, open)]
  out <- data.table::copy(orders)
  spec_index <- match(as.integer(out$asset_id), as.integer(specs$asset_id))
  price_index <- match(as.integer(out$asset_id), as.integer(bar_prices$asset_id))
  out[, `:=`(
    instrument_profile = specs$instrument_profile[spec_index],
    contract_size = as.numeric(specs$contract_size[spec_index]),
    qty_step = as.numeric(specs$qty_step[spec_index]),
    quote_ccy = as.character(specs$quote_ccy[spec_index]),
    open = as.numeric(bar_prices$open[price_index])
  )]
  if (anyNA(out$instrument_profile) || any(!is.finite(out$open))) {
    stop("Every heterogeneous portfolio order requires a registered asset and current open price.", call. = FALSE)
  }
  out[, `:=`(
    execution_price = ifelse(order_type == "limit", limit_price, open),
    fee_rt = as.numeric(exchange$config$fee_rt %||% 0),
    target_derived = !is.na(rebalance_id),
    atomic_group_id = as.character(atomic_group_id %||% order_id)
  )]
  # A synthetic price-return profile deliberately uses the typed margin
  # account, not spot inventory.  The C++ kernel's future branch is the
  # signed-exposure implementation; the registered profile remains durable
  # metadata and is never changed in the asset registry.
  out[instrument_profile == "synthetic_price_return", instrument_profile := "future"]
  # The registered instrument profile remains the source of calendar and
  # metadata rules.  This execution-only profile identifies target-derived
  # portfolio-margin legs to the typed C++ margin-position branch.
  out[target_derived & asset_id %in% as.integer(margin_asset_ids), instrument_profile := "future"]
  # Target weights are intent-level portfolio decisions.  Fully-paid
  # inventory buys must reserve the configured fee at the executable open;
  # otherwise a 100% target becomes an avoidable atomic rejection after a
  # normal overnight price move. Explicit contract orders retain strict
  # all-or-nothing cash admission in the kernel.
  inventory_target_buys <- which(
    out$target_derived & out$side == "buy" &
      out$instrument_profile %in% c("equity", "etf", "crypto_spot", "fx_spot", "bond")
  )
  for (i in inventory_target_buys) {
    if (is.finite(out$target_weight[i]) && out$target_weight[i] > 1 + 1e-12) next
    available <- .profile_cash_balance(exchange, out$agent_id[i], out$quote_ccy[i])
    unit_cost <- out$execution_price[i] * out$contract_size[i] * (1 + out$fee_rt[i])
    max_qty <- floor((available / unit_cost) / out$qty_step[i] + 1e-10) * out$qty_step[i]
    if (is.finite(max_qty) && max_qty < out$qty[i]) out$qty[i] <- max(0, max_qty)
  }
  # Scale target-derived inventory buys at the atomic-group boundary rather
  # than independently per leg. This reserves one deterministic fee-aware
  # cash budget, preserves target ratios up to quantity-step rounding, and
  # lets a complete-universe target group commit as a documented partial
  # execution instead of rolling back because fees consume the last cents.
  out[, reason_code := as.character(reason_code)]
  target_groups <- unique(out[target_derived %in% TRUE, atomic_group_id])
  for (group_id in target_groups) {
    group_rows <- which(out$atomic_group_id == group_id & out$target_derived %in% TRUE)
    if (!length(group_rows)) next
    inventory_rows <- group_rows[
      out$instrument_profile[group_rows] %in% c("equity", "etf", "crypto_spot", "fx_spot", "bond")
    ]
    buy_rows <- inventory_rows[out$side[inventory_rows] == "buy" & out$qty[inventory_rows] > 0]
    if (!length(buy_rows)) next
    currencies <- unique(out$quote_ccy[buy_rows])
    for (currency in currencies) {
      currency_buys <- buy_rows[out$quote_ccy[buy_rows] == currency]
      if (!length(currency_buys)) next
      currency_sells <- inventory_rows[
        out$quote_ccy[inventory_rows] == currency &
          out$side[inventory_rows] == "sell" & out$qty[inventory_rows] > 0
      ]
      available <- .profile_cash_balance(exchange, out$agent_id[currency_buys[1L]], currency)
      if (length(currency_sells)) {
        available <- available + sum(
          out$qty[currency_sells] * out$execution_price[currency_sells] *
            out$contract_size[currency_sells] * (1 - out$fee_rt[currency_sells]),
          na.rm = TRUE
        )
      }
      requested <- sum(
        out$qty[currency_buys] * out$execution_price[currency_buys] *
          out$contract_size[currency_buys] * (1 + out$fee_rt[currency_buys]),
        na.rm = TRUE
      )
      if (!is.finite(available) || !is.finite(requested) || requested <= available + 1e-10) next
      scale <- max(0, available / requested)
      for (i in currency_buys) {
        step <- abs(out$qty_step[i])
        scaled <- out$qty[i] * scale
        out$qty[i] <- if (is.finite(step) && step > 0) floor(scaled / step + 1e-10) * step else scaled
        out$reason_code[i] <- "fee_scaled"
      }
    }
  }
  out[, `:=`(
    action_code = .encode_step_action(data.table::fifelse(
      is.na(intended_action) | !nzchar(intended_action), "open", intended_action
    )),
    dir_code = .encode_step_dir(data.table::fifelse(
      is.na(intended_dir) | !nzchar(intended_dir),
      data.table::fifelse(side == "buy", "long", data.table::fifelse(side == "sell", "short", "flat")),
      intended_dir
    )),
    order_type_code = .encode_step_order_type(order_type),
    ctr_step = as.numeric(qty_step),
    fund_rt = as.numeric(exchange$config$fund_rt %||% 0),
    funding_interval_hours = as.numeric(exchange$config$funding_interval_hours %||% 8)
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
  .heterogeneous_inventory_commit_state(
    exchange, agent_id, proposed, data.table::data.table(), record_typed_state = FALSE
  )
  for (asset_id in as.integer(proposed$inventory_positions$asset_id)) {
    exchange$agent_states[[.agent_state_key(agent_id, asset_id)]] <- NULL
  }
  margin <- data.table::as.data.table(proposed$margin_positions)
  if (nrow(margin)) {
    margin[, `:=`(agent_id = as.character(agent_id), old_timestamp = as.numeric(proposed$timestamp %||% NA_real_))]
    exchange$margin_positions <- exchange$margin_positions[!(agent_id == as.character(agent_id) & asset_id %in% margin$asset_id)]
    exchange$margin_positions <- data.table::rbindlist(list(exchange$margin_positions, margin), fill = TRUE)
  }
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

#' @keywords internal
.heterogeneous_v2_record_state <- function(exchange, agent_id, proposed, timestamp) {
  if (!.exchange_uses_heterogeneous_v2(exchange)) return(invisible(NULL))
  agent_id <- as.character(agent_id)
  timestamp <- .profile_utc_timestamp(timestamp)
  event_timestamp <- timestamp
  replace_rows <- function(table_name, rows, key_columns) {
    if (!nrow(rows)) return(invisible(NULL))
    current <- exchange[[table_name]]
    key <- do.call(paste, c(current[, key_columns, with = FALSE], sep = "\r"))
    replacement_key <- do.call(paste, c(rows[, key_columns, with = FALSE], sep = "\r"))
    exchange[[table_name]] <- data.table::rbindlist(list(current[!key %in% replacement_key], rows), fill = TRUE)
    if ("timestamp" %in% names(exchange[[table_name]])) {
      data.table::set(exchange[[table_name]], j = "timestamp",
        value = .profile_utc_timestamp(exchange[[table_name]]$timestamp))
    }
    invisible(NULL)
  }
  cash <- data.table::as.data.table(proposed$cash_balances)
  if (nrow(cash)) {
    cash[, `:=`(agent_id = agent_id, timestamp = timestamp)]
    data.table::setcolorder(cash, names(exchange$cash_balances))
    replace_rows("cash_balances", cash, c("agent_id", "currency"))
  }
  inventory <- data.table::as.data.table(proposed$inventory_positions)
  if (nrow(inventory)) {
    inventory[, `:=`(
      agent_id = agent_id,
      symbol = vapply(asset_id, function(id) exchange$asset_symbols[[as.character(id)]] %||% paste0("asset-", id), character(1L)),
      timestamp = timestamp
    )]
    data.table::setcolorder(inventory, names(exchange$inventory_positions))
    replace_rows("inventory_positions", inventory, c("agent_id", "asset_id"))
  }
  margin <- data.table::as.data.table(proposed$margin_positions)
  if (nrow(margin)) {
    margin[, `:=`(
      agent_id = agent_id,
      symbol = vapply(asset_id, function(id) exchange$asset_symbols[[as.character(id)]] %||% paste0("asset-", id), character(1L)),
      timestamp = timestamp
    )]
    data.table::setcolorder(margin, names(exchange$typed_margin_positions))
    replace_rows("typed_margin_positions", margin, c("agent_id", "asset_id"))
  }
  event_source <- proposed$account_events
  if (is.null(event_source) || !nrow(data.table::as.data.table(event_source))) event_source <- proposed$events
  events <- data.table::as.data.table(event_source)
  if ("event_type" %in% names(events)) {
    events <- events[as.character(event_type) %in% c("funding", "variation_margin", "bond_coupon", "bond_accrual", "redemption")]
  }
  if (nrow(events) && (!"currency" %in% names(events) || all(is.na(events$currency)))) {
    currency_map <- stats::setNames(as.character(exchange$assets$quote_ccy), as.character(exchange$assets$asset_id))
    events[, currency := unname(currency_map[as.character(asset_id)])]
  }
  if (nrow(events)) {
    rows <- events[, .(
      account_event_id = paste0("AE", sprintf("%06d", exchange$next_account_event_id + seq_len(.N) - 1L)),
      timestamp = event_timestamp, agent_id = agent_id,
      event_type = as.character(event_type), asset_id = as.integer(asset_id),
      symbol = vapply(asset_id, function(id) exchange$asset_symbols[[as.character(id)]] %||% paste0("asset-", id), character(1L)),
      currency = as.character(currency), amount = as.numeric(amount),
      order_id = NA_character_, fill_id = NA_character_, atomic_group_id = NA_character_,
      message = "Typed heterogeneous account event."
    )]
    exchange$next_account_event_id <- exchange$next_account_event_id + nrow(rows)
    exchange$account_events <- data.table::rbindlist(list(exchange$account_events, rows), fill = TRUE)
  }
  fills <- data.table::as.data.table(proposed$fills)
  fills <- fills[status == "filled"]
  if (nrow(fills)) {
    position_currency <- c(
      stats::setNames(as.character(margin$currency), as.character(margin$asset_id)),
      stats::setNames(as.character(inventory$currency), as.character(inventory$asset_id))
    )
    rows <- fills[, .(
      account_event_id = paste0("AE", sprintf("%06d", exchange$next_account_event_id + seq_len(.N) - 1L)),
      timestamp = as.POSIXct(timestamp, tz = "UTC"), agent_id = agent_id,
      event_type = "trade_fill", asset_id = as.integer(asset_id),
      symbol = vapply(asset_id, function(id) exchange$asset_symbols[[as.character(id)]] %||% paste0("asset-", id), character(1L)),
      currency = unname(position_currency[as.character(asset_id)]),
      amount = -as.numeric(fee), order_id = as.character(order_id), fill_id = as.character(fill_id),
      atomic_group_id = as.character(atomic_group_id), message = "Typed heterogeneous trade fill."
    )]
    exchange$next_account_event_id <- exchange$next_account_event_id + nrow(rows)
    exchange$account_events <- data.table::rbindlist(list(exchange$account_events, rows), fill = TRUE)
  }
  invisible(NULL)
}

.heterogeneous_portfolio_apply_outcomes <- function(exchange, orders, proposed, timestamp,
                                                     margin_asset_ids = integer()) {
  events <- list()
  fills <- data.table::as.data.table(proposed$fills)
  for (i in seq_len(nrow(fills))) {
    fill <- fills[i]
    order <- orders[order_id == fill$order_id]
    if (nrow(order) != 1L || identical(as.character(fill$status), "pending")) next
    if (identical(as.character(fill$status), "filled")) {
      events[[length(events) + 1L]] <- .heterogeneous_inventory_apply_fill(
        exchange, order, fill, timestamp,
        force_margin = as.integer(order$asset_id[1L]) %in% as.integer(margin_asset_ids)
      )
    } else {
      reason_code <- as.character(fill$reason_code[1L])
      # A one-leg explicit contract order has no prior provisional leg to
      # roll back. Preserve its public contract-order rejection semantics
      # rather than exposing the kernel's internal group implementation.
      if (!isTRUE(order$target_derived[1L]) && identical(reason_code, "atomic_group_rejected")) {
        reason_code <- "execution_rejected"
      }
      .spot_mark_order_terminal(exchange, order$order_id[1L], as.character(fill$status),
        reason_code, .heterogeneous_inventory_message(as.character(fill$status), reason_code),
        timestamp, price = fill$price[1L], fee = fill$fee[1L], realized_pnl = fill$realized_pnl[1L])
    }
  }
  data.table::rbindlist(events, fill = TRUE)
}

.heterogeneous_portfolio_variation_events <- function(exchange, agent_id, proposed, timestamp) {
  # Derivatives-only v2 calls expose typed account events separately from the
  # legacy execution-event projection. Mixed account calls already use
  # `events`; both are C++-authoritative cash settlements.
  event_source <- proposed$account_events
  if (is.null(event_source) || !nrow(data.table::as.data.table(event_source))) event_source <- proposed$events
  variations <- data.table::as.data.table(event_source)
  if ("event_type" %in% names(variations)) {
    variations <- variations[as.character(event_type) %in% c("funding", "variation_margin", "bond_coupon", "bond_accrual", "redemption")]
  }
  # The derivatives compatibility kernel records funding in the exact C++
  # recorder's `funding_fee` column. Project it as a typed cash event instead
  # of re-estimating it from R state.
  legacy_events <- data.table::as.data.table(proposed$events %||% data.table::data.table())
  if (!nrow(variations) && "funding_fee" %in% names(legacy_events)) {
    funding <- legacy_events[is.finite(funding_fee) & abs(funding_fee) > 1e-12]
    if (nrow(funding)) {
      funding[, `:=`(
        event_type = "funding", amount = -as.numeric(funding_fee),
        currency = vapply(asset_id, function(id) {
          asset <- exchange$assets[asset_id == as.integer(id)]
          .profile_currency(exchange, asset$quote_ccy[1L])
        }, character(1L)),
        settlement_price = as.numeric(price), cash_effect = TRUE
      )]
      variations <- data.table::rbindlist(list(variations, funding[, .(
        event_type, asset_id, currency, amount, settlement_price, cash_effect
      )]), fill = TRUE)
    }
  }
  if (nrow(variations) && (!"currency" %in% names(variations) || all(is.na(variations$currency)))) {
    currency_map <- stats::setNames(as.character(exchange$assets$quote_ccy), as.character(exchange$assets$asset_id))
    variations[, currency := unname(currency_map[as.character(asset_id)])]
  }
  balances <- data.table::as.data.table(proposed$cash_balances)
  if ("cash_effect" %in% names(variations)) variations <- variations[cash_effect == TRUE]
  if (!nrow(variations)) return(data.table::data.table())
  first_event_id <- .next_spot_event_id(exchange)
  rows <- lapply(seq_len(nrow(variations)), function(i) {
    row <- variations[i]
    event_asset_id <- as.integer(row[["asset_id"]][1L])
    event_currency <- as.character(row[["currency"]][1L])
    event_amount <- as.numeric(row[["amount"]][1L])
    event_type <- as.character(row[["event_type"]][1L])
    event_price <- if ("settlement_price" %in% names(row)) as.numeric(row[["settlement_price"]][1L]) else NA_real_
    asset <- exchange$assets[asset_id == event_asset_id]
    if (nrow(asset) != 1L) return(NULL)
    balance <- balances[
      currency == event_currency, settled
    ]
    .profile_record_cash(exchange, timestamp, agent_id, event_currency, event_amount,
      if (length(balance)) balance[1L] else .profile_cash_balance(exchange, agent_id, event_currency),
      event_type, event_asset_id, asset$symbol[1L],
      message = switch(event_type,
        funding = "Funding settled through heterogeneous execution.",
        variation_margin = "Futures variation margin settled through heterogeneous execution.",
        bond_coupon = "Bond coupon booked through heterogeneous execution.",
        bond_accrual = "Bond accrual booked through heterogeneous execution.",
        redemption = "Bond redemption booked through heterogeneous execution.",
        "Typed account event booked through heterogeneous execution."
      ))
    .profile_record_account_event(exchange, timestamp, agent_id, event_type,
      event_asset_id, asset$symbol[1L], event_currency, event_amount,
      switch(event_type,
        funding = "Funding settled through heterogeneous execution.",
        variation_margin = "Futures variation margin settled through heterogeneous execution.",
        "Typed account event booked through heterogeneous execution."
      ))
    data.table::data.table(
      timestamp = timestamp, event_id = first_event_id + i - 1L,
      event_type = if (identical(event_type, "funding")) 5L else if (identical(event_type, "variation_margin")) 4L else 6L,
      event_type_label = event_type, action_id = 0L, status_label = "filled",
      action_label = event_type, dir_label = "flat", ctr_qty = 0,
      price = event_price, cash = .profile_cash_balance(exchange, agent_id, event_currency),
      equity = .profile_agent_equity(exchange, agent_id), fee = 0,
      realized_pnl = event_amount, agent_id = as.character(agent_id),
      symbol = asset$symbol[1L], asset_id = event_asset_id, order_id = NA_character_
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

#' @keywords internal
.heterogeneous_portfolio_bond_actions <- function(exchange, bars, timestamp) {
  actions <- exchange$corporate_actions[
    status == "pending" & action_type %in% c("coupon", "bond_accrual", "redemption") &
      asset_id %in% as.integer(bars$asset_id) & effective_timestamp <= timestamp
  ]
  if (!nrow(actions)) return(actions)
  actions[, .(
    action_id, asset_id = as.integer(asset_id), action_type = as.character(action_type),
    amount = as.numeric(amount), currency = as.character(currency),
    effective_timestamp = as.numeric(effective_timestamp)
  )]
}

#' @keywords internal
.heterogeneous_portfolio_mark_bond_actions_applied <- function(exchange, actions) {
  if (!nrow(actions)) return(invisible(NULL))
  for (action_id in actions$action_id) {
    index <- match(action_id, exchange$corporate_actions$action_id)
    if (is.na(index)) next
    data.table::set(exchange$corporate_actions, i = index, j = "status", value = "applied")
    data.table::set(exchange$corporate_actions, i = index, j = "message",
      value = "Applied through the heterogeneous account kernel.")
  }
  invisible(NULL)
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
    for (i in seq_len(nrow(boundary_bars))) .profile_apply_future_lifecycle(exchange, boundary_timestamp, boundary_bars$asset_id[i])
    # Dividends and splits retain the legacy inventory mutation until their
    # own typed C++ schedule exists. Bond cash lifecycle rows are passed to
    # the heterogeneous kernel below, once per account.
    for (i in seq_len(nrow(boundary_bars))) {
      .profile_apply_corporate_actions(exchange, boundary_timestamp, boundary_bars$asset_id[i],
        action_types = c("dividend", "split"))
    }
    bond_actions <- .heterogeneous_portfolio_bond_actions(exchange, boundary_bars, boundary_timestamp)
    bond_schedules <- .bond_schedule_kernel_rows(exchange, boundary_bars, boundary_timestamp)
    # These inputs are identical for every account at a market boundary. Build
    # them once rather than repeating sorting, merging, and covariance-table
    # allocation for each Arena competitor.
    asset_ids <- as.integer(boundary_bars$asset_id)
    cov <- .cross_asset_covariance(exchange, asset_ids)
    covariance <- data.table::as.data.table(as.data.frame(as.table(cov)))
    data.table::setnames(covariance, c("asset_i_index", "asset_j_index", "covariance"))
    covariance[, `:=`(
      asset_i = asset_ids[as.integer(asset_i_index)],
      asset_j = asset_ids[as.integer(asset_j_index)]
    )]
    kernel_bars <- data.frame(merge(
      boundary_bars[, .(asset_id, open, high, low, close)],
      exchange$assets[, .(asset_id, instrument_profile)],
      by = "asset_id", all.x = TRUE, sort = FALSE
    ))
    settings <- data.frame(
      execution_mode = "mixed_portfolio_native",
      lev = as.numeric(exchange$config$lev %||% 10),
      fund_rt = as.numeric(exchange$config$fund_rt %||% 0),
      funding_interval_hours = as.numeric(exchange$config$funding_interval_hours %||% 8),
      mmr = as.numeric(exchange$config$mmr %||% 0.02),
      portfolio_margin_sigma = as.numeric(exchange$config$portfolio_margin_sigma %||% 3),
      portfolio_margin_floor = as.numeric(exchange$config$portfolio_margin_floor %||% exchange$config$mmr %||% 0.02)
    )
    kernel_actions <- data.frame(data.table::rbindlist(list(
      covariance[, .(asset_i, asset_j, covariance)], bond_actions, bond_schedules
    ), fill = TRUE))
    order_specs <- exchange$assets[, .(asset_id, instrument_profile, contract_size, qty_step, quote_ccy)]
    order_prices <- boundary_bars[, .(asset_id, open)]
    for (agent_id in .heterogeneous_portfolio_agents(exchange, boundary_bars)) {
      normalization_started <- .sim_profile_start(exchange)
      requested_agent_id <- as.character(agent_id)
      accepted <- .heterogeneous_portfolio_orders(exchange, agent_id, boundary_bars)
      existing_margin_ids <- unique(c(
        as.integer((exchange$typed_margin_positions %||% data.table::data.table())[agent_id == requested_agent_id, asset_id]),
        as.integer((exchange$margin_positions %||% data.table::data.table())[agent_id == requested_agent_id, asset_id])
      ))
      target_margin_ids <- accepted[
        target_derived %in% TRUE & !is.na(rebalance_id) &
          asset_id %in% boundary_bars$asset_id &
          ((isTRUE(exchange$config$portfolio_margin %||% FALSE) == FALSE &
            vapply(asset_id, function(id) .asset_uses_spot_inventory(exchange, id), logical(1L))) |
            asset_id %in% existing_margin_ids |
            intended_dir == "short" |
            !vapply(asset_id, function(id) .asset_uses_spot_inventory(exchange, id), logical(1L))),
        unique(as.integer(asset_id))
      ]
      input <- .heterogeneous_portfolio_account_input(
        exchange, agent_id, boundary_bars,
        margin_asset_ids = target_margin_ids,
        active_asset_ids = as.integer(accepted$asset_id)
      )
      normalized <- .heterogeneous_portfolio_normalize_orders(
        exchange, accepted, boundary_bars, margin_asset_ids = target_margin_ids,
        specs = order_specs, bar_prices = order_prices
      )
      # Carry deterministic pre-admission fee scaling into the durable order
      # projection and the outcome projector. The original planned quantity
      # remains in portfolio_targets for execution-quality comparison.
      if ("reason_code" %in% names(normalized) && any(normalized$reason_code == "fee_scaled", na.rm = TRUE)) {
        if (!"reason_code" %in% names(accepted)) accepted[, reason_code := NA_character_]
        for (current_order_id in normalized$order_id[normalized$reason_code == "fee_scaled"]) {
          accepted[order_id == current_order_id, reason_code := "fee_scaled"]
          index <- match(current_order_id, exchange$agent_orders$order_id)
          if (!is.na(index)) {
            data.table::set(exchange$agent_orders, i = index, j = "reason_code", value = "fee_scaled")
            data.table::set(exchange$agent_orders, i = index, j = "message", value = "Target-derived group was scaled to reserve execution fees.")
          }
        }
      }
      .sim_profile_add(exchange, "heterogeneous_r_normalization", normalization_started)
      kernel_started <- .sim_profile_start(exchange)
      proposed <- heterogeneous_account_step_rcpp(
        .profile_base_currency(exchange), input$cash_balances, input$inventory_positions, input$margin_positions,
        kernel_bars, input$fx_rates, settings, kernel_actions, data.frame(normalized),
        as.numeric(boundary_timestamp)
      )
      .sim_profile_add(exchange, "portfolio_step_rcpp", kernel_started)
      proposed$timestamp <- as.numeric(boundary_timestamp)
      proposed$cash_balances <- data.table::as.data.table(proposed$cash_balances)
      proposed$inventory_positions <- data.table::as.data.table(proposed$inventory_positions)
      proposed$margin_positions <- data.table::as.data.table(proposed$margin_positions)
      proposed$fills <- data.table::as.data.table(proposed$fills)
      proposed$events <- data.table::as.data.table(proposed$events)
      ledger_started <- .sim_profile_start(exchange)
      .heterogeneous_portfolio_commit_state(exchange, agent_id, proposed)
      # Fill account events are projected here. Lifecycle cash events are
      # projected once below with their matching public step event and
      # profile-cash-ledger entry. Non-cash typed events, such as bond
      # accrual, remain durable account events here.
      typed_proposed <- proposed
      typed_proposed$account_events <- exchange$account_events[0]
      typed_proposed$events <- proposed$events
      if ("cash_effect" %in% names(typed_proposed$events)) {
        typed_proposed$events <- typed_proposed$events[is.na(cash_effect) | cash_effect != TRUE]
      } else {
        typed_proposed$events <- exchange$account_events[0]
      }
      .heterogeneous_v2_record_state(exchange, agent_id, typed_proposed, boundary_timestamp)
      outcome_events <- .heterogeneous_portfolio_apply_outcomes(
        exchange, accepted, proposed, boundary_timestamp,
        margin_asset_ids = target_margin_ids
      )
      variation_events <- .heterogeneous_portfolio_variation_events(exchange, agent_id, proposed, boundary_timestamp)
      if (nrow(variation_events)) {
        data.table::set(variation_events, j = "event_id", value = max(c(
          0L, exchange$step_events$event_id %||% integer(), outcome_events$event_id %||% integer()
        ), na.rm = TRUE) + seq_len(nrow(variation_events)))
      }
      .sim_profile_add(exchange, "heterogeneous_ledger_projection", ledger_started)
      if (nrow(outcome_events)) events[[length(events) + 1L]] <- outcome_events
      if (nrow(variation_events)) events[[length(events) + 1L]] <- variation_events
      snapshot_started <- .sim_profile_start(exchange)
      .enforce_cross_margin(exchange, agent_id, boundary_timestamp)
      snapshot <- .agent_position_snapshots(exchange, agent_id, boundary_timestamp)
      # Typed cash is authoritative in a heterogeneous account. In particular,
      # a non-base futures settlement balance must contribute to every legacy
      # per-position snapshot rather than being lost in its base-cash field.
      typed_account <- sim_exchange_account_state(exchange, agent_id)$account
      has_non_base_cash <- nrow(exchange$cash_balances[
        agent_id == as.character(agent_id) & currency != .profile_base_currency(exchange) &
          (abs(settled) > 1e-12 | abs(unsettled) > 1e-12)
      ]) > 0L
      if (nrow(snapshot) && nrow(typed_account) && has_non_base_cash) {
        data.table::set(snapshot, j = "equity", value = as.numeric(typed_account$equity[1L]))
        data.table::set(snapshot, j = "cash", value = as.numeric(typed_account$cash_settled[1L]))
        data.table::set(snapshot, j = "maintenance_margin", value = as.numeric(typed_account$maintenance_margin[1L]))
      }
      snapshots[[length(snapshots) + 1L]] <- snapshot
      .sim_profile_add(exchange, "snapshot_construction", snapshot_started)
    }
    .heterogeneous_portfolio_mark_bond_actions_applied(exchange, bond_actions)
    .bond_schedule_advance(exchange, bond_schedules, boundary_timestamp)
  }
  list(events = data.table::rbindlist(events, fill = TRUE), snapshots = data.table::rbindlist(snapshots, fill = TRUE))
}
