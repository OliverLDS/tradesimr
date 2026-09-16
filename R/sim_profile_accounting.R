#' Set a foreign-exchange conversion rate
#'
#' Rates express units of `to_ccy` per one unit of `from_ccy`. They are used
#' only for account valuation and explicit currency conversion; they never
#' alter an execution price.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param from_ccy Source currency.
#' @param to_ccy Destination currency.
#' @param rate Positive conversion rate.
#' @param timestamp Valuation timestamp.
#' @param source Public-safe source label.
#' @return Invisibly returns the added rate row.
#' @export
sim_exchange_fx_rate <- function(exchange, from_ccy, to_ccy, rate,
                                 timestamp = Sys.time(), source = "manual") {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  from_ccy <- toupper(as.character(from_ccy))
  to_ccy <- toupper(as.character(to_ccy))
  if (!nzchar(from_ccy) || !nzchar(to_ccy) || !is.finite(rate) || rate <= 0) {
    stop("Currencies and a positive finite `rate` are required.", call. = FALSE)
  }
  row <- data.table::data.table(timestamp = as.POSIXct(timestamp, tz = "UTC"),
    from_ccy = from_ccy, to_ccy = to_ccy, rate = as.numeric(rate), source = as.character(source))
  exchange$fx_rates <- data.table::rbindlist(list(exchange$fx_rates, row), fill = TRUE)
  invisible(row)
}

#' Deposit or withdraw a profile-aware currency balance
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Account identifier.
#' @param amount Signed amount.
#' @param currency Currency code. Defaults to the exchange base currency.
#' @param timestamp Ledger timestamp.
#' @param message Public-safe ledger description.
#' @return Invisibly returns the resulting currency balance.
#' @export
sim_exchange_cash_adjust <- function(exchange, agent_id, amount,
                                     currency = NULL, timestamp = Sys.time(),
                                     message = "Manual cash adjustment") {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (!is.finite(amount)) stop("`amount` must be finite.", call. = FALSE)
  agent_id <- as.character(agent_id)
  .ensure_shared_account(exchange, agent_id)
  currency <- .profile_currency(exchange, currency)
  balance <- .profile_cash_balance(exchange, agent_id, currency) + as.numeric(amount)
  .profile_set_cash_balance(exchange, agent_id, currency, balance)
  .profile_record_cash(exchange, timestamp, agent_id, currency, amount, balance,
    event_type = "cash_adjustment", message = message)
  invisible(balance)
}

#' Convert cash between currencies at an authoritative exchange FX mark
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Account identifier.
#' @param amount Source-currency amount to convert.
#' @param from_ccy,to_ccy Currency codes.
#' @param timestamp Ledger timestamp.
#' @return Invisibly returns the destination amount.
#' @export
sim_exchange_convert_cash <- function(exchange, agent_id, amount, from_ccy, to_ccy,
                                      timestamp = Sys.time()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (!is.finite(amount) || amount <= 0) stop("`amount` must be positive and finite.", call. = FALSE)
  .ensure_shared_account(exchange, agent_id)
  from_ccy <- .profile_currency(exchange, from_ccy); to_ccy <- .profile_currency(exchange, to_ccy)
  available <- .profile_cash_balance(exchange, agent_id, from_ccy)
  if (available + 1e-10 < amount) stop("Insufficient source-currency cash for conversion.", call. = FALSE)
  received <- as.numeric(amount) * .profile_fx_rate(exchange, from_ccy, to_ccy)
  .profile_set_cash_balance(exchange, agent_id, from_ccy, available - amount)
  .profile_record_cash(exchange, timestamp, agent_id, from_ccy, -amount, available - amount, "fx_conversion", message = "FX conversion source debit.")
  destination <- .profile_cash_balance(exchange, agent_id, to_ccy) + received
  .profile_set_cash_balance(exchange, agent_id, to_ccy, destination)
  .profile_record_cash(exchange, timestamp, agent_id, to_ccy, received, destination, "fx_conversion", message = "FX conversion destination credit.")
  invisible(received)
}

#' Configure borrow and cash interest rates
#'
#' Rates are annualized simple rates keyed by registered symbol for borrow and
#' by currency for settled-cash interest. Positive cash earns the configured
#' rate; negative cash is charged it. Short inventory is charged its symbol's
#' borrow rate from the inventory quote-currency balance.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param borrow_rates Named numeric annualized rates keyed by symbol.
#' @param cash_interest_rates Named numeric annualized rates keyed by currency.
#' @return Invisibly returns the configured rates.
#' @export
sim_exchange_set_carry_rates <- function(exchange, borrow_rates = numeric(),
                                         cash_interest_rates = numeric()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  validate_rates <- function(rates, name) {
    if (length(rates) && (is.null(names(rates)) || any(!nzchar(names(rates))) || any(!is.finite(rates)))) {
      stop("`", name, "` must be a named finite numeric vector.", call. = FALSE)
    }
    stats::setNames(as.numeric(rates), toupper(as.character(names(rates))))
  }
  exchange$config$borrow_rates <- validate_rates(borrow_rates, "borrow_rates")
  exchange$config$cash_interest_rates <- validate_rates(cash_interest_rates, "cash_interest_rates")
  invisible(list(borrow_rates = exchange$config$borrow_rates,
    cash_interest_rates = exchange$config$cash_interest_rates))
}

#' Accrue profile-aware borrow and cash interest
#'
#' The function is idempotent at a timestamp and is called automatically before
#' every executable exchange boundary. Call it explicitly to establish an
#' initial accrual cursor or to accrue a durable account without new bars.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param timestamp Accrual boundary.
#' @return A data.table of booked carry events.
#' @export
sim_exchange_accrue_carry <- function(exchange, timestamp) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  .profile_accrue_carry(exchange, timestamp)
}

#' Get profile-aware cash balances
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Optional account identifier.
#' @return A data.table where `amount` is settled cash (kept for compatibility),
#'   `unsettled` is pending settlement cash, and the base-value columns report
#'   settled-only and total cash valuation respectively.
#' @export
sim_exchange_cash_balances <- function(exchange, agent_id = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  typed <- data.table::as.data.table(exchange$cash_balances %||% sim_schemas()$cash_balances[0])
  cash_keys <- names(exchange$currency_cash) %||% character()
  ids <- if (is.null(agent_id)) {
    unique(c(names(exchange$agent_accounts), sub("\r.*$", "", cash_keys), typed$agent_id))
  } else {
    as.character(agent_id)
  }
  rows <- unlist(lapply(ids, function(id) {
    agent_keys <- cash_keys[startsWith(cash_keys, paste0(id, "\r"))]
    currencies <- unique(c(.profile_base_currency(exchange), sub("^.*\r", "", agent_keys),
      typed[agent_id == id, currency]))
    lapply(currencies, function(ccy) data.table::data.table(
      agent_id = id, currency = ccy,
      amount = .profile_cash_balance(exchange, id, ccy),
      unsettled = .profile_unsettled_cash(exchange, id, ccy),
      base_value = .profile_to_base(exchange, .profile_cash_balance(exchange, id, ccy), ccy),
      total_base_value = .profile_to_base(exchange,
        .profile_cash_balance(exchange, id, ccy) + .profile_unsettled_cash(exchange, id, ccy), ccy)
    ))
  }), recursive = FALSE)
  out <- data.table::rbindlist(rows, fill = TRUE)
  if (!ncol(out)) {
    out <- data.table::data.table(agent_id = character(), currency = character(), amount = numeric(),
      unsettled = numeric(), base_value = numeric(), total_base_value = numeric())
  }
  out
}

#' Get the durable heterogeneous account state
#'
#' Returns the typed, profile-aware account projection used by the
#' heterogeneous execution engine. Cash is valued in the exchange base
#' currency; fully paid inventory contributes marked market value, while
#' margin positions contribute marked unrealized P&L and maintenance margin.
#' `sim_exchange_account()` remains the compatibility account snapshot API.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Optional account identifier.
#' @return A named list containing `account`, `cash_balances`,
#'   `inventory_positions`, `margin_positions`, and `events` data.tables.
#' @export
sim_exchange_account_state <- function(exchange, agent_id = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  filter_agents <- function(table, ids) {
    table <- data.table::copy(table)
    if (!is.null(ids) && "agent_id" %in% names(table)) table <- table[table[["agent_id"]] %in% ids]
    table
  }
  requested_ids <- if (is.null(agent_id)) NULL else as.character(agent_id)
  cash <- filter_agents(exchange$cash_balances %||% sim_schemas()$cash_balances[0], requested_ids)
  inventory <- filter_agents(exchange$inventory_positions %||% sim_schemas()$inventory_positions[0], requested_ids)
  margin <- filter_agents(exchange$typed_margin_positions %||% sim_schemas()$margin_positions[0], requested_ids)
  events <- filter_agents(exchange$account_events %||% sim_schemas()$account_events[0], requested_ids)
  base_currency <- .profile_base_currency(exchange)

  if (nrow(cash)) {
    cash[, `:=`(
      settled_base = vapply(seq_len(.N), function(i) .profile_to_base(exchange, settled[i], currency[i]), numeric(1L)),
      unsettled_base = vapply(seq_len(.N), function(i) .profile_to_base(exchange, unsettled[i], currency[i]), numeric(1L))
    )]
    cash[, total_base := settled_base + unsettled_base]
  } else {
    cash[, `:=`(settled_base = numeric(), unsettled_base = numeric(), total_base = numeric())]
  }
  if (nrow(inventory)) {
    if (!"accrued_interest" %in% names(inventory)) inventory[, accrued_interest := 0]
    inventory[, `:=`(
      market_value = units * last_price * contract_size + accrued_interest,
      unrealized_pnl = (last_price - average_cost) * units * contract_size
    )]
    inventory[, `:=`(
      market_value_base = vapply(seq_len(.N), function(i) .profile_to_base(exchange, market_value[i], currency[i]), numeric(1L)),
      unrealized_pnl_base = vapply(seq_len(.N), function(i) .profile_to_base(exchange, unrealized_pnl[i], currency[i]), numeric(1L))
    )]
  } else {
    inventory[, `:=`(accrued_interest = numeric(), market_value = numeric(), unrealized_pnl = numeric(),
      market_value_base = numeric(), unrealized_pnl_base = numeric())]
  }
  if (nrow(margin)) {
    margin[, `:=`(
      notional = signed_units * last_price * contract_size,
      unrealized_pnl = (last_price - settlement_price) * signed_units * contract_size,
      maintenance_margin = abs(signed_units * last_price * contract_size) * maintenance_rate
    )]
    margin[, `:=`(
      notional_base = vapply(seq_len(.N), function(i) .profile_to_base(exchange, notional[i], currency[i]), numeric(1L)),
      unrealized_pnl_base = vapply(seq_len(.N), function(i) .profile_to_base(exchange, unrealized_pnl[i], currency[i]), numeric(1L)),
      maintenance_margin_base = vapply(seq_len(.N), function(i) .profile_to_base(exchange, maintenance_margin[i], currency[i]), numeric(1L))
    )]
  } else {
    margin[, `:=`(notional = numeric(), unrealized_pnl = numeric(), maintenance_margin = numeric(),
      notional_base = numeric(), unrealized_pnl_base = numeric(), maintenance_margin_base = numeric())]
  }

  ids <- requested_ids %||% unique(c(cash$agent_id, inventory$agent_id, margin$agent_id, events$agent_id,
    names(exchange$agent_accounts %||% list())))
  empty_account <- data.table::data.table(
    timestamp = as.POSIXct(character()), agent_id = character(), base_currency = character(),
    cash_settled = numeric(), cash_unsettled = numeric(), inventory_value = numeric(),
    margin_unrealized_pnl = numeric(), notional = numeric(), maintenance_margin = numeric(),
    equity = numeric(), liquidated = logical()
  )
  account_rows <- lapply(ids, function(id) {
    cash_rows <- cash[cash$agent_id == id]
    inventory_rows <- inventory[inventory$agent_id == id]
    margin_rows <- margin[margin$agent_id == id]
    event_rows <- events[events$agent_id == id]
    timestamps <- c(cash_rows$timestamp, inventory_rows$timestamp, margin_rows$timestamp, event_rows$timestamp)
    timestamps <- timestamps[is.finite(as.numeric(timestamps))]
    if (!length(timestamps) && nrow(exchange$market_events)) timestamps <- tail(exchange$market_events$timestamp, 1L)
    timestamp <- if (length(timestamps)) .profile_utc_timestamp(max(as.numeric(timestamps))) else as.POSIXct(NA, tz = "UTC")
    cash_settled <- sum(cash_rows$settled_base, na.rm = TRUE)
    cash_unsettled <- sum(cash_rows$unsettled_base, na.rm = TRUE)
    inventory_value <- sum(inventory_rows$market_value_base, na.rm = TRUE)
    margin_unrealized_pnl <- sum(margin_rows$unrealized_pnl_base, na.rm = TRUE)
    maintenance_margin <- sum(margin_rows$maintenance_margin_base, na.rm = TRUE)
    account <- exchange$agent_accounts[[as.character(id)]] %||% list()
    data.table::data.table(
      timestamp = timestamp, agent_id = as.character(id), base_currency = base_currency,
      cash_settled = cash_settled, cash_unsettled = cash_unsettled,
      inventory_value = inventory_value, margin_unrealized_pnl = margin_unrealized_pnl,
      notional = sum(margin_rows$notional_base, na.rm = TRUE),
      maintenance_margin = maintenance_margin,
      equity = cash_settled + cash_unsettled + inventory_value + margin_unrealized_pnl,
      liquidated = isTRUE(account$liquidated)
    )
  })
  account <- data.table::rbindlist(account_rows, fill = TRUE)
  if (!nrow(account)) account <- empty_account
  list(account = account[], cash_balances = cash[], inventory_positions = inventory[],
    margin_positions = margin[], events = events[])
}

#' @keywords internal
.profile_cash_kernel_input <- function(exchange, agent_id) {
  balances <- sim_exchange_cash_balances(exchange, agent_id)
  if (!nrow(balances)) {
    balances <- data.table::data.table(
      currency = .profile_base_currency(exchange), amount = .shared_cash(exchange, agent_id), unsettled = 0
    )
  }
  data.frame(
    currency = as.character(balances$currency),
    settled = as.numeric(balances$amount),
    unsettled = as.numeric(balances$unsettled %||% 0)
  )
}

#' Register a durable inventory corporate action
#'
#' Actions are applied at the first exchange step at or after their effective
#' timestamp and retained in a durable audit table.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param symbol Registered symbol.
#' @param action_type One of `dividend`, `split`, `coupon`, `bond_accrual`,
#'   `redemption`, `delisting`, `future_expiry`, or `future_roll`.
#' @param amount Cash per inventory unit for dividend/coupon/accrual/redemption,
#'   split ratio for `split`, or the per-unit cash settlement price for
#'   `delisting`.
#' @param effective_timestamp Action timestamp.
#' @param currency Action currency. Defaults to the asset quote currency.
#' @return Invisibly returns the action id.
#' @export
sim_exchange_corporate_action <- function(exchange, symbol,
                                          action_type = c("dividend", "split", "coupon", "bond_accrual", "redemption", "delisting", "future_expiry", "future_roll"),
                                          amount, effective_timestamp,
                                          currency = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  asset <- .asset_require_registered(exchange, symbol = symbol, context = "corporate action asset")
  action_type <- match.arg(action_type)
  if (!is.finite(amount) || (action_type == "split" && amount <= 0)) {
    stop("Corporate action amount must be finite; split ratios must be positive.", call. = FALSE)
  }
  id <- paste0("CA", sprintf("%06d", exchange$next_corporate_action_id))
  exchange$next_corporate_action_id <- exchange$next_corporate_action_id + 1L
  spec <- exchange$assets[asset_id == asset$asset_id]
  row <- data.table::data.table(action_id = id, effective_timestamp = as.POSIXct(effective_timestamp, tz = "UTC"),
    asset_id = asset$asset_id, symbol = asset$symbol, action_type = action_type,
    amount = as.numeric(amount), currency = .profile_currency(exchange, currency %||% spec$quote_ccy[1L]),
    status = "pending", message = "Awaiting the next eligible market step.")
  exchange$corporate_actions <- data.table::rbindlist(list(exchange$corporate_actions, row), fill = TRUE)
  invisible(id)
}

#' Register a futures expiry or contract roll
#'
#' Expiry settles the old contract's marked P&L into settled quote-currency
#' cash and terminates its open margin position. A roll additionally opens the
#' same signed quantity in a registered successor contract at `roll_price`.
#' The successor must not already have an open margin position for an affected
#' account; callers should make any independent successor adjustment first.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param symbol Expiring registered futures symbol.
#' @param effective_timestamp Lifecycle boundary.
#' @param settlement_price Cash-settlement price for the expiring contract.
#' @param successor_symbol Optional registered successor futures symbol.
#' @param roll_price Required successor reference price when rolling.
#' @return Invisibly returns the corporate action id.
#' @export
sim_exchange_future_roll <- function(exchange, symbol, effective_timestamp,
                                     settlement_price, successor_symbol = NULL,
                                     roll_price = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  asset <- .asset_require_registered(exchange, symbol = symbol, context = "futures lifecycle asset")
  spec <- exchange$assets[asset_id == asset$asset_id]
  if (!identical(spec$instrument_profile[1L], "future")) {
    stop("Futures expiry/roll requires an asset with `instrument_profile = 'future'`.", call. = FALSE)
  }
  if (!is.finite(settlement_price) || settlement_price <= 0) {
    stop("`settlement_price` must be positive and finite.", call. = FALSE)
  }
  successor <- NULL
  if (!is.null(successor_symbol)) {
    successor <- .asset_require_registered(exchange, symbol = successor_symbol, context = "futures roll successor")
    successor_spec <- exchange$assets[asset_id == successor$asset_id]
    if (!identical(successor_spec$instrument_profile[1L], "future") || !is.finite(roll_price) || roll_price <= 0) {
      stop("A futures successor and positive finite `roll_price` are required for a roll.", call. = FALSE)
    }
  }
  action_id <- sim_exchange_corporate_action(exchange, symbol,
    action_type = if (is.null(successor)) "future_expiry" else "future_roll",
    amount = settlement_price, effective_timestamp = effective_timestamp,
    currency = spec$quote_ccy[1L])
  index <- match(action_id, exchange$corporate_actions$action_id)
  exchange$corporate_actions[index, `:=`(
    successor_asset_id = if (is.null(successor)) NA_integer_ else successor$asset_id,
    successor_symbol = if (is.null(successor)) NA_character_ else successor$symbol,
    successor_price = if (is.null(successor)) NA_real_ else as.numeric(roll_price)
  )]
  invisible(action_id)
}

#' Settle due profile-aware cash movements
#'
#' @param exchange A `tradesimr_exchange`.
#' @param timestamp Settlement cutoff.
#' @return A data.table of settled ledger rows.
#' @export
sim_exchange_settle <- function(exchange, timestamp = Sys.time()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  .profile_settle_due(exchange, as.POSIXct(timestamp, tz = "UTC"))
}

#' Submit spot target weights for next-bar execution
#'
#' This is the inventory-accounting counterpart to the derivatives portfolio
#' target API. It plans all supplied spot legs from one completed boundary and
#' emits only next-eligible explicit inventory orders.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Account identifier.
#' @param bars Completed registered spot bars at one timestamp.
#' @param target_weights Named target weights keyed by symbols.
#' @param fee_rt Non-negative execution fee rate.
#' @return A list of accepted orders and planned target quantities.
#' @export
sim_spot_target_submit <- function(exchange, agent_id, bars, target_weights, fee_rt = 0) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (!is.numeric(target_weights) || is.null(names(target_weights)) || any(!is.finite(target_weights))) {
    stop("`target_weights` must be a named finite numeric vector.", call. = FALSE)
  }
  if (any(target_weights < 0) || sum(target_weights) > 1 + 1e-10 || !is.finite(fee_rt) || fee_rt < 0) {
    stop("Spot target weights must be non-negative, sum to at most one, and use non-negative fees.", call. = FALSE)
  }
  bars <- .portfolio_validate_decision_bars(exchange, bars)
  .portfolio_require_one_timestamp(bars)
  agent_id <- as.character(agent_id)
  .ensure_shared_account(exchange, agent_id)
  targets <- data.table::data.table(symbol = names(target_weights), target_weight = as.numeric(target_weights))
  targets <- merge(targets, exchange$assets, by = "symbol", all.x = TRUE, sort = FALSE)
  if (anyNA(targets$asset_id) || any(!vapply(targets$asset_id, function(id) .asset_uses_spot_inventory(exchange, id), logical(1L)))) {
    stop("Spot targets require registered inventory-profile assets.", call. = FALSE)
  }
  marks <- bars[, .(symbol, timestamp, close)]
  targets <- merge(targets, marks, by = "symbol", all.x = TRUE, sort = FALSE)
  if (any(!is.finite(targets$close))) stop("Every target requires a completed decision bar.", call. = FALSE)
  equity <- .profile_agent_equity(exchange, agent_id)
  plans <- list()
  for (i in seq_len(nrow(targets))) {
    row <- targets[i]
    .ensure_spot_account(exchange, agent_id, row$asset_id, row$symbol)
    state <- exchange$spot_states[[.agent_state_key(agent_id, row$asset_id)]]
    px_base <- .profile_to_base(exchange, row$close * row$contract_size, .profile_currency(exchange, row$quote_ccy))
    target_units <- floor((equity * row$target_weight / (px_base * (1 + fee_rt))) / row$qty_step + 1e-10) * row$qty_step
    delta <- target_units - as.numeric(state$units %||% 0)
    plans[[i]] <- data.table::data.table(symbol = row$symbol, asset_id = row$asset_id,
      timestamp = row$timestamp, target_weight = row$target_weight,
      target_units = target_units, current_units = as.numeric(state$units %||% 0), delta_units = delta)
  }
  plan <- data.table::rbindlist(plans)
  orders <- character()
  for (i in seq_len(nrow(plan[abs(delta_units) > 1e-12]))) {
    row <- plan[abs(delta_units) > 1e-12][i]
    id <- sim_exchange_place_order(exchange, agent_id, row$timestamp, symbol = row$symbol,
      side = if (row$delta_units > 0) "buy" else "sell", qty = abs(row$delta_units))
    idx <- match(id, exchange$agent_orders$order_id)
    data.table::set(exchange$agent_orders, i = idx, j = "eligible_after", value = row$timestamp)
    orders <- c(orders, id)
  }
  list(orders = data.table::copy(exchange$agent_orders[order_id %in% orders]), targets = plan,
    status = if (length(orders)) "accepted" else "no_op")
}

.profile_base_currency <- function(exchange) toupper(as.character(exchange$config$base_currency %||% "USD"))
.profile_utc_timestamp <- function(timestamp) {
  structure(as.POSIXct(as.numeric(timestamp), origin = "1970-01-01", tz = "UTC"), tzone = "UTC")
}
.profile_currency <- function(exchange, currency = NULL) {
  value <- toupper(as.character(currency %||% .profile_base_currency(exchange)))
  if (is.na(value) || !nzchar(value)) .profile_base_currency(exchange) else value
}
.profile_cash_key <- function(agent_id, currency) paste(as.character(agent_id), toupper(as.character(currency)), sep = "\r")
.profile_typed_cash_row <- function(exchange, agent_id, currency) {
  balances <- exchange$cash_balances %||% sim_schemas()$cash_balances[0]
  requested_agent_id <- as.character(agent_id)
  requested_currency <- .profile_currency(exchange, currency)
  # `which()` avoids creating a secondary data.table index every time a
  # profile-aware balance is read from the hot execution path.
  balances[which(agent_id == requested_agent_id & currency == requested_currency)]
}
.profile_typed_cash_upsert <- function(exchange, agent_id, currency, settled = NULL,
                                       unsettled = NULL, timestamp = Sys.time()) {
  agent_id <- as.character(agent_id)
  currency <- .profile_currency(exchange, currency)
  if (is.null(exchange$cash_balances)) exchange$cash_balances <- sim_schemas()$cash_balances[0]
  existing <- .profile_typed_cash_row(exchange, agent_id, currency)
  settled <- as.numeric(settled %||% if (nrow(existing)) existing$settled[1L] else 0)
  unsettled <- as.numeric(unsettled %||% if (nrow(existing)) existing$unsettled[1L] else 0)
  row <- data.table::data.table(agent_id = agent_id, currency = currency, settled = settled,
    unsettled = unsettled, timestamp = .profile_utc_timestamp(timestamp))
  index <- which(exchange$cash_balances$agent_id == agent_id & exchange$cash_balances$currency == currency)
  if (length(index)) {
    exchange$cash_balances <- exchange$cash_balances[-index]
  }
  exchange$cash_balances <- data.table::rbindlist(list(exchange$cash_balances, row), fill = TRUE)
  data.table::set(exchange$cash_balances, j = "timestamp",
    value = .profile_utc_timestamp(exchange$cash_balances$timestamp))
  invisible(row)
}
.profile_project_typed_cash_balances <- function(exchange) {
  balances <- exchange$cash_balances %||% sim_schemas()$cash_balances[0]
  if (!nrow(balances)) return(invisible(NULL))
  for (i in seq_len(nrow(balances))) {
    row <- balances[i]
    .ensure_shared_account(exchange, row$agent_id)
    if (identical(row$currency, .profile_base_currency(exchange))) {
      exchange$agent_accounts[[row$agent_id]]$cash <- as.numeric(row$settled)
    } else {
      exchange$currency_cash[[.profile_cash_key(row$agent_id, row$currency)]] <- as.numeric(row$settled)
    }
  }
  invisible(NULL)
}
.profile_unsettled_cash <- function(exchange, agent_id, currency) {
  row <- .profile_typed_cash_row(exchange, agent_id, currency)
  if (nrow(row)) as.numeric(row$unsettled[1L]) else 0
}
.profile_cash_balance <- function(exchange, agent_id, currency) {
  currency <- .profile_currency(exchange, currency)
  if (identical(currency, .profile_base_currency(exchange))) return(.shared_cash(exchange, agent_id))
  typed <- .profile_typed_cash_row(exchange, agent_id, currency)
  if (nrow(typed)) return(as.numeric(typed$settled[1L]))
  as.numeric(exchange$currency_cash[[.profile_cash_key(agent_id, currency)]] %||% 0)
}
.profile_set_cash_balance <- function(exchange, agent_id, currency, amount) {
  .ensure_shared_account(exchange, agent_id)
  currency <- .profile_currency(exchange, currency)
  agent_id <- as.character(agent_id)
  .profile_typed_cash_upsert(exchange, agent_id, currency, settled = as.numeric(amount))
  if (identical(currency, .profile_base_currency(exchange))) {
    exchange$agent_accounts[[agent_id]]$cash <- as.numeric(amount)
  } else {
    exchange$currency_cash[[.profile_cash_key(agent_id, currency)]] <- as.numeric(amount)
  }
  invisible(amount)
}
.profile_fx_rate <- function(exchange, from_ccy, to_ccy = .profile_base_currency(exchange)) {
  source_ccy <- .profile_currency(exchange, from_ccy); destination_ccy <- .profile_currency(exchange, to_ccy)
  if (identical(source_ccy, destination_ccy)) return(1)
  direct <- exchange$fx_rates[from_ccy == source_ccy & to_ccy == destination_ccy]
  if (nrow(direct)) return(tail(direct$rate, 1L))
  inverse <- exchange$fx_rates[from_ccy == destination_ccy & to_ccy == source_ccy]
  if (nrow(inverse)) return(1 / tail(inverse$rate, 1L))
  stop("No FX conversion rate is available for ", source_ccy, "/", destination_ccy, ".", call. = FALSE)
}
.profile_to_base <- function(exchange, amount, currency) as.numeric(amount) * .profile_fx_rate(exchange, currency)
.profile_record_cash <- function(exchange, timestamp, agent_id, currency, amount, balance, event_type, asset_id = NA_integer_, symbol = NA_character_, order_id = NA_character_, settlement_id = NA_character_, message = NA_character_) {
  id <- paste0("LED", sprintf("%06d", exchange$next_ledger_id)); exchange$next_ledger_id <- exchange$next_ledger_id + 1L
  exchange$profile_cash_ledger <- data.table::rbindlist(list(exchange$profile_cash_ledger, data.table::data.table(
    entry_id = id, timestamp = as.POSIXct(timestamp, tz = "UTC"), agent_id = as.character(agent_id), currency = .profile_currency(exchange, currency), amount = as.numeric(amount), balance_after = as.numeric(balance), event_type = event_type, asset_id = as.integer(asset_id), symbol = as.character(symbol), order_id = as.character(order_id), settlement_id = as.character(settlement_id), message = as.character(message)
  )), fill = TRUE)
  invisible(id)
}

#' @keywords internal
.profile_carry_cursor <- function(exchange, agent_id, asset_id, currency, carry_type, timestamp) {
  asset_filter <- if (is.na(asset_id)) is.na(exchange$carry_accruals$asset_id) else exchange$carry_accruals$asset_id == as.integer(asset_id)
  cursor <- exchange$carry_accruals[
    agent_id == as.character(agent_id) & asset_filter & currency == as.character(currency) &
      carry_type == as.character(carry_type)
  ]
  if (!nrow(cursor)) {
    row <- data.table::data.table(agent_id = as.character(agent_id), asset_id = as.integer(asset_id),
      currency = as.character(currency), carry_type = as.character(carry_type),
      last_timestamp = as.POSIXct(timestamp, tz = "UTC"))
    exchange$carry_accruals <- data.table::rbindlist(list(exchange$carry_accruals, row), fill = TRUE)
    return(as.POSIXct(NA, tz = "UTC"))
  }
  cursor$last_timestamp[1L]
}

#' @keywords internal
.profile_set_carry_cursor <- function(exchange, agent_id, asset_id, currency, carry_type, timestamp) {
  asset_filter <- if (is.na(asset_id)) is.na(exchange$carry_accruals$asset_id) else exchange$carry_accruals$asset_id == as.integer(asset_id)
  index <- which(exchange$carry_accruals$agent_id == as.character(agent_id) &
    asset_filter &
    exchange$carry_accruals$currency == as.character(currency) &
    exchange$carry_accruals$carry_type == as.character(carry_type))
  if (length(index)) data.table::set(exchange$carry_accruals, i = index[1L], j = "last_timestamp",
    value = as.POSIXct(timestamp, tz = "UTC"))
  invisible(NULL)
}

#' @keywords internal
.profile_record_account_event <- function(exchange, timestamp, agent_id, event_type, asset_id,
                                          symbol, currency, amount, message) {
  event <- data.table::data.table(
    account_event_id = paste0("AE", sprintf("%06d", exchange$next_account_event_id)),
    timestamp = as.POSIXct(timestamp, tz = "UTC"), agent_id = as.character(agent_id),
    event_type = as.character(event_type), asset_id = as.integer(asset_id), symbol = as.character(symbol),
    currency = as.character(currency), amount = as.numeric(amount), order_id = NA_character_,
    fill_id = NA_character_, atomic_group_id = NA_character_, message = as.character(message)
  )
  exchange$next_account_event_id <- exchange$next_account_event_id + 1L
  exchange$account_events <- data.table::rbindlist(list(exchange$account_events, event), fill = TRUE)
  invisible(event)
}

#' @keywords internal
.profile_accrue_carry <- function(exchange, timestamp) {
  timestamp <- .profile_utc_timestamp(timestamp)
  events <- list()
  cash_rates <- exchange$config$cash_interest_rates %||% numeric()
  borrow_rates <- exchange$config$borrow_rates %||% numeric()
  accrue <- function(agent_id, asset_id, symbol, currency, carry_type, annual_rate, base_amount) {
    if (!is.finite(annual_rate) || annual_rate == 0 || !is.finite(base_amount) || base_amount == 0) return()
    previous <- .profile_carry_cursor(exchange, agent_id, asset_id, currency, carry_type, timestamp)
    if (!is.na(previous)) {
      days <- as.numeric(difftime(timestamp, previous, units = "days"))
      if (days > 0) {
        amount <- base_amount * annual_rate * days / 365
        balance <- .profile_cash_balance(exchange, agent_id, currency) + amount
        .profile_set_cash_balance(exchange, agent_id, currency, balance)
        .profile_record_cash(exchange, timestamp, agent_id, currency, amount, balance, carry_type,
          asset_id, symbol, message = if (carry_type == "borrow_fee") "Short inventory borrow accrued." else "Settled cash interest accrued.")
        events[[length(events) + 1L]] <<- .profile_record_account_event(exchange, timestamp, agent_id,
          carry_type, asset_id, symbol, currency, amount,
          if (carry_type == "borrow_fee") "Short inventory borrow accrued." else "Settled cash interest accrued.")
      }
    }
    .profile_set_carry_cursor(exchange, agent_id, asset_id, currency, carry_type, timestamp)
  }
  cash <- exchange$cash_balances
  for (i in seq_len(nrow(cash))) {
    currency_key <- toupper(cash$currency[i])
    rate <- if (currency_key %in% names(cash_rates)) cash_rates[[currency_key]] else 0
    accrue(cash$agent_id[i], NA_integer_, NA_character_, cash$currency[i], "cash_interest", rate, cash$settled[i])
  }
  inventory <- exchange$inventory_positions[units < -1e-12]
  for (i in seq_len(nrow(inventory))) {
    symbol_key <- toupper(inventory$symbol[i])
    rate <- if (symbol_key %in% names(borrow_rates)) borrow_rates[[symbol_key]] else 0
    notional <- -abs(inventory$units[i] * inventory$last_price[i] * inventory$contract_size[i])
    accrue(inventory$agent_id[i], inventory$asset_id[i], inventory$symbol[i], inventory$currency[i], "borrow_fee", rate, notional)
  }
  data.table::rbindlist(events, fill = TRUE)
}
.profile_agent_equity <- function(exchange, agent_id) {
  balances <- sim_exchange_cash_balances(exchange, agent_id)
  cash <- balances$total_base_value %||% balances$base_value
  spot <- .agent_position_snapshots(exchange, agent_id, Sys.time())
  inventory <- if (nrow(spot)) sum(spot[accounting_model == "spot_inventory", notional], na.rm = TRUE) else 0
  sum(cash, na.rm = TRUE) + inventory
}

.profile_settle_due <- function(exchange, timestamp) {
  due <- exchange$settlement_ledger[status == "pending" & due_timestamp <= timestamp]
  if (!nrow(due)) return(exchange$settlement_ledger[0])
  for (i in seq_len(nrow(due))) {
    row <- due[i]
    balance <- .profile_cash_balance(exchange, row$agent_id, row$currency) + row$amount
    .profile_set_cash_balance(exchange, row$agent_id, row$currency, balance)
    .profile_typed_cash_upsert(exchange, row$agent_id, row$currency,
      unsettled = .profile_unsettled_cash(exchange, row$agent_id, row$currency) - row$amount,
      timestamp = timestamp)
    state_key <- .agent_state_key(row$agent_id, row$asset_id)
    if (!is.null(exchange$spot_states[[state_key]])) {
      exchange$spot_states[[state_key]]$unsettled_cash <- as.numeric(exchange$spot_states[[state_key]]$unsettled_cash %||% 0) - as.numeric(row$amount)
    }
    .profile_record_cash(exchange, timestamp, row$agent_id, row$currency, row$amount, balance,
      "settlement", row$asset_id, row$symbol, row$order_id, row$settlement_id,
      "Cash settlement completed.")
    idx <- match(row$settlement_id, exchange$settlement_ledger$settlement_id)
    data.table::set(exchange$settlement_ledger, i = idx, j = "status", value = "settled")
    data.table::set(exchange$settlement_ledger, i = idx, j = "settled_timestamp", value = timestamp)
  }
  due[]
}

.profile_record_settlement <- function(exchange, timestamp, agent_id, currency, amount,
                                       asset_id, symbol, order_id, lag_days, message) {
  id <- paste0("SET", sprintf("%06d", exchange$next_settlement_id))
  exchange$next_settlement_id <- exchange$next_settlement_id + 1L
  requested_asset_id <- as.integer(asset_id)
  asset <- exchange$assets[asset_id == requested_asset_id]
  calendar_id <- if (nrow(asset)) asset$calendar_id[1L] else "ALWAYS_OPEN"
  due_timestamp <- sim_calendar_settlement_timestamp(calendar_id, timestamp, lag_days,
    exceptions = .sim_calendar_exceptions_for_asset(exchange, asset_id))
  row <- data.table::data.table(settlement_id = id, trade_timestamp = timestamp,
    due_timestamp = due_timestamp,
    settled_timestamp = as.POSIXct(NA, tz = "UTC"), agent_id = as.character(agent_id),
    currency = .profile_currency(exchange, currency), amount = as.numeric(amount),
    asset_id = as.integer(asset_id), symbol = as.character(symbol), order_id = as.character(order_id),
    status = "pending", message = as.character(message))
  exchange$settlement_ledger <- data.table::rbindlist(list(exchange$settlement_ledger, row), fill = TRUE)
  .profile_typed_cash_upsert(exchange, agent_id, currency,
    unsettled = .profile_unsettled_cash(exchange, agent_id, currency) + as.numeric(amount),
    timestamp = timestamp)
  invisible(id)
}

.profile_apply_corporate_actions <- function(exchange, timestamp, asset_id,
                                             action_types = NULL) {
  requested_asset_id <- as.integer(asset_id)
  actions <- exchange$corporate_actions[status == "pending" & asset_id == requested_asset_id & effective_timestamp <= timestamp]
  if (!is.null(action_types)) actions <- actions[action_type %in% as.character(action_types)]
  if (!nrow(actions)) return(invisible(NULL))
  for (i in seq_len(nrow(actions))) {
    action <- actions[i]
    keys <- names(exchange$spot_states)[vapply(names(exchange$spot_states), function(key) .parse_agent_state_key(key)$asset_id == action$asset_id, logical(1L))]
    for (key in keys) {
      parsed <- .parse_agent_state_key(key)
      state <- exchange$spot_states[[key]]
      if (identical(action$action_type, "delisting")) {
        units <- as.numeric(state$units %||% 0)
        contract_size <- exchange$assets[asset_id == action$asset_id, contract_size][1L] %||% 1
        proceeds <- units * as.numeric(action$amount) * as.numeric(contract_size)
        balance <- .profile_cash_balance(exchange, parsed$agent_id, action$currency) + proceeds
        .profile_set_cash_balance(exchange, parsed$agent_id, action$currency, balance)
        .profile_record_cash(exchange, timestamp, parsed$agent_id, action$currency, proceeds,
          balance, "delisting", action$asset_id, action$symbol,
          message = "Delisted inventory settled at the registered cash price.")
        state$units <- 0
        state$avg_cost <- NA_real_
        state$last_price <- as.numeric(action$amount)
        state$accrued_interest <- 0
        exchange$spot_states[[key]] <- state
        position_idx <- which(exchange$inventory_positions$agent_id == parsed$agent_id &
          exchange$inventory_positions$asset_id == action$asset_id)
        if (length(position_idx)) {
          data.table::set(exchange$inventory_positions, i = position_idx, j = "units", value = 0)
          data.table::set(exchange$inventory_positions, i = position_idx, j = "average_cost", value = NA_real_)
          data.table::set(exchange$inventory_positions, i = position_idx, j = "last_price", value = as.numeric(action$amount))
          data.table::set(exchange$inventory_positions, i = position_idx, j = "accrued_interest", value = 0)
          data.table::set(exchange$inventory_positions, i = position_idx, j = "timestamp", value = as.POSIXct(timestamp, tz = "UTC"))
        }
        event <- data.table::data.table(
          account_event_id = paste0("AE", sprintf("%06d", exchange$next_account_event_id)),
          timestamp = as.POSIXct(timestamp, tz = "UTC"), agent_id = parsed$agent_id,
          event_type = "delisting", asset_id = as.integer(action$asset_id), symbol = action$symbol,
          currency = action$currency, amount = proceeds, order_id = NA_character_,
          fill_id = NA_character_, atomic_group_id = NA_character_,
          message = "Delisted inventory settled at the registered cash price."
        )
        exchange$next_account_event_id <- exchange$next_account_event_id + 1L
        exchange$account_events <- data.table::rbindlist(list(exchange$account_events, event), fill = TRUE)
        next
      }
      state$cash <- .profile_cash_balance(exchange, parsed$agent_id, action$currency)
      updated <- sim_spot_step(state, close = state$last_price %||% 1,
        contract_size = 1,
        dividend_per_unit = if (action$action_type %in% c("dividend", "coupon", "bond_accrual")) action$amount else 0,
        split_ratio = if (action$action_type == "split") action$amount else 1)
      updated$currency <- state$currency
      updated$unsettled_cash <- state$unsettled_cash %||% 0
      exchange$spot_states[[key]] <- updated
      .profile_set_cash_balance(exchange, parsed$agent_id, action$currency, updated$cash)
      if (updated$dividend_cash != 0) .profile_record_cash(exchange, timestamp, parsed$agent_id, action$currency,
        updated$dividend_cash, updated$cash, action$action_type, action$asset_id, action$symbol,
        message = "Corporate action cash booked.")
    }
    if (identical(action$action_type, "delisting")) {
      order_ids <- exchange$agent_orders[
        status == "accepted" & asset_id == action$asset_id, order_id
      ]
      for (order_id in order_ids) {
        .spot_mark_order_terminal(exchange, order_id, "cancelled", "asset_delisted",
          "Order cancelled because the asset was delisted.", timestamp)
      }
      asset_idx <- which(exchange$assets$asset_id == action$asset_id)
      if (length(asset_idx)) data.table::set(exchange$assets, i = asset_idx, j = "status", value = "delisted")
    }
    idx <- match(action$action_id, exchange$corporate_actions$action_id)
    data.table::set(exchange$corporate_actions, i = idx, j = "status", value = "applied")
    data.table::set(exchange$corporate_actions, i = idx, j = "message", value = "Applied to eligible inventory accounts.")
  }
  invisible(NULL)
}

#' @keywords internal
.profile_apply_future_lifecycle <- function(exchange, timestamp, asset_id) {
  timestamp <- .profile_utc_timestamp(timestamp)
  action_rows <- exchange$corporate_actions[
    status == "pending" & asset_id == as.integer(asset_id) &
      action_type %in% c("future_expiry", "future_roll") & effective_timestamp <= timestamp
  ]
  if (!nrow(action_rows)) return(invisible(NULL))
  for (i in seq_len(nrow(action_rows))) {
    action <- action_rows[i]
    positions <- exchange$typed_margin_positions[asset_id == action$asset_id & abs(signed_units) > 1e-12]
    for (j in seq_len(nrow(positions))) {
      position <- positions[j]
      pnl <- (as.numeric(action$amount) - position$settlement_price) * position$signed_units * position$contract_size
      balance <- .profile_cash_balance(exchange, position$agent_id, position$currency) + pnl
      .profile_set_cash_balance(exchange, position$agent_id, position$currency, balance)
      .profile_record_cash(exchange, timestamp, position$agent_id, position$currency, pnl, balance,
        "future_expiry", action$asset_id, action$symbol,
        message = "Futures expiry marked P&L settled into cash.")
      old_index <- which(exchange$typed_margin_positions$agent_id == position$agent_id &
        exchange$typed_margin_positions$asset_id == action$asset_id)
      data.table::set(exchange$typed_margin_positions, i = old_index, j = "signed_units", value = 0)
      data.table::set(exchange$typed_margin_positions, i = old_index, j = "settlement_price", value = as.numeric(action$amount))
      data.table::set(exchange$typed_margin_positions, i = old_index, j = "last_price", value = as.numeric(action$amount))
      data.table::set(exchange$typed_margin_positions, i = old_index, j = "timestamp", value = as.POSIXct(timestamp, tz = "UTC"))
      legacy_index <- which(exchange$margin_positions$agent_id == position$agent_id &
        exchange$margin_positions$asset_id == action$asset_id)
      if (length(legacy_index)) data.table::set(exchange$margin_positions, i = legacy_index, j = "signed_units", value = 0)
      event <- data.table::data.table(
        account_event_id = paste0("AE", sprintf("%06d", exchange$next_account_event_id)),
        timestamp = as.POSIXct(timestamp, tz = "UTC"), agent_id = position$agent_id,
        event_type = "future_expiry", asset_id = as.integer(action$asset_id), symbol = action$symbol,
        currency = position$currency, amount = pnl, order_id = NA_character_, fill_id = NA_character_,
        atomic_group_id = NA_character_, message = "Futures expiry marked P&L settled into cash."
      )
      exchange$next_account_event_id <- exchange$next_account_event_id + 1L
      exchange$account_events <- data.table::rbindlist(list(exchange$account_events, event), fill = TRUE)
      successor_id <- as.integer(action$successor_asset_id %||% NA_integer_)
      if (!is.na(successor_id)) {
        successor <- exchange$assets[asset_id == successor_id]
        successor_position <- exchange$typed_margin_positions[
          agent_id == position$agent_id & asset_id == successor_id & abs(signed_units) > 1e-12
        ]
        if (nrow(successor_position)) stop("Cannot roll into a contract with an existing open margin position.", call. = FALSE)
        row <- data.table::data.table(
          agent_id = position$agent_id, asset_id = successor_id, symbol = successor$symbol[1L],
          currency = .profile_currency(exchange, successor$quote_ccy[1L]), signed_units = position$signed_units,
          settlement_price = as.numeric(action$successor_price), last_price = as.numeric(action$successor_price),
          contract_size = successor$contract_size[1L], maintenance_rate = position$maintenance_rate,
          timestamp = as.POSIXct(timestamp, tz = "UTC")
        )
        exchange$typed_margin_positions <- data.table::rbindlist(list(exchange$typed_margin_positions, row), fill = TRUE)
        roll_event <- data.table::copy(event)
        roll_event[, `:=`(account_event_id = paste0("AE", sprintf("%06d", exchange$next_account_event_id)),
          event_type = "future_roll", asset_id = successor_id, symbol = successor$symbol[1L], amount = 0,
          message = "Futures position rolled into the registered successor contract.")]
        exchange$next_account_event_id <- exchange$next_account_event_id + 1L
        exchange$account_events <- data.table::rbindlist(list(exchange$account_events, roll_event), fill = TRUE)
      }
    }
    order_ids <- exchange$agent_orders[status == "accepted" & asset_id == action$asset_id, order_id]
    for (order_id in order_ids) .spot_mark_order_terminal(exchange, order_id, "cancelled", "contract_expired",
      "Order cancelled because the futures contract expired.", timestamp)
    asset_index <- which(exchange$assets$asset_id == action$asset_id)
    if (length(asset_index)) data.table::set(exchange$assets, i = asset_index, j = "status", value = "expired")
    action_index <- match(action$action_id, exchange$corporate_actions$action_id)
    data.table::set(exchange$corporate_actions, i = action_index, j = "status", value = "applied")
    data.table::set(exchange$corporate_actions, i = action_index, j = "message",
      value = if (is.na(action$successor_asset_id %||% NA_integer_)) "Futures contract expired and was cash settled." else "Futures contract expired, was cash settled, and rolled.")
  }
  invisible(NULL)
}
