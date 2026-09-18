skip_on_cran()

test_that("FX cash conversion and non-base spot inventory use durable balances", {
  exchange <- sim_exchange_new(list(cash = 1000, base_currency = "USD"))
  sim_exchange_fx_rate(exchange, "USD", "EUR", 0.8, as.POSIXct("2025-01-01", tz = "UTC"))
  sim_exchange_convert_cash(exchange, "alice", 125, "USD", "EUR")
  sim_asset_add(exchange, "SAP", asset_id = 1L, instrument_profile = "equity", quote_ccy = "EUR")
  sim_exchange_place_order(exchange, "alice", as.POSIXct("2025-01-01", tz = "UTC"), symbol = "SAP", side = "buy", qty = 1)
  sim_exchange_step(exchange, data.frame(timestamp = as.POSIXct("2025-01-01", tz = "UTC"),
    symbol = "SAP", asset_id = 1L, open = 50, high = 51, low = 49, close = 50))
  sim_exchange_step(exchange, data.frame(timestamp = as.POSIXct("2025-01-02", tz = "UTC"),
    symbol = "SAP", asset_id = 1L, open = 50, high = 51, low = 49, close = 50))
  balances <- sim_exchange_cash_balances(exchange, "alice")
  expect_equal(balances[currency == "EUR", amount], 50)
  expect_equal(sim_exchange_positions(exchange)$ctr_unit, 1)
  expect_true(nrow(exchange$profile_cash_ledger) >= 3)
  path <- tempfile("tradesimr-profile-")
  sim_exchange_save(exchange, path)
  restored <- sim_exchange_load(path)
  restored_balances <- sim_exchange_cash_balances(restored, "alice")
  expect_equal(restored_balances[currency == "USD", amount], 875)
  expect_equal(restored_balances[currency == "EUR", amount], 50)
})

test_that("spot sales settle on the profile settlement calendar and corporate actions are durable", {
  exchange <- sim_exchange_new(list(cash = 1000))
  sim_asset_add(exchange, "SPY", asset_id = 1L, instrument_profile = "equity", quote_ccy = "USD", settlement_lag_days = 1)
  day1 <- as.POSIXct("2025-01-01", tz = "UTC")
  sim_exchange_place_order(exchange, "alice", day1, symbol = "SPY", side = "buy", qty = 2)
  sim_exchange_step(exchange, data.frame(timestamp = day1, symbol = "SPY", asset_id = 1L, open = 100, high = 101, low = 99, close = 100))
  sim_exchange_step(exchange, data.frame(timestamp = day1 + 86400, symbol = "SPY", asset_id = 1L, open = 100, high = 101, low = 99, close = 100))
  sim_exchange_place_order(exchange, "alice", day1 + 86400, symbol = "SPY", side = "sell", qty = 1)
  sim_exchange_step(exchange, data.frame(timestamp = day1 + 86400, symbol = "SPY", asset_id = 1L, open = 110, high = 111, low = 109, close = 110))
  sim_exchange_step(exchange, data.frame(timestamp = day1 + 2 * 86400, symbol = "SPY", asset_id = 1L, open = 110, high = 111, low = 109, close = 110))
  expect_equal(sim_exchange_cash_balances(exchange, "alice")$amount, 800)
  expect_identical(exchange$settlement_ledger$status, "pending")
  sim_exchange_corporate_action(exchange, "SPY", "dividend", 2, day1 + 3 * 86400)
  sim_exchange_step(exchange, data.frame(timestamp = day1 + 3 * 86400, symbol = "SPY", asset_id = 1L, open = 110, high = 111, low = 109, close = 110))
  expect_equal(sim_exchange_cash_balances(exchange, "alice")$amount, 912)
  expect_identical(exchange$settlement_ledger$status, "settled")
  expect_identical(exchange$corporate_actions$status, "applied")
})

test_that("delisting settles inventory, cancels pending orders, and survives save/load", {
  exchange <- sim_exchange_new(list(cash = 1000, execution_engine = "heterogeneous_v2"))
  sim_asset_add(exchange, "OLD", asset_id = 71L, instrument_profile = "equity", quote_ccy = "USD")
  day_1 <- as.POSIXct("2025-01-01", tz = "UTC")
  bar <- data.frame(timestamp = day_1, symbol = "OLD", asset_id = 71L,
    open = 100, high = 101, low = 99, close = 100)
  sim_exchange_place_order(exchange, "alice", day_1, symbol = "OLD", side = "buy", qty = 2)
  sim_exchange_step(exchange, bar)
  sim_exchange_step(exchange, transform(bar, timestamp = timestamp + 86400))
  pending <- sim_exchange_place_order(exchange, "alice", day_1 + 86400, symbol = "OLD", side = "buy", qty = 1)
  sim_exchange_corporate_action(exchange, "OLD", "delisting", 120, day_1 + 2 * 86400)
  sim_exchange_step(exchange, transform(bar, timestamp = timestamp + 2 * 86400))

  expect_identical(sim_assets(exchange)[symbol == "OLD", status], "delisted")
  expect_equal(exchange$spot_states[[tradesimr:::.agent_state_key("alice", 71L)]]$units, 0)
  expect_equal(sim_exchange_cash_balances(exchange, "alice")$amount, 1040)
  expect_identical(exchange$agent_orders[order_id == pending, status], "cancelled")
  expect_identical(exchange$agent_orders[order_id == pending, reason_code], "asset_delisted")
  expect_true(any(exchange$account_events$event_type == "delisting"))
  expect_error(sim_exchange_place_order(exchange, "alice", day_1 + 3 * 86400,
    symbol = "OLD", side = "buy", qty = 1), "delisted")

  path <- tempfile("tradesimr-delisting-")
  sim_exchange_save(exchange, path)
  restored <- sim_exchange_load(path)
  expect_identical(sim_assets(restored)[symbol == "OLD", status], "delisted")
  expect_equal(restored$inventory_positions[agent_id == "alice" & asset_id == 71L, units], 0)
  expect_equal(restored$account_events[event_type == "delisting", amount], 240)
})

test_that("futures expiry cash-settles and rolls durable typed margin state", {
  exchange <- sim_exchange_new(list(cash = 0, base_currency = "USD", portfolio_margin = TRUE,
    execution_engine = "heterogeneous_v2"))
  sim_asset_add(exchange, "ESU25", asset_id = 81L, instrument_profile = "future",
    quote_ccy = "USD", contract_size = 1)
  sim_asset_add(exchange, "ESZ25", asset_id = 82L, instrument_profile = "future",
    quote_ccy = "USD", contract_size = 1)
  timestamp <- as.POSIXct("2025-09-19", tz = "UTC")
  sim_exchange_cash_adjust(exchange, "alice", 1000, "USD", timestamp)
  exchange$typed_margin_positions <- data.table::data.table(
    agent_id = "alice", asset_id = 81L, symbol = "ESU25", currency = "USD",
    signed_units = 2, settlement_price = 100, last_price = 100, contract_size = 1,
    maintenance_rate = .02, timestamp = timestamp - 86400
  )
  pending <- sim_exchange_place_order(exchange, "alice", timestamp - 3600,
    symbol = "ESU25", side = "buy", qty = 1)
  action_id <- sim_exchange_future_roll(exchange, "ESU25", timestamp,
    settlement_price = 110, successor_symbol = "ESZ25", roll_price = 115)
  bars <- data.frame(timestamp = c(timestamp, timestamp), symbol = c("ESU25", "ESZ25"),
    asset_id = c(81L, 82L), open = c(110, 115), high = c(110, 115),
    low = c(110, 115), close = c(110, 115))
  sim_exchange_step(exchange, bars)

  expect_identical(sim_assets(exchange)[symbol == "ESU25", status], "expired")
  expect_equal(exchange$typed_margin_positions[agent_id == "alice" & asset_id == 81L, signed_units], 0)
  expect_equal(exchange$typed_margin_positions[agent_id == "alice" & asset_id == 82L, signed_units], 2)
  expect_equal(sim_exchange_cash_balances(exchange, "alice")[currency == "USD", amount], 1020)
  expect_identical(exchange$agent_orders[order_id == pending, reason_code], "contract_expired")
  expect_true(all(c("future_expiry", "future_roll") %in% exchange$account_events$event_type))
  expect_identical(exchange$corporate_actions[action_id == action_id, status], "applied")

  path <- tempfile("tradesimr-future-roll-")
  sim_exchange_save(exchange, path)
  restored <- sim_exchange_load(path)
  restored_action <- restored$corporate_actions[action_id == action_id]
  expect_equal(restored_action$successor_asset_id, 82L)
  expect_equal(restored_action$successor_price, 115)
  expect_equal(restored$typed_margin_positions[agent_id == "alice" & asset_id == 82L, signed_units], 2)
})

test_that("cash interest and short inventory borrow accrue once through save/load", {
  exchange <- sim_exchange_new(list(cash = 0, base_currency = "USD", execution_engine = "heterogeneous_v2"))
  sim_asset_add(exchange, "HARD", asset_id = 91L, instrument_profile = "equity", quote_ccy = "USD")
  start <- as.POSIXct("2025-01-01", tz = "UTC")
  sim_exchange_cash_adjust(exchange, "alice", 1000, "USD", start)
  exchange$inventory_positions <- data.table::data.table(
    agent_id = "alice", asset_id = 91L, symbol = "HARD", currency = "USD",
    units = -1, average_cost = 100, last_price = 100, contract_size = 1,
    accrued_interest = 0, timestamp = start
  )
  sim_exchange_set_carry_rates(exchange, borrow_rates = c(HARD = .365),
    cash_interest_rates = c(USD = .0365))
  sim_exchange_accrue_carry(exchange, start)
  sim_exchange_accrue_carry(exchange, start + 10 * 86400)

  # USD 1,000 earns 1 over ten days, then the 100-dollar short borrow costs 1.
  expect_equal(sim_exchange_cash_balances(exchange, "alice")$amount, 1000)
  expect_true(all(c("cash_interest", "borrow_fee") %in% exchange$account_events$event_type))
  expect_equal(nrow(exchange$carry_accruals), 2L)
  path <- tempfile("tradesimr-carry-")
  sim_exchange_save(exchange, path)
  restored <- sim_exchange_load(path)
  sim_exchange_accrue_carry(restored, start + 10 * 86400)
  expect_equal(sim_exchange_cash_balances(restored, "alice")$amount, 1000)
  sim_exchange_accrue_carry(restored, start + 20 * 86400)
  expect_equal(sim_exchange_cash_balances(restored, "alice")$amount, 1000)
  expect_equal(nrow(restored$carry_accruals), 2L)
})

test_that("crypto perpetual funding reconciles C++ cash to durable typed events", {
  exchange <- sim_exchange_new(list(
    cash = 1000, base_currency = "USD", portfolio_margin = TRUE,
    execution_engine = "heterogeneous_v2", fee_rt = 0,
    fund_rt = .001, funding_interval_hours = 8, lev = 1
  ))
  sim_asset_add(exchange, "BTC-PERP", asset_id = 95L,
    instrument_profile = "crypto_perp", quote_ccy = "USD", qty_step = 1)
  start <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  bar <- function(timestamp) data.frame(
    timestamp = timestamp, symbol = "BTC-PERP", asset_id = 95L,
    open = 100, high = 101, low = 99, close = 100
  )
  sim_portfolio_market_step(exchange, bar(start))
  sim_portfolio_target_submit(exchange, "alice", bar(start), c("BTC-PERP" = .5))
  sim_portfolio_market_step(exchange, bar(start + 8 * 3600))
  sim_portfolio_market_step(exchange, bar(start + 16 * 3600))

  funding_cash <- exchange$profile_cash_ledger[event_type == "funding"]
  funding_events <- exchange$account_events[event_type == "funding"]
  expect_equal(nrow(funding_cash), 1L)
  expect_equal(nrow(funding_events), 1L)
  expect_equal(funding_cash$amount, -.5)
  expect_equal(funding_events$amount, funding_cash$amount)
  expect_equal(sim_exchange_cash_balances(exchange, "alice")$amount, 999.5)
  expect_equal(nrow(exchange$step_events[event_type_label == "funding"]), 1L)

  path <- tempfile("tradesimr-perp-funding-")
  sim_exchange_save(exchange, path)
  restored <- sim_exchange_load(path)
  expect_equal(restored$profile_cash_ledger[event_type == "funding", amount], -.5)
  expect_equal(restored$account_events[event_type == "funding", amount], -.5)
})

test_that("typed cash balances retain settled and unsettled cash through settlement and load", {
  exchange <- sim_exchange_new(list(cash = 1000, base_currency = "USD"))
  sim_asset_add(exchange, "SAP", asset_id = 1L, instrument_profile = "equity",
    quote_ccy = "EUR", settlement_lag_days = 1)
  sim_exchange_fx_rate(exchange, "USD", "EUR", 0.8, as.POSIXct("2025-01-01", tz = "UTC"))
  sim_exchange_convert_cash(exchange, "alice", 250, "USD", "EUR")
  day_1 <- as.POSIXct("2025-01-01", tz = "UTC")
  bar <- data.frame(timestamp = day_1, symbol = "SAP", asset_id = 1L,
    open = 50, high = 51, low = 49, close = 50)
  sim_exchange_place_order(exchange, "alice", day_1, symbol = "SAP", side = "buy", qty = 2)
  sim_exchange_step(exchange, bar)
  sim_exchange_step(exchange, transform(bar, timestamp = timestamp + 86400))
  sim_exchange_place_order(exchange, "alice", day_1 + 86400, symbol = "SAP", side = "sell", qty = 1)
  sim_exchange_step(exchange, transform(bar, timestamp = timestamp + 2 * 86400))

  typed <- exchange$cash_balances[agent_id == "alice" & currency == "EUR"]
  expect_equal(typed$settled, 100)
  expect_equal(typed$unsettled, 50)
  public <- sim_exchange_cash_balances(exchange, "alice")[currency == "EUR"]
  expect_equal(public$amount, 100)
  expect_equal(public$unsettled, 50)
  input <- tradesimr:::.heterogeneous_inventory_account_input(exchange, "alice", bar)
  expect_equal(input$cash_balances[input$cash_balances$currency == "EUR", "settled"], 100)
  expect_equal(input$cash_balances[input$cash_balances$currency == "EUR", "unsettled"], 50)
  kernel <- sim_heterogeneous_account_step(
    base_currency = "USD", cash_balances = input$cash_balances,
    inventory_positions = input$inventory_positions,
    margin_positions = input$margin_positions,
    bars = data.frame(asset_id = 1L, close = 50), fx_rates = input$fx_rates,
    timestamp = day_1 + 2 * 86400
  )
  expect_equal(kernel$cash_balances[kernel$cash_balances$currency == "EUR", "unsettled"], 50)
  expect_equal(kernel$equity, 1000)

  path <- tempfile("tradesimr-typed-cash-")
  sim_exchange_save(exchange, path)
  restored <- sim_exchange_load(path)
  expect_equal(restored$cash_balances[agent_id == "alice" & currency == "EUR", settled], 100)
  expect_equal(restored$cash_balances[agent_id == "alice" & currency == "EUR", unsettled], 50)
  sim_exchange_settle(restored, day_1 + 3 * 86400)
  settled <- restored$cash_balances[agent_id == "alice" & currency == "EUR"]
  expect_equal(settled$settled, 150)
  expect_equal(settled$unsettled, 0)
})

test_that("typed account state projects cash, inventory, and margin in base currency", {
  exchange <- sim_exchange_new(list(cash = 1000, base_currency = "USD", execution_engine = "heterogeneous_v2"))
  sim_exchange_cash_adjust(exchange, "alice", 0)
  timestamp <- as.POSIXct("2025-01-01", tz = "UTC")
  exchange$inventory_positions <- data.table::data.table(
    agent_id = "alice", asset_id = 1L, symbol = "SPY", currency = "USD",
    units = 2, average_cost = 90, last_price = 100, contract_size = 1, timestamp = timestamp
  )
  exchange$typed_margin_positions <- data.table::data.table(
    agent_id = "alice", asset_id = 2L, symbol = "ES", currency = "USD",
    signed_units = -1, settlement_price = 120, last_price = 100, contract_size = 10,
    maintenance_rate = 0.02, timestamp = timestamp
  )
  state <- sim_exchange_account_state(exchange, "alice")
  expect_named(state, c("account", "cash_balances", "inventory_positions", "margin_positions", "events"))
  expect_equal(state$account$cash_settled, 1000)
  expect_equal(state$account$inventory_value, 200)
  expect_equal(state$account$margin_unrealized_pnl, 200)
  expect_equal(state$account$maintenance_margin, 20)
  expect_equal(state$account$equity, 1400)
  expect_equal(state$margin_positions$notional, -1000)

  path <- tempfile("tradesimr-typed-dashboard-")
  paths <- sim_state_dashboard_export(exchange, path)
  expect_true(all(c("typed_account", "cash_balances", "inventory_positions", "margin_positions", "account_events", "bond_schedules") %in% names(paths)))
  exported <- data.table::fread(paths[["typed_account"]])
  expect_equal(exported$equity, 1400)
})

test_that("heterogeneous v2 persists bond coupon events from the C++ account kernel", {
  exchange <- sim_exchange_new(list(
    cash = 1000, base_currency = "USD", portfolio_margin = TRUE,
    execution_engine = "heterogeneous_v2"
  ))
  sim_asset_add(exchange, "BOND", asset_id = 9L, instrument_profile = "bond",
    quote_ccy = "USD", qty_step = 1)
  day_1 <- as.POSIXct("2025-01-01", tz = "UTC")
  bar <- data.frame(timestamp = day_1, symbol = "BOND", asset_id = 9L,
    open = 100, high = 101, low = 99, close = 100)
  sim_portfolio_market_step(exchange, bar)
  sim_spot_target_submit(exchange, "alice", bar, c(BOND = 1))
  sim_portfolio_market_step(exchange, transform(bar, timestamp = timestamp + 86400))
  sim_exchange_corporate_action(exchange, "BOND", "coupon", 2, day_1 + 2 * 86400)
  sim_portfolio_market_step(exchange, transform(bar, timestamp = timestamp + 2 * 86400))

  expect_identical(exchange$corporate_actions$status, "applied")
  expect_equal(exchange$cash_balances[agent_id == "alice" & currency == "USD", settled], 20)
  expect_true(any(exchange$account_events$event_type == "bond_coupon"))
  expect_true(any(exchange$profile_cash_ledger$event_type == "bond_coupon"))
  path <- tempfile("tradesimr-bond-lifecycle-")
  sim_exchange_save(exchange, path)
  restored <- sim_exchange_load(path)
  durable <- function(table) {
    out <- data.table::copy(table)
    for (column in names(out)) if (inherits(out[[column]], "POSIXt")) {
      data.table::set(out, j = column, value = as.numeric(out[[column]]))
    }
    out
  }
  expect_equal(durable(restored$corporate_actions), durable(exchange$corporate_actions))
  restored_event <- durable(restored$account_events[event_type == "bond_coupon"])
  original_event <- durable(exchange$account_events[event_type == "bond_coupon"])
  expect_equal(restored_event[, .(timestamp, agent_id, event_type, asset_id, symbol, currency, amount)],
    original_event[, .(timestamp, agent_id, event_type, asset_id, symbol, currency, amount)])
  expect_true(all(is.na(restored_event$order_id)))
})

test_that("bond schedules survive load and settle coupon then redemption through v2", {
  exchange <- sim_exchange_new(list(cash = 1000, base_currency = "USD", portfolio_margin = TRUE,
    execution_engine = "heterogeneous_v2"))
  sim_asset_add(exchange, "NOTE", asset_id = 10L, instrument_profile = "bond", quote_ccy = "USD")
  issue <- as.POSIXct("2025-01-01", tz = "UTC")
  maturity <- issue + 365 * 86400
  sim_bond_schedule_add(exchange, "NOTE", coupon_rate = .1, coupon_frequency = 2,
    issue_timestamp = issue, maturity_timestamp = maturity, face_value = 100)
  bar <- data.frame(timestamp = issue, symbol = "NOTE", asset_id = 10L,
    open = 100, high = 101, low = 99, close = 100)
  sim_portfolio_market_step(exchange, bar)
  sim_spot_target_submit(exchange, "alice", bar, c(NOTE = 1))
  sim_portfolio_market_step(exchange, transform(bar, timestamp = timestamp + 86400))
  accrual_boundary <- issue + 90 * 86400
  sim_portfolio_market_step(exchange, transform(bar, timestamp = accrual_boundary))
  # The bond was acquired on the next eligible bar, so it accrues from that
  # execution boundary rather than from the issue bar.
  accrued <- 1000 * .1 * 89 / 365
  expect_equal(exchange$inventory_positions[agent_id == "alice" & asset_id == 10L, accrued_interest], accrued)
  expect_equal(sim_exchange_account_state(exchange, "alice")$account$equity, 1000 + accrued)
  path <- tempfile("tradesimr-bond-schedule-")
  sim_exchange_save(exchange, path)
  resumed <- sim_exchange_load(path)
  expect_equal(resumed$inventory_positions[agent_id == "alice" & asset_id == 10L, accrued_interest], accrued)
  coupon_boundary <- issue + 365 * 86400 / 2
  sim_portfolio_market_step(resumed, transform(bar, timestamp = coupon_boundary))
  expect_equal(resumed$cash_balances[agent_id == "alice" & currency == "USD", settled], 50)
  expect_equal(resumed$inventory_positions[agent_id == "alice" & asset_id == 10L, accrued_interest], 0)
  expect_true(any(resumed$account_events$event_type == "bond_accrual"))
  expect_true(any(resumed$profile_cash_ledger$event_type == "bond_coupon"))
  expect_equal(as.numeric(resumed$bond_schedules$next_coupon_timestamp),
    as.numeric(coupon_boundary + 365 * 86400 / 2))
  sim_portfolio_market_step(resumed, transform(bar, timestamp = maturity))
  expect_identical(resumed$bond_schedules$status, "matured")
  expect_equal(resumed$inventory_positions[agent_id == "alice" & asset_id == 10L, units], 0)
  expect_true(any(resumed$account_events$event_type == "redemption"))
})

test_that("v2 bond schedule replay preserves sparse-boundary accrual through load", {
  exchange <- sim_exchange_new(list(cash = 1000, base_currency = "USD", portfolio_margin = TRUE,
    execution_engine = "heterogeneous_v2"))
  sim_asset_add(exchange, "NOTE", asset_id = 11L, instrument_profile = "bond", quote_ccy = "USD")
  issue <- as.POSIXct("2025-01-01", tz = "UTC")
  maturity <- issue + 365 * 86400
  sim_bond_schedule_add(exchange, "NOTE", coupon_rate = .1, coupon_frequency = 2,
    issue_timestamp = issue, maturity_timestamp = maturity, face_value = 100)
  bar <- data.frame(timestamp = issue, symbol = "NOTE", asset_id = 11L,
    open = 100, high = 101, low = 99, close = 100)
  sim_portfolio_market_step(exchange, bar)
  sim_spot_target_submit(exchange, "alice", bar, c(NOTE = 1))
  sim_portfolio_market_step(exchange, transform(bar, timestamp = timestamp + 86400))

  sparse_boundary <- issue + 365 * 86400 * 3 / 4
  sim_portfolio_market_step(exchange, transform(bar, timestamp = sparse_boundary))
  # The fixed schedule pays the contractual first coupon; the remaining
  # quarter-period is carried as accrued interest after that coupon boundary.
  accrued <- 25
  expect_equal(exchange$cash_balances[agent_id == "alice" & currency == "USD", settled], 50)
  expect_equal(exchange$inventory_positions[agent_id == "alice" & asset_id == 11L, accrued_interest], accrued)
  expect_equal(sim_exchange_account_state(exchange, "alice")$account$equity, 1050 + accrued)

  path <- tempfile("tradesimr-sparse-bond-")
  sim_exchange_save(exchange, path)
  resumed <- sim_exchange_load(path)
  expect_equal(resumed$inventory_positions[agent_id == "alice" & asset_id == 11L, accrued_interest], accrued)
  sim_portfolio_market_step(resumed, transform(bar, timestamp = maturity))
  expect_equal(resumed$cash_balances[agent_id == "alice" & currency == "USD", settled], 1100)
  expect_equal(resumed$inventory_positions[agent_id == "alice" & asset_id == 11L, units], 0)
  expect_identical(resumed$bond_schedules$status, "matured")
})

test_that("spot target submission plans atomically and executes only after its decision bar", {
  exchange <- sim_exchange_new(list(cash = 1000))
  sim_asset_add(exchange, "SPY", asset_id = 1L, instrument_profile = "etf", quote_ccy = "USD", qty_step = 1)
  day1 <- data.frame(timestamp = as.POSIXct("2025-01-01", tz = "UTC"), symbol = "SPY", asset_id = 1L, open = 100, high = 101, low = 99, close = 100)
  sim_exchange_step(exchange, day1)
  submitted <- sim_spot_target_submit(exchange, "alice", day1, c(SPY = 1))
  expect_identical(submitted$status, "accepted")
  expect_identical(sim_exchange_orders(exchange)$status, "accepted")
  sim_exchange_step(exchange, transform(day1, timestamp = timestamp + 86400, open = 100, high = 101, low = 99, close = 100))
  expect_equal(sim_exchange_positions(exchange)$ctr_unit, 10)
  expect_identical(sim_exchange_orders(exchange)$status, "filled")
})
