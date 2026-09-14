test_that("heterogeneous account kernel settles long and short futures variation margin", {
  positions <- data.frame(asset_id = c(1L, 2L), currency = c("USD", "USD"),
    signed_units = c(2, -3), settlement_price = c(100, 100), last_price = c(100, 100),
    contract_size = c(10, 10), maintenance_rate = c(.1, .1))
  bars <- data.frame(asset_id = c(1L, 2L), close = c(110, 90), instrument_profile = c("future", "future"))
  out <- sim_heterogeneous_account_step("USD", data.frame(currency = "USD", settled = 1000, unsettled = 0), data.frame(), positions, bars, data.frame(currency = "USD", rate_to_base = 1), timestamp = as.POSIXct("2025-01-01", tz = "UTC"))
  expect_equal(out$cash_balances$settled, 1500)
  expect_equal(out$margin_positions$settlement_price, c(110, 90))
  expect_equal(out$equity, 1500)
  expect_equal(out$maintenance_margin, 490)
  expect_identical(out$events$event_type, c("variation_margin", "variation_margin"))
})

test_that("heterogeneous account kernel settles non-base futures in native currency", {
  positions <- data.frame(asset_id = 7L, currency = "EUR", signed_units = 2,
    settlement_price = 100, last_price = 100, contract_size = 10,
    maintenance_rate = .1)
  inventory <- data.frame(asset_id = 8L, currency = "EUR", units = 3,
    average_cost = 40, last_price = 40, contract_size = 1)
  bars <- data.frame(asset_id = c(7L, 8L), close = c(110, 50),
    instrument_profile = c("future", "equity"))
  out <- sim_heterogeneous_account_step(
    "USD",
    data.frame(currency = c("USD", "EUR"), settled = c(1000, 500), unsettled = c(0, 0)),
    inventory, positions, bars,
    data.frame(currency = c("USD", "EUR"), rate_to_base = c(1, 1.2)),
    timestamp = as.POSIXct("2025-01-01", tz = "UTC")
  )
  expect_equal(out$cash_balances$settled, c(1000, 700))
  expect_equal(out$margin_positions$settlement_price, 110)
  expect_equal(out$inventory_positions$last_price, 50)
  expect_equal(out$equity, 1990)
  expect_equal(out$maintenance_margin, 264)
  expect_equal(out$events$amount, 200)
  expect_equal(out$events$currency, "EUR")
})

test_that("incremental future exchange books durable variation margin without changing the legacy portfolio kernel", {
  exchange <- sim_exchange_new(list(cash = 1000, lev = 10, mmr = .02))
  sim_asset_add(exchange, "ES", asset_id = 1L, instrument_profile = "future", quote_ccy = "USD", contract_size = 10)
  day1 <- as.POSIXct("2025-01-01", tz = "UTC")
  sim_exchange_place_order(exchange, "trader", day1, symbol = "ES", side = "buy", qty = 1)
  sim_exchange_step(exchange, data.frame(timestamp = day1, symbol = "ES", asset_id = 1L, open = 100, high = 101, low = 99, close = 100))
  sim_exchange_step(exchange, data.frame(timestamp = day1 + 86400, symbol = "ES", asset_id = 1L, open = 110, high = 111, low = 109, close = 110))
  state <- exchange$agent_states[[tradesimr:::.agent_state_key("trader", 1L)]]
  expect_equal(state$cash, 1100)
  expect_equal(state$avg_price, 110)
  expect_true(any(exchange$step_events$event_type_label == "variation_margin"))
  expect_true(any(exchange$profile_cash_ledger$event_type == "variation_margin"))
})

test_that("incremental futures settle variation margin in native currency", {
  exchange <- sim_exchange_new(list(cash = 1000, lev = 10, mmr = .02, base_currency = "USD"))
  sim_asset_add(exchange, "FESX", asset_id = 2L, instrument_profile = "future", quote_ccy = "EUR", contract_size = 10)
  sim_exchange_fx_rate(exchange, "EUR", "USD", 1.2)
  day1 <- as.POSIXct("2025-01-01", tz = "UTC")
  sim_exchange_place_order(exchange, "trader", day1, symbol = "FESX", side = "buy", qty = 1)
  sim_exchange_step(exchange, data.frame(timestamp = day1, symbol = "FESX", asset_id = 2L, open = 100, high = 101, low = 99, close = 100))
  sim_exchange_step(exchange, data.frame(timestamp = day1 + 86400, symbol = "FESX", asset_id = 2L, open = 110, high = 111, low = 109, close = 110))
  balances <- sim_exchange_cash_balances(exchange, "trader")
  expect_equal(balances[currency == "EUR", amount], 100)
  expect_equal(balances[currency == "EUR", base_value], 120)
  expect_equal(tail(exchange$step_snapshots$equity, 1L), 1120)
  expect_equal(tail(exchange$profile_cash_ledger[event_type == "variation_margin", amount], 1L), 100)
})

test_that("heterogeneous account liquidation clears spot and futures together", {
  exchange <- sim_exchange_new(list(cash = 100, lev = 10, mmr = 1, base_currency = "USD", portfolio_margin = TRUE))
  sim_asset_add(exchange, "EQ", asset_id = 3L, instrument_profile = "equity", quote_ccy = "USD")
  sim_asset_add(exchange, "FUT", asset_id = 4L, instrument_profile = "future", quote_ccy = "USD")
  day1 <- as.POSIXct("2025-01-01", tz = "UTC")
  sim_exchange_place_order(exchange, "trader", day1, symbol = "EQ", side = "buy", qty = 1)
  sim_exchange_place_order(exchange, "trader", day1, symbol = "FUT", side = "sell", qty = 1)
  sim_exchange_step(exchange, data.frame(
    timestamp = c(day1, day1), symbol = c("EQ", "FUT"), asset_id = c(3L, 4L),
    open = c(80, 10), high = c(81, 11), low = c(79, 9), close = c(80, 10)
  ))
  sim_exchange_step(exchange, data.frame(
    timestamp = c(day1 + 86400, day1 + 86400), symbol = c("EQ", "FUT"), asset_id = c(3L, 4L),
    open = c(1, 20), high = c(1, 21), low = c(1, 19), close = c(1, 20)
  ))
  expect_true(isTRUE(exchange$agent_accounts[["trader"]]$liquidated))
  expect_equal(exchange$spot_states[[tradesimr:::.agent_state_key("trader", 3L)]]$units, 0)
  expect_equal(exchange$agent_states[[tradesimr:::.agent_state_key("trader", 4L)]]$ctr_unit, 0)
  expect_true(any(exchange$event_log$event == "cross_margin_liquidation"))
})

test_that("portfolio-margin future batches run post-step risk through heterogeneous valuation", {
  exchange <- sim_exchange_new(list(cash = 1000, lev = 10, mmr = .02, portfolio_margin = TRUE))
  sim_asset_add(exchange, "ES", asset_id = 5L, instrument_profile = "future", quote_ccy = "USD", contract_size = 10)
  day1 <- as.POSIXct("2025-01-01", tz = "UTC")
  sim_exchange_place_order(exchange, "trader", day1, symbol = "ES", side = "buy", qty = 1)
  expect_no_error(sim_exchange_step(exchange, data.frame(timestamp = day1, symbol = "ES", asset_id = 5L, open = 100, high = 101, low = 99, close = 100)))
  sim_exchange_step(exchange, data.frame(timestamp = day1 + 86400, symbol = "ES", asset_id = 5L, open = 110, high = 111, low = 109, close = 110))
  expect_false(isTRUE(exchange$agent_accounts[["trader"]]$liquidated))
  expect_true(any(exchange$step_events$event_type_label == "variation_margin"))
})
