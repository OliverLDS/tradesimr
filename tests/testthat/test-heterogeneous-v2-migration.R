test_that("v2 homogeneous inventory boundaries persist typed account state", {
  exchange <- sim_exchange_new(list(cash = 1000, portfolio_margin = TRUE))
  sim_asset_add(exchange, "SPY", asset_id = 1L, instrument_profile = "etf",
    quote_ccy = "USD", qty_step = 1)
  day_1 <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC"), symbol = "SPY", asset_id = 1L,
    open = 100, high = 101, low = 99, close = 100
  )
  sim_portfolio_market_step(exchange, day_1)
  submitted <- sim_portfolio_target_submit(exchange, "alice", day_1, c(SPY = 0.5),
    allowed_symbols = "SPY")
  sim_portfolio_market_step(exchange, transform(day_1, timestamp = timestamp + 86400))

  expect_identical(exchange$config$execution_engine, "heterogeneous_v2")
  expect_identical(sim_exchange_orders(exchange)[order_id %in% submitted$orders$order_id, status], "filled")
  expect_equal(exchange$inventory_positions[agent_id == "alice" & asset_id == 1L, units], 5)
  expect_equal(sim_exchange_positions(exchange)[agent_id == "alice" & asset_id == 1L, ctr_unit], 5)
  expect_true(nrow(exchange$cash_balances[agent_id == "alice" & currency == "USD"]) == 1L)
})

test_that("legacy-v1 and v2 have a frozen derivative settlement parity baseline", {
  make_exchange <- function(engine) {
    exchange <- suppressWarnings(sim_exchange_new(list(
      cash = 1000, portfolio_margin = TRUE, execution_engine = engine,
      lev = 10, mmr = 0.02, fee_rt = 0.001, fund_rt = 0.0001,
      funding_interval_hours = 8
    )))
    sim_asset_add(exchange, "ES", asset_id = 1L, instrument_profile = "future",
      quote_ccy = "USD", contract_size = 10, qty_step = 0.25)
    exchange
  }
  bars <- function(timestamp, price) data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"), symbol = "ES", asset_id = 1L,
    open = price, high = price + 1, low = price - 1, close = price
  )
  run <- function(engine) {
    exchange <- make_exchange(engine)
    first <- bars("2026-01-01", 100)
    sim_portfolio_market_step(exchange, first)
    sim_portfolio_target_submit(exchange, "trader", first, c(ES = 0.5),
      allowed_symbols = "ES")
    sim_portfolio_market_step(exchange, bars("2026-01-02", 110))
    sim_portfolio_market_step(exchange, bars("2026-01-03", 105))
    list(
      order = sim_exchange_orders(exchange)[, .(status, reason_code, qty, fee, price)],
      fills = exchange$portfolio_fills[, .(order_id, status, qty, price, fee, realized_pnl)],
      state = if (identical(engine, "heterogeneous_v2")) {
        exchange$typed_margin_positions[, .(asset_id, signed_units, settlement_price, last_price)]
      } else {
        exchange$margin_positions[, .(asset_id, signed_units, settlement_price, last_price)]
      },
      cash = exchange$cash_balances[, .(currency, settled, unsettled)],
      events = exchange$profile_cash_ledger[event_type %in% c("funding", "variation_margin"),
        .(event_type, amount)],
      account = sim_exchange_account(exchange)[agent_id == "trader", .(equity, maintenance_margin)]
    )
  }
  legacy <- run("legacy_v1")
  typed <- run("heterogeneous_v2")

  expect_equal(typed$order, legacy$order)
  expect_equal(typed$fills, legacy$fills)
  expect_equal(typed$state, legacy$state)
  expect_equal(typed$cash, legacy$cash)
  expect_equal(typed$events, legacy$events)
  expect_equal(typed$account, legacy$account)
  expect_equal(typed$cash$settled, 974.8425)
  expect_equal(typed$state$settlement_price, 105)
})
