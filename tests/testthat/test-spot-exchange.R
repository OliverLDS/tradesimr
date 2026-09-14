test_that("spot-profile assets use the Rcpp inventory path in incremental exchange stepping", {
  exchange <- sim_exchange_new(list(cash = 1000, fee_rt = 0.01))
  sim_asset_add(exchange, "SPY", asset_id = 1L, instrument_profile = "equity")
  ts <- as.POSIXct("2025-01-01", tz = "UTC")

  sim_exchange_place_order(exchange, "alice", ts, symbol = "SPY", side = "buy", qty = 5)
  sim_exchange_step(exchange, data.frame(
    timestamp = ts, symbol = "SPY", asset_id = 1L,
    open = 100, high = 101, low = 99, close = 100
  ))

  order <- sim_exchange_orders(exchange)
  position <- sim_exchange_positions(exchange)
  account <- sim_exchange_account(exchange)
  expect_identical(order$status, "filled")
  expect_equal(order$price, 100)
  expect_equal(order$fee, 5)
  expect_equal(position$ctr_unit, 5)
  expect_equal(account$cash, 495)
  expect_equal(account$equity, 995)

  sim_exchange_place_order(exchange, "alice", ts + 86400, symbol = "SPY", side = "sell", qty = 2)
  sim_exchange_step(exchange, data.frame(
    timestamp = ts + 86400, symbol = "SPY", asset_id = 1L,
    open = 110, high = 111, low = 109, close = 110
  ))
  position <- sim_exchange_positions(exchange)
  account <- sim_exchange_account(exchange)
  expect_equal(position$ctr_unit, 3)
  expect_equal(account$cash, 495)
  expect_equal(account$equity, 1042.8)
})

test_that("spot exchange state survives save/load and rejects unfunded inventory", {
  exchange <- sim_exchange_new(list(cash = 100, fee_rt = 0))
  sim_asset_add(exchange, "BTC-USD", asset_id = 5L, instrument_profile = "crypto_spot", qty_step = 0.001)
  ts <- as.POSIXct("2025-01-01", tz = "UTC")
  sim_exchange_place_order(exchange, "alice", ts, symbol = "BTC-USD", side = "buy", qty = 2)
  sim_exchange_step(exchange, data.frame(
    timestamp = ts, symbol = "BTC-USD", asset_id = 5L,
    open = 100, high = 101, low = 99, close = 100
  ))
  expect_identical(sim_exchange_orders(exchange)$status, "rejected")
  expect_equal(sim_exchange_positions(exchange)$ctr_unit, 0)

  sim_exchange_place_order(exchange, "alice", ts + 86400, symbol = "BTC-USD", side = "buy", qty = 0.5)
  sim_exchange_step(exchange, data.frame(
    timestamp = ts + 86400, symbol = "BTC-USD", asset_id = 5L,
    open = 100, high = 101, low = 99, close = 100
  ))
  path <- tempfile("tradesimr-spot-")
  sim_exchange_save(exchange, path)
  restored <- sim_exchange_load(path)
  expect_equal(sim_exchange_positions(restored)$ctr_unit, 0.5)
  expect_equal(sim_exchange_account(restored)$cash, 50)
})

test_that("an account may hold spot inventory alongside a derivatives margin state", {
  exchange <- sim_exchange_new(list(cash = 1000))
  sim_asset_add(exchange, "ES", asset_id = 1L, instrument_profile = "future")
  sim_asset_add(exchange, "SPY", asset_id = 2L, instrument_profile = "equity")
  sim_exchange_place_order(exchange, "alice", as.POSIXct("2025-01-01", tz = "UTC"), symbol = "ES", side = "buy", qty = 1)
  expect_no_error(sim_exchange_place_order(exchange, "alice", as.POSIXct("2025-01-01", tz = "UTC"), symbol = "SPY", side = "buy", qty = 1))
})

test_that("portfolio-margin exchange refuses spot assets until its accounting path is implemented", {
  exchange <- sim_exchange_new(list(cash = 1000, portfolio_margin = TRUE))
  sim_asset_add(exchange, "SPY", asset_id = 1L, instrument_profile = "equity")
  expect_error(
    sim_exchange_step(exchange, data.frame(
      timestamp = as.POSIXct("2025-01-01", tz = "UTC"), symbol = "SPY", asset_id = 1L,
      open = 100, high = 101, low = 99, close = 100
    )),
    "does not support spot-inventory"
  )
})
