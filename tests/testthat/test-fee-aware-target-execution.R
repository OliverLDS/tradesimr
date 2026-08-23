fee_aware_target_bars <- function(targets, open = 100, close = open) {
  n <- length(targets)
  data.frame(
    timestamp = as.POSIXct("2025-01-01", tz = "UTC") + seq_len(n) * 86400,
    open = rep(open, n),
    high = rep(open + 1, n),
    low = rep(open - 1, n),
    close = rep(close, n),
    tgt_pos = targets
  )
}

test_that("a fully long fee-bearing target reserves its opening fee", {
  bars <- data.frame(
    timestamp = as.POSIXct("2025-01-01", tz = "UTC") + 0:3 * 86400,
    open = c(100, 101, 102, 103), high = c(101, 102, 103, 104),
    low = c(99, 100, 101, 102), close = c(100, 102, 101, 104),
    tgt_pos = c(0, 1, 1, 0)
  )
  sim <- sim_backtest(
    bars, init_cash = 100000, ctr_step = 0.001, lev = 1,
    fee_rt = 0.0007, fill_model = "next_open"
  )
  fills <- sim_fills(sim)
  expected_qty <- floor(100000 / (102 * (1 + 0.0007)) / 0.001 + 1e-10) * 0.001
  expected_fee <- expected_qty * 102 * 0.0007

  expect_equal(nrow(fills), 1L)
  expect_equal(fills$ctr_qty, expected_qty)
  expect_equal(fills$price, 102)
  expect_equal(fills$fee, expected_fee, tolerance = 1e-8)
  expect_equal(sim$cash[3], 100000 - expected_fee, tolerance = 1e-8)
  expect_equal(sim$equity[3], 100000 - expected_fee + expected_qty * (101 - 102), tolerance = 1e-8)
  expect_lte(sim$abs_notional[3], sim$equity[3] + 1e-8)
})

test_that("a fully short fee-bearing target reserves its opening fee", {
  sim <- sim_backtest(
    fee_aware_target_bars(c(0, -1, -1)), init_cash = 100000,
    ctr_step = 0.001, lev = 1, fee_rt = 0.0007, fill_model = "next_open"
  )
  fills <- sim_fills(sim)
  expected_qty <- floor(100000 / (100 * (1 + 0.0007)) / 0.001 + 1e-10) * 0.001
  expected_fee <- expected_qty * 100 * 0.0007

  expect_equal(nrow(fills), 1L)
  expect_equal(fills$dir_label, "short")
  expect_equal(fills$ctr_qty, expected_qty)
  expect_equal(fills$fee, expected_fee, tolerance = 1e-8)
  expect_equal(sim$cash[3], 100000 - expected_fee, tolerance = 1e-8)
  expect_lte(sim$abs_notional[3], sim$equity[3] + 1e-8)
})

test_that("fractional targets retain their decision-boundary contract size", {
  sim <- sim_backtest(
    fee_aware_target_bars(c(0, 0.5, 0.5)), init_cash = 100000,
    ctr_step = 0.001, lev = 1, fee_rt = 0.0007, fill_model = "next_open"
  )
  fills <- sim_fills(sim)

  expect_equal(nrow(fills), 1L)
  expect_equal(fills$ctr_qty, 500)
  expect_equal(fills$fee, 35, tolerance = 1e-8)
  expect_equal(sim$cash[3], 99965, tolerance = 1e-8)
  expect_equal(sim$equity[3], 99965, tolerance = 1e-8)
})

test_that("target reversals charge both legs and resize the new opening leg", {
  sim <- sim_backtest(
    fee_aware_target_bars(c(0, 1, -1, -1)), init_cash = 100000,
    ctr_step = 0.001, lev = 1, fee_rt = 0.0007, fill_model = "next_open"
  )
  fills <- sim_fills(sim)
  long_qty <- floor(100000 / (100 * (1 + 0.0007)) / 0.001 + 1e-10) * 0.001
  long_fee <- long_qty * 100 * 0.0007
  cash_after_close <- 100000 - 2 * long_fee
  short_qty <- floor(cash_after_close / (100 * (1 + 0.0007)) / 0.001 + 1e-10) * 0.001
  short_fee <- short_qty * 100 * 0.0007

  expect_equal(fills$action_label, c("open", "close", "open"))
  expect_equal(fills$ctr_qty, c(long_qty, long_qty, short_qty))
  expect_equal(fills$fee, c(long_fee, long_fee, short_fee), tolerance = 1e-8)
  expect_equal(sim$cash[4], cash_after_close - short_fee, tolerance = 1e-8)
  expect_equal(sim$pos_dir[4], -1L)
  expect_lte(sim$abs_notional[4], sim$equity[4] + 1e-8)
})

test_that("target-weight replay uses the same fee-aware next-bar sizing", {
  exchange <- sim_exchange_new(list(cash = 100000, ctr_step = 0.001, lev = 1))
  sim_asset_add(exchange, "SPY", asset_id = 1L, asset_class = "etf", qty_step = 0.001)
  execution <- sim_portfolio_execution(fee_rt = 0.0007, lev = 1)
  first <- data.frame(
    timestamp = as.POSIXct("2025-01-01", tz = "UTC"), symbol = "SPY", asset_id = 1L,
    open = 100, high = 100, low = 100, close = 100
  )
  submitted <- sim_portfolio_target_step(exchange, "vox-a", first, c(SPY = 1), execution)
  second <- first
  second$timestamp <- second$timestamp + 86400
  second[, c("open", "high", "low", "close")] <- 101
  filled <- sim_portfolio_target_step(exchange, "vox-a", second, NULL, execution)
  expected_qty <- floor(100000 / (101 * (1 + 0.0007)) / 0.001 + 1e-10) * 0.001
  expected_fee <- expected_qty * 101 * 0.0007

  expect_equal(submitted$orders$status, "accepted")
  expect_equal(filled$fills$ctr_qty, expected_qty)
  expect_equal(filled$fills$fee, expected_fee, tolerance = 1e-8)
  expect_equal(filled$positions$ctr_unit, expected_qty)
  expect_equal(filled$account$cash, 100000 - expected_fee, tolerance = 1e-8)
  expect_lte(abs(filled$positions$notional), filled$account$equity + 1e-8)
})

test_that("target-weight crypto orders retain asset-level fractional contract steps", {
  exchange <- sim_exchange_new(list(cash = 100000, ctr_step = 1, lev = 1))
  sim_asset_add(exchange, "BTC-USD", asset_id = 1L, asset_class = "crypto_spot", qty_step = 0.001)
  execution <- sim_portfolio_execution(fee_rt = 0, lev = 1)
  first <- data.frame(
    timestamp = as.POSIXct("2026-08-01", tz = "UTC"), symbol = "BTC-USD", asset_id = 1L,
    open = 64055.953125, high = 64055.953125, low = 64055.953125, close = 64055.953125
  )
  submitted <- sim_portfolio_target_step(exchange, "buy_hold--btc-usd", first, c(`BTC-USD` = 1), execution)
  second <- first
  second$timestamp <- second$timestamp + 86400
  second[, c("open", "high", "low", "close")] <- 64054.87890625
  filled <- sim_portfolio_target_step(exchange, "buy_hold--btc-usd", second, NULL, execution)

  expect_equal(submitted$orders$qty, 1.561)
  expect_equal(filled$fills$ctr_qty, 1.561)
  expect_equal(filled$positions$ctr_unit, 1.561)
  expect_equal(filled$fills$price, 64054.87890625)
})
