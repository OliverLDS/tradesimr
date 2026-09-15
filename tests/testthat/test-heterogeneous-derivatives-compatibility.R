test_that("native heterogeneous derivatives route preserves portfolio kernel results", {
  timestamp <- as.POSIXct("2026-09-15 12:00:00", tz = "UTC")
  bars <- data.frame(
    asset_id = c(11L, 22L), timestamp = rep(timestamp, 2L),
    open = c(100, 50), high = c(103, 53), low = c(98, 48), close = c(101, 49)
  )
  states <- list(
    `11` = sim_state(cash = 1000, pos_dir = 1L, ctr_unit = 1.25, avg_price = 99,
      last_px = 100, asset = 11L, action_id_now = 3L,
      old_timestamp = as.numeric(timestamp) - 8 * 3600),
    `22` = sim_state(cash = 1000, pos_dir = -1L, ctr_unit = 2, avg_price = 51,
      last_px = 50, asset = 22L, action_id_now = 4L,
      old_timestamp = as.numeric(timestamp) - 8 * 3600)
  )
  orders <- data.frame(
    order_id = c("open-11", "reduce-22"), asset_id = c(11L, 22L),
    action = c("increase", "reduce"), dir = c("long", "short"),
    order_type = c("market", "market"), ctr_qty = c(0.375, 0.5),
    price = c(NA_real_, NA_real_), strat_id = c(7L, 8L), action_id = c(3L, 4L),
    fee_aware_target = c(TRUE, FALSE)
  )
  normalized <- tradesimr:::.normalize_portfolio_step_orders(orders)
  cov <- matrix(c(0.04, -0.01, -0.01, 0.09), nrow = 2)
  legacy <- tradesimr:::portfolio_step_rcpp(
    states = states,
    bars = data.frame(asset_id = bars$asset_id, timestamp = as.numeric(bars$timestamp),
      open = bars$open, high = bars$high, low = bars$low, close = bars$close),
    orders = normalized, cov = cov, shared_cash = 1000,
    ctr_size = c(1, 1), ctr_step = c(0.125, 0.25), lev = 4,
    fee_rt = 0.001, maker_fee_rt = NA_real_, taker_fee_rt = NA_real_,
    fund_rt = 0.0001, funding_interval_hours = 8, mmr = 0.02,
    portfolio_margin_sigma = 2, portfolio_margin_floor = 0.01,
    old_timestamp = as.numeric(timestamp) - 8 * 3600,
    slippage = 0.0002, spread = 0.0001, rec = TRUE
  )
  bridged <- sim_portfolio_step(
    states = states, bars = bars, orders = orders, cov = cov, shared_cash = 1000,
    ctr_size = c(1, 1), ctr_step = c(0.125, 0.25), lev = 4,
    fee_rt = 0.001, fund_rt = 0.0001, funding_interval_hours = 8, mmr = 0.02,
    portfolio_margin_sigma = 2, portfolio_margin_floor = 0.01,
    slippage = 0.0002, spread = 0.0001, record = TRUE
  )

  expect_equal(bridged$states, legacy$states)
  expect_equal(bridged$cash, legacy$cash)
  expect_equal(bridged$equity, legacy$equity)
  expect_equal(bridged$maintenance_margin, legacy$maintenance_margin)
  expect_equal(bridged$liquidated, legacy$liquidated)
  expect_equal(bridged$events, tradesimr:::.portfolio_kernel_events(legacy$events))

  next_bars <- bars
  next_bars$timestamp <- next_bars$timestamp + 8 * 3600
  next_bars$open <- c(102, 48)
  next_bars$high <- c(104, 49)
  next_bars$low <- c(101, 46)
  next_bars$close <- c(103, 47)
  empty_orders <- tradesimr:::.normalize_portfolio_step_orders(data.frame())
  legacy_next <- tradesimr:::portfolio_step_rcpp(
    states = legacy$states,
    bars = data.frame(asset_id = next_bars$asset_id, timestamp = as.numeric(next_bars$timestamp),
      open = next_bars$open, high = next_bars$high, low = next_bars$low, close = next_bars$close),
    orders = empty_orders, cov = cov, shared_cash = legacy$cash,
    ctr_size = c(1, 1), ctr_step = c(0.125, 0.25), lev = 4,
    fee_rt = 0.001, maker_fee_rt = NA_real_, taker_fee_rt = NA_real_,
    fund_rt = 0.0001, funding_interval_hours = 8, mmr = 0.02,
    portfolio_margin_sigma = 2, portfolio_margin_floor = 0.01,
    old_timestamp = as.numeric(timestamp), slippage = 0.0002, spread = 0.0001,
    rec = TRUE
  )
  bridged_next <- sim_portfolio_step(
    states = bridged$states, bars = next_bars, cov = cov, shared_cash = bridged$cash,
    ctr_size = c(1, 1), ctr_step = c(0.125, 0.25), lev = 4,
    fee_rt = 0.001, fund_rt = 0.0001, funding_interval_hours = 8, mmr = 0.02,
    portfolio_margin_sigma = 2, portfolio_margin_floor = 0.01,
    slippage = 0.0002, spread = 0.0001, record = TRUE
  )
  expect_equal(bridged_next$states, legacy_next$states)
  expect_equal(bridged_next$cash, legacy_next$cash)
  expect_equal(bridged_next$equity, legacy_next$equity)
  expect_equal(bridged_next$maintenance_margin, legacy_next$maintenance_margin)
  expect_equal(bridged_next$liquidated, legacy_next$liquidated)
})

test_that("heterogeneous derivatives route retains fee-aware target clipping", {
  timestamp <- as.POSIXct("2026-09-15", tz = "UTC")
  bars <- data.frame(asset_id = 7L, timestamp = timestamp, open = 100, high = 100, low = 100, close = 100)
  states <- list(`7` = sim_state(cash = 100, asset = 7L, last_px = 100))
  orders <- data.frame(asset_id = 7L, action = "open", dir = "long", order_type = "market",
    ctr_qty = 1, price = NA_real_, strat_id = 0L, action_id = 1L, fee_aware_target = TRUE)
  normalized <- tradesimr:::.normalize_portfolio_step_orders(orders)
  legacy <- tradesimr:::portfolio_step_rcpp(
    states, data.frame(asset_id = 7L, timestamp = as.numeric(timestamp), open = 100, high = 100, low = 100, close = 100),
    normalized, matrix(0, 1, 1), 100, 1, 0.01, 1, 0.01, NA_real_, NA_real_, 0, 8, 0.02,
    0, 0, NA_real_, 0, 0, TRUE
  )
  native <- sim_portfolio_step(
    states, bars, orders, matrix(0, 1, 1), 100, 1, 0.01, 1, 0.01,
    portfolio_margin_sigma = 0, portfolio_margin_floor = 0
  )
  expect_equal(native$states, legacy$states)
  expect_equal(native$cash, legacy$cash)
  expect_equal(native$events, tradesimr:::.portfolio_kernel_events(legacy$events))
  expect_equal(native$cash_balances$settled, native$cash)
  expect_equal(native$margin_positions$signed_units,
    native$states[["7"]]$pos_dir * native$states[["7"]]$ctr_unit)
  expect_true(all(c("fill_id", "order_id", "atomic_group_id", "status", "qty", "fee") %in% names(native$fills)))
  expect_true(all(c("atomic_group_id", "group_status", "committed", "equity", "maintenance_margin") %in% names(native$groups)))
  expect_lt(native$states[["7"]]$ctr_unit, 1)
})
