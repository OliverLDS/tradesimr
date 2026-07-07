test_that("sim_backtest returns equity, recorder events, and metrics", {
  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC") + 0:5 * 86400,
    open = c(100, 101, 102, 103, 104, 105),
    high = c(101, 102, 103, 104, 105, 106),
    low = c(99, 100, 101, 102, 103, 104),
    close = c(100, 102, 103, 104, 105, 106),
    tgt_pos = c(0, 1, 1, 0, -1, 0)
  )

  sim <- sim_backtest(
    bars,
    init_cash = 10000,
    ctr_step = 0.01,
    lev = 10,
    fee_rt = 0.0005
  )

  expect_s3_class(sim, "data.table")
  expect_named(sim, c("timestamp", "equity"))
  expect_equal(nrow(sim), nrow(bars))
  expect_true(all(is.finite(sim$equity)))

  orders <- sim_orders(sim)
  expect_s3_class(orders, "data.table")
  expect_true(nrow(orders) > 0)
  expect_true(all(orders$status_label == "filled"))

  metrics <- sim_metrics(sim)
  expect_s3_class(metrics, "data.table")
  expect_equal(metrics$n_events, nrow(orders))
  expect_false(metrics$liquidated)
})
