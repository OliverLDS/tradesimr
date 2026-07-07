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
  expect_true(all(c("timestamp", "equity", "cash", "pos_dir", "notional", "maintenance_margin") %in% names(sim)))
  expect_equal(nrow(sim), nrow(bars))
  expect_true(all(is.finite(sim$equity)))

  orders <- sim_orders(sim)
  expect_s3_class(orders, "data.table")
  expect_true(nrow(orders) > 0)
  expect_true(all(orders$status_label == "filled"))

  events <- sim_events(sim)
  expect_s3_class(events, "data.table")
  expect_gte(nrow(events), nrow(orders))

  expect_s3_class(sim_fills(sim), "data.table")
  expect_s3_class(sim_positions(sim), "data.table")
  expect_s3_class(sim_cash_ledger(sim), "data.table")
  expect_s3_class(sim_account(sim), "data.table")
  expect_s3_class(sim_risk(sim), "data.table")

  metrics <- sim_metrics(sim)
  expect_s3_class(metrics, "data.table")
  expect_equal(metrics$n_events, nrow(events))
  expect_false(metrics$liquidated)
})

test_that("schemas, adapters, export, and exchange replay work", {
  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC") + 0:3 * 86400,
    open = c(100, 101, 102, 103),
    high = c(101, 102, 103, 104),
    low = c(99, 100, 101, 102),
    close = c(100, 102, 103, 104),
    tgt_pos = c(0, 1, 1, 0)
  )

  expect_true("events" %in% names(sim_schemas()))
  expect_s3_class(as_market_bars(bars), "data.table")
  expect_s3_class(as_target_positions(bars), "data.table")

  sim <- sim_backtest(bars, ctr_step = 0.01, lev = 10)
  out_dir <- tempfile()
  paths <- sim_export(sim, out_dir, format = "csv")
  expect_true(all(file.exists(paths)))

  exchange <- sim_exchange_new(list(ctr_step = 0.01, lev = 10))
  sim_exchange_add_bars(exchange, bars[, c("timestamp", "open", "high", "low", "close")])
  order_id <- sim_exchange_place_order(exchange, "agent1", bars$timestamp[2], 1)
  expect_match(order_id, "^ORD")
  replay <- sim_exchange_run(exchange)
  expect_s3_class(replay, "data.table")
  expect_s3_class(sim_exchange_account(exchange), "data.table")
  expect_s3_class(sim_exchange_positions(exchange), "data.table")
})
