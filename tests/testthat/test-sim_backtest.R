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
  expect_true(file.exists(file.path(out_dir, "manifest.csv")))
  expect_true(nrow(sim_read_manifest(out_dir)) > 0)
  expect_equal(nrow(sim_read_events(out_dir)), nrow(sim_events(sim)))
  expect_equal(nrow(sim_read_account(out_dir)), nrow(sim_account(sim)))
  expect_equal(nrow(sim_run_from_events(out_dir)), nrow(sim))

  exchange <- sim_exchange_new(list(ctr_step = 0.01, lev = 10))
  sim_exchange_add_bars(exchange, bars[, c("timestamp", "open", "high", "low", "close")])
  order_id <- sim_exchange_place_order(exchange, "agent1", bars$timestamp[2], side = "buy", qty = 1, client_order_id = "client-1")
  expect_match(order_id, "^ORD")
  expect_equal(sim_exchange_orders(exchange)$client_order_id[1], "client-1")
  expect_equal(sim_exchange_orders(exchange)$qty_type[1], "contracts")
  replay <- sim_exchange_run(exchange)
  expect_s3_class(replay, "data.table")
  expect_equal(nrow(sim_orders(replay)), 0)
  expect_s3_class(sim_exchange_new_events(exchange), "data.table")
  expect_s3_class(sim_exchange_account(exchange), "data.table")
  expect_s3_class(sim_exchange_positions(exchange), "data.table")

  target_exchange <- sim_exchange_new(list(ctr_step = 0.01, lev = 10))
  sim_exchange_add_bars(target_exchange, bars[, c("timestamp", "open", "high", "low", "close")])
  sim_exchange_place_order(target_exchange, "agent1", bars$timestamp[2], side = "target", tgt_pos = 1)
  expect_equal(sim_exchange_orders(target_exchange)$qty_type[1], "target_pos")
  target_replay <- sim_exchange_run(target_exchange)
  expect_gt(nrow(sim_orders(target_replay)), 0)

  exchange_dir <- tempfile()
  sim_exchange_save(exchange, exchange_dir)
  loaded <- sim_exchange_load(exchange_dir)
  expect_equal(nrow(sim_exchange_orders(loaded)), nrow(sim_exchange_orders(exchange)))
  expect_s3_class(sim_exchange_account(loaded), "data.table")
})

test_that("sim_step processes one bar and carries prior state forward", {
  state <- sim_state(cash = 10000, last_px = 100)
  bar1 <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
    open = 100,
    high = 102,
    low = 99,
    close = 101
  )
  orders <- data.frame(
    action = "open",
    dir = "long",
    order_type = "market",
    ctr_qty = 1,
    price = NA_real_
  )

  step1 <- sim_step(state, bar1, orders, ctr_step = 0.01, lev = 10, fee_rt = 0.001)
  expect_equal(step1$state$pos_dir, 1L)
  expect_equal(step1$state$ctr_unit, 1)
  expect_s3_class(step1$events, "data.table")
  expect_equal(nrow(step1$events), 1)
  expect_equal(step1$events$action_label[1], "open")

  bar2 <- data.frame(
    timestamp = as.POSIXct("2026-01-02", tz = "UTC"),
    open = 101,
    high = 103,
    low = 100,
    close = 102
  )
  step2 <- sim_step(step1$state, bar2, data.frame(), ctr_step = 0.01, lev = 10, fund_rt = 0.0001)
  expect_equal(step2$state$pos_dir, 1L)
  expect_true(step2$state$cash < step1$state$cash)
  expect_true(any(step2$events$event_type_label == "funding"))
})

test_that("sim_exchange_step uses incremental step state", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10))
  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC") + 0:1 * 86400,
    open = c(100, 101),
    high = c(102, 103),
    low = c(99, 100),
    close = c(101, 102)
  )

  sim_exchange_place_order(exchange, "agent1", bars$timestamp[1], side = "buy", qty = 1)
  first <- sim_exchange_step(exchange, bars[1, ])
  expect_equal(nrow(first), 1)
  expect_equal(exchange$step_state$pos_dir, 1L)
  expect_equal(sim_exchange_orders(exchange)$status[1], "filled")
  expect_equal(sim_exchange_orders(exchange)$qty_type[1], "contracts")

  second <- sim_exchange_step(exchange, bars[2, ])
  expect_equal(nrow(second), 2)
  expect_equal(exchange$step_state$pos_dir, 1L)
  expect_equal(nrow(sim_exchange_new_events(exchange)), 0)
  expect_gt(sim_exchange_account(exchange)$equity[1], first$equity[1])
})

test_that("order quantity semantics are explicit", {
  exchange <- sim_exchange_new()
  ts <- as.POSIXct("2026-01-01", tz = "UTC")
  expect_error(
    sim_exchange_place_order(exchange, "agent1", ts, side = "target", qty_type = "contracts", qty = 1),
    "requires `qty_type = 'target_pos'`"
  )
  expect_error(
    sim_exchange_place_order(exchange, "agent1", ts, side = "buy"),
    "`qty` is required"
  )
})

test_that("sim_dashboard_export writes static dashboard contract", {
  bars <- data.table::data.table(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC") + 0:3 * 86400,
    open = c(100, 101, 102, 103),
    high = c(101, 102, 103, 104),
    low = c(99, 100, 101, 102),
    close = c(101, 102, 103, 104),
    tgt_pos = c(0, 1, 1, 0)
  )
  sim <- sim_backtest(bars, ctr_step = 0.01, lev = 10)
  out_dir <- tempfile()
  paths <- sim_dashboard_export(sim, out_dir)

  expect_true(all(file.exists(paths)))
  expect_true(all(file.exists(file.path(
    out_dir,
    c("index.html", "dashboard.js", "style.css", "manifest.csv", "market_events.csv", "events.csv", "account_snapshots.csv", "risk_snapshots.csv", "orders.csv", "fills.csv")
  ))))
  manifest <- data.table::fread(file.path(out_dir, "manifest.csv"))
  expect_setequal(
    manifest$table,
    c("market_events", "events", "account_snapshots", "risk_snapshots", "orders", "fills")
  )
})

test_that("agent command APIs append and process order commands", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10))
  schemas <- sim_agent_command_schema()
  expect_setequal(names(schemas), c("agent_commands", "order_requests", "order_cancellations"))

  cmd <- sim_submit_order(exchange, "agent-a", side = "buy", qty = 1, process = FALSE)
  expect_match(cmd, "^CMD")
  expect_equal(exchange$agent_commands$status[1], "pending")
  expect_equal(exchange$order_requests$status[1], "pending")

  processed <- sim_exchange_process_commands(exchange)
  expect_equal(processed$status[1], "accepted")
  expect_equal(exchange$order_requests$status[1], "accepted")
  expect_match(exchange$order_requests$order_id[1], "^ORD")
  expect_equal(sim_exchange_orders(exchange)$qty_type[1], "contracts")

  bad <- sim_submit_order(exchange, "agent-a", side = "buy", process = TRUE)
  expect_match(bad, "^CMD")
  expect_equal(exchange$order_requests$status[2], "rejected")

  cancel <- sim_cancel_order(exchange, "agent-a", exchange$order_requests$order_id[1], process = TRUE)
  expect_match(cancel, "^CMD")
  expect_equal(exchange$order_cancellations$status[1], "accepted")
  expect_equal(sim_exchange_orders(exchange)$status[1], "cancelled")
})

test_that("live feed steps completed bars on schedule", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10))
  sim_feed_configure(exchange, sim_feed_config(
    timeframe = "4h",
    tz = "UTC",
    start_time = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    random_walk = list(start_price = 100, drift = 0, vol = 0.01, seed = 42L)
  ))
  sim_feed_start(exchange, now = as.POSIXct("2026-01-01 00:30:00", tz = "UTC"))
  bars <- sim_feed_step(exchange, now = as.POSIXct("2026-01-01 08:01:00", tz = "UTC"))
  expect_equal(nrow(bars), 2)
  expect_equal(bars$timestamp, as.POSIXct(c("2026-01-01 00:00:00", "2026-01-01 04:00:00"), tz = "UTC"))
  expect_equal(nrow(exchange$market_events), 2)
  expect_equal(sim_feed_status(exchange)$last_completed_end, as.POSIXct("2026-01-01 08:00:00", tz = "UTC"))

  no_new_bars <- sim_feed_step(exchange, now = as.POSIXct("2026-01-01 09:00:00", tz = "UTC"))
  expect_equal(nrow(no_new_bars), 0)
})

test_that("live feed accepts external adapter functions", {
  exchange <- sim_exchange_new()
  adapter <- function(symbol, timeframe, start, end, tz = "UTC") {
    data.table::data.table(timestamp = start, open = 10, high = 11, low = 9, close = 10.5)
  }
  sim_feed_configure(exchange, sim_feed_config(
    feed_mode = "external",
    feed_adapter = adapter,
    timeframe = "4h",
    start_time = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  ))
  bars <- sim_feed_step(exchange, now = as.POSIXct("2026-01-01 04:01:00", tz = "UTC"))
  expect_equal(nrow(bars), 1)
  expect_equal(bars$close[1], 10.5)
})
