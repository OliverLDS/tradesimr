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
  expect_true("assets" %in% names(sim_schemas()))
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

  exchange <- sim_exchange_new(list(ctr_step = 0.01, lev = 10, auto_register_assets = TRUE))
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

  target_exchange <- sim_exchange_new(list(ctr_step = 0.01, lev = 10, auto_register_assets = TRUE))
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

  first_bar_state <- sim_state(cash = 10000)
  first_bar_step <- sim_step(first_bar_state, bar1, orders, ctr_step = 0.01, lev = 10, fee_rt = 0.001)
  expect_equal(first_bar_step$events$equity[1], 9999.9, tolerance = 1e-8)
  expect_equal(first_bar_step$events$fee[1], 0.1, tolerance = 1e-8)

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
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10, auto_register_assets = TRUE))
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

test_that("sim_exchange_step routes orders and state by asset", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10, fee_rt = 0))
  sim_asset_add(exchange, "BTC-USDT-SWAP", asset_id = 1L)
  sim_asset_add(exchange, "ETH-USDT-SWAP", asset_id = 2L)
  ts <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  bars <- data.frame(
    timestamp = c(ts, ts),
    symbol = c("BTC-USDT-SWAP", "ETH-USDT-SWAP"),
    asset_id = c(1L, 2L),
    open = c(100, 50),
    high = c(101, 51),
    low = c(99, 49),
    close = c(100, 50)
  )

  sim_exchange_place_order(exchange, "agent1", ts, symbol = "BTC-USDT-SWAP", asset_id = 1L, side = "buy", qty = 1)
  sim_exchange_place_order(exchange, "agent1", ts, symbol = "ETH-USDT-SWAP", asset_id = 2L, side = "sell", qty = 2)
  sim_exchange_step(exchange, bars)

  positions <- sim_exchange_positions(exchange)
  expect_setequal(positions$asset_id, c(1L, 2L))
  expect_equal(positions$pos_dir[positions$asset_id == 1L], 1L)
  expect_equal(positions$pos_dir[positions$asset_id == 2L], -1L)
  expect_equal(positions$ctr_unit[positions$asset_id == 1L], 1)
  expect_equal(positions$ctr_unit[positions$asset_id == 2L], 2)

  account <- sim_exchange_account(exchange)
  expect_equal(nrow(account), 1)
  expect_equal(account$agent_id[1], "agent1")
  expect_true(all(c("equity", "cash", "unrealized_pnl") %in% names(account)))
  expect_equal(account$cash[1], 10000)
  expect_equal(length(exchange$agent_states), 2)
})

test_that("asset registry is explicit unless auto registration is enabled", {
  exchange <- sim_exchange_new()
  bar <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
    symbol = "BTC-USDT-SWAP",
    open = 100,
    high = 101,
    low = 99,
    close = 100
  )
  expect_error(sim_exchange_step(exchange, bar), "Unregistered market bar asset")
  sim_asset_add(exchange, "BTC-USDT-SWAP")
  sim_exchange_step(exchange, bar)
  expect_equal(nrow(sim_assets(exchange)), 1)
  expect_equal(sim_assets(exchange)$symbol[1], "BTC-USDT-SWAP")
  expect_true(sim_asset_remove(exchange, "BTC-USDT-SWAP"))
  expect_equal(sim_assets(exchange)$status[1], "removed")

  auto_exchange <- sim_exchange_new(list(auto_register_assets = TRUE))
  sim_exchange_step(auto_exchange, bar)
  expect_equal(sim_assets(auto_exchange)$symbol[1], "BTC-USDT-SWAP")
})

test_that("order quantity semantics are explicit", {
  exchange <- sim_exchange_new(list(auto_register_assets = TRUE))
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
    c("index.html", "shared/dashboard.js", "shared/style.css", "manifest.csv", "market_events.csv", "strategy_snapshots.csv", "events.csv", "account_snapshots.csv", "risk_snapshots.csv", "orders.csv", "fills.csv")
  ))))
  manifest <- data.table::fread(file.path(out_dir, "manifest.csv"))
  expect_setequal(
    manifest$table,
    c("market_events", "strategy_snapshots", "events", "account_snapshots", "risk_snapshots", "orders", "fills")
  )
})

test_that("agent command APIs append and process order commands", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10, auto_register_assets = TRUE))
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
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10, auto_register_assets = TRUE))
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

test_that("live feed does not create a default agent before registration", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10, auto_register_assets = TRUE))
  sim_feed_configure(exchange, sim_feed_config(
    timeframe = "1m",
    tz = "UTC",
    start_time = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  ))
  sim_feed_start(exchange, now = as.POSIXct("2026-01-01 00:00:30", tz = "UTC"))
  bars <- sim_feed_step(exchange, now = as.POSIXct("2026-01-01 00:03:01", tz = "UTC"))

  expect_equal(nrow(bars), 3)
  expect_equal(nrow(exchange$market_events), 3)
  expect_equal(nrow(exchange$agents), 0)
  expect_equal(nrow(sim_exchange_account(exchange)), 0)
})

test_that("live feed config updates preserve running state", {
  exchange <- sim_exchange_new(list(auto_register_assets = TRUE))
  sim_feed_configure(exchange, sim_feed_config(timeframe = "4h", tz = "UTC"))
  sim_feed_start(exchange, now = as.POSIXct("2026-01-01 08:30:00", tz = "UTC"))
  before <- sim_feed_status(exchange)
  expect_true(before$running)
  expect_equal(before$last_completed_end, as.POSIXct("2026-01-01 08:00:00", tz = "UTC"))

  sim_feed_configure(exchange, list(timeframe = "5m"))
  after <- sim_feed_status(exchange)
  expect_true(after$running)
  expect_equal(after$last_completed_end, before$last_completed_end)
})

test_that("live feed configs are keyed by registered asset", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "AAPL", asset_id = 101L, asset_class = "stock", tick_size = 0.01, qty_step = 1)
  sim_asset_add(exchange, "ES-202603", asset_id = 202L, asset_class = "commodity_future", contract_size = 50, tick_size = 0.25, qty_step = 1)

  sim_feed_configure(exchange, sim_feed_config(symbol = "AAPL", asset_id = 101L, timeframe = "1m", random_walk = list(start_price = 200, seed = 1L)))
  sim_feed_configure(exchange, sim_feed_config(symbol = "ES-202603", asset_id = 202L, timeframe = "5m", random_walk = list(start_price = 5000, seed = 2L)))

  expect_equal(exchange$feeds[["101"]]$timeframe, "1m")
  expect_equal(exchange$feeds[["202"]]$timeframe, "5m")

  sim_feed_start(exchange, symbol = "AAPL", asset_id = 101L, now = as.POSIXct("2026-01-01 00:00:30", tz = "UTC"))
  bars <- sim_feed_step(exchange, symbol = "AAPL", asset_id = 101L, now = as.POSIXct("2026-01-01 00:02:01", tz = "UTC"))
  expect_true(nrow(bars) >= 1)
  expect_equal(unique(bars$symbol), "AAPL")
  expect_equal(unique(bars$asset_id), 101L)
  expect_false(isTRUE(exchange$feeds[["202"]]$running))
})

test_that("live feed config set and controls apply to all registered assets", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "AAPL", asset_id = 101L, asset_class = "stock")
  sim_asset_add(exchange, "TLT", asset_id = 102L, asset_class = "etf")

  sim_feed_configure(exchange, list(configs = list(
    list(symbol = "AAPL", asset_id = 101L, timeframe = "1m", random_walk = list(start_price = 200, seed = 1L)),
    list(symbol = "TLT", asset_id = 102L, timeframe = "1m", random_walk = list(start_price = 90, seed = 2L))
  )))
  expect_equal(exchange$feeds[["101"]]$timeframe, "1m")
  expect_equal(exchange$feeds[["102"]]$timeframe, "1m")

  sim_feed_start(exchange, now = as.POSIXct("2026-01-01 00:00:30", tz = "UTC"))
  expect_true(all(vapply(exchange$feeds, function(feed) isTRUE(feed$running), logical(1))))

  bars <- sim_feed_step(exchange, now = as.POSIXct("2026-01-01 00:02:01", tz = "UTC"))
  expect_setequal(unique(bars$asset_id), c(101L, 102L))

  sim_feed_stop(exchange)
  expect_false(any(vapply(exchange$feeds, function(feed) isTRUE(feed$running), logical(1))))
})

test_that("market-level random walk generates correlated warmup batches", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "BTC-USDT-SWAP", asset_id = 101L)
  sim_asset_add(exchange, "ETH-USDT-SWAP", asset_id = 102L)
  sim_feed_configure(exchange, list(configs = list(
    list(symbol = "BTC-USDT-SWAP", asset_id = 101L, timeframe = "1m", random_walk = list(start_price = 100, drift = 0, vol = 0.01, seed = 1L)),
    list(symbol = "ETH-USDT-SWAP", asset_id = 102L, timeframe = "1m", random_walk = list(start_price = 100, drift = 0, vol = 0.01, seed = 2L))
  )))
  sim_market_model_configure(exchange, sim_market_model_config(
    model = "multi_asset_random_walk",
    corr = matrix(c(1, 0.9, 0.9, 1), nrow = 2),
    seed = 42L
  ))

  bars <- sim_feed_warmup(exchange, n_bars = 240, now = as.POSIXct("2026-01-01 04:00:30", tz = "UTC"))
  expect_equal(nrow(bars), 480)
  expect_true(all(bars[, .N, by = timestamp]$N == 2L))
  expect_equal(sim_feed_status(exchange)$market_model$model, "multi_asset_random_walk")

  wide <- data.table::dcast(
    bars[, .(timestamp, symbol, close)],
    timestamp ~ symbol,
    value.var = "close"
  )
  returns <- data.table::data.table(
    btc = diff(log(wide[["BTC-USDT-SWAP"]])),
    eth = diff(log(wide[["ETH-USDT-SWAP"]]))
  )
  expect_gt(stats::cor(returns$btc, returns$eth), 0.75)
})

test_that("market-level random walk live step advances assets as synchronized batches", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "AAPL", asset_id = 101L)
  sim_asset_add(exchange, "TLT", asset_id = 102L)
  sim_feed_configure(exchange, list(configs = list(
    list(symbol = "AAPL", asset_id = 101L, timeframe = "1m", random_walk = list(start_price = 200, drift = 0, vol = 0.01, seed = 1L)),
    list(symbol = "TLT", asset_id = 102L, timeframe = "1m", random_walk = list(start_price = 90, drift = 0, vol = 0.01, seed = 2L))
  )))
  sim_market_model_configure(exchange, sim_market_model_config(
    model = "multi_asset_random_walk",
    corr = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    seed = 7L
  ))

  sim_feed_start(exchange, now = as.POSIXct("2026-01-01 00:00:30", tz = "UTC"))
  bars <- sim_feed_step(exchange, now = as.POSIXct("2026-01-01 00:02:01", tz = "UTC"))

  expect_equal(nrow(bars), 4)
  expect_true(all(bars[, .N, by = timestamp]$N == 2L))
  expect_equal(nrow(exchange$market_events), 4)
  expect_true(isTRUE(exchange$market_model$running))
  expect_equal(sim_feed_status(exchange)$market_model$running, TRUE)
})

test_that("market-level AR-GARCH updates per-asset state with correlated shocks", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "BTC-USDT-SWAP", asset_id = 101L)
  sim_asset_add(exchange, "ETH-USDT-SWAP", asset_id = 102L)
  sim_feed_configure(exchange, list(configs = list(
    list(
      symbol = "BTC-USDT-SWAP", asset_id = 101L, timeframe = "1m",
      random_walk = list(start_price = 100, drift = 0, vol = 0.02, seed = 1L),
      simulation = list(ar = list(a = 0.2), garch11 = list(alpha0 = 0.000001, alpha1 = 0.08, beta1 = 0.9))
    ),
    list(
      symbol = "ETH-USDT-SWAP", asset_id = 102L, timeframe = "1m",
      random_walk = list(start_price = 100, drift = 0, vol = 0.025, seed = 2L),
      simulation = list(ar = list(a = 0.1), garch11 = list(alpha0 = 0.000001, alpha1 = 0.08, beta1 = 0.9))
    )
  )))
  sim_market_model_configure(exchange, sim_market_model_config(
    model = "multi_asset_ar_garch",
    corr = matrix(c(1, 0.75, 0.75, 1), nrow = 2),
    seed = 11L
  ))

  bars <- sim_feed_warmup(exchange, n_bars = 10, now = as.POSIXct("2026-01-01 00:10:30", tz = "UTC"))

  expect_equal(nrow(bars), 20)
  expect_true(is.finite(exchange$feeds[["101"]]$simulation_state$sigma2))
  expect_true(is.finite(exchange$feeds[["102"]]$simulation_state$sigma2))
  expect_true(length(exchange$feeds[["101"]]$simulation_state$ret_lags) >= 10)
})

test_that("factor and regime market models generate synchronized correlated batches", {
  make_exchange <- function() {
    exchange <- sim_exchange_new()
    sim_asset_add(exchange, "AAPL", asset_id = 101L)
    sim_asset_add(exchange, "MSFT", asset_id = 102L)
    sim_feed_configure(exchange, list(configs = list(
      list(symbol = "AAPL", asset_id = 101L, timeframe = "1m", random_walk = list(start_price = 100, drift = 0, vol = 0.01, seed = 1L)),
      list(symbol = "MSFT", asset_id = 102L, timeframe = "1m", random_walk = list(start_price = 100, drift = 0, vol = 0.01, seed = 2L))
    )))
    exchange
  }
  realized_cor <- function(bars) {
    wide <- data.table::dcast(bars[, .(timestamp, symbol, close)], timestamp ~ symbol, value.var = "close")
    stats::cor(diff(log(wide[[2L]])), diff(log(wide[[3L]])))
  }

  factor_exchange <- make_exchange()
  sim_market_model_configure(factor_exchange, sim_market_model_config(
    model = "factor_random_walk",
    factors = list(loadings = matrix(c(0.9, 0.85), nrow = 2), factor_vol = 0.02, idio_vol = c(0.001, 0.001)),
    seed = 12L
  ))
  factor_bars <- sim_feed_warmup(factor_exchange, n_bars = 180, now = as.POSIXct("2026-01-01 03:00:30", tz = "UTC"))
  expect_gt(realized_cor(factor_bars), 0.85)

  regime_exchange <- make_exchange()
  sim_market_model_configure(regime_exchange, sim_market_model_config(
    model = "regime_random_walk",
    regimes = list(
      initial_state = 2L,
      transition = matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE),
      states = list(
        list(name = "calm", corr = diag(2), vol_multiplier = 1),
        list(name = "stress", corr = matrix(c(1, 0.9, 0.9, 1), nrow = 2), vol_multiplier = 2)
      )
    ),
    seed = 13L
  ))
  regime_bars <- sim_feed_warmup(regime_exchange, n_bars = 180, now = as.POSIXct("2026-01-01 03:00:30", tz = "UTC"))
  expect_gt(realized_cor(regime_bars), 0.75)
  expect_equal(sim_market_model_status(regime_exchange)$regime, 2L)
})

test_that("market model seeds reproduce generated bars", {
  make_exchange <- function() {
    exchange <- sim_exchange_new()
    sim_asset_add(exchange, "BTC-USDT-SWAP", asset_id = 101L)
    sim_asset_add(exchange, "ETH-USDT-SWAP", asset_id = 102L)
    sim_feed_configure(exchange, list(configs = list(
      list(symbol = "BTC-USDT-SWAP", asset_id = 101L, timeframe = "1m", random_walk = list(start_price = 100, drift = 0, vol = 0.01, seed = 1L)),
      list(symbol = "ETH-USDT-SWAP", asset_id = 102L, timeframe = "1m", random_walk = list(start_price = 200, drift = 0, vol = 0.015, seed = 2L))
    )))
    sim_market_model_configure(exchange, sim_market_model_config(
      model = "multi_asset_random_walk",
      corr = matrix(c(1, 0.6, 0.6, 1), nrow = 2),
      seed = 123L
    ))
    exchange
  }
  a <- make_exchange()
  b <- make_exchange()

  bars_a <- sim_feed_warmup(a, n_bars = 12, now = as.POSIXct("2026-01-01 00:12:30", tz = "UTC"))
  bars_b <- sim_feed_warmup(b, n_bars = 12, now = as.POSIXct("2026-01-01 00:12:30", tz = "UTC"))

  expect_equal(bars_a, bars_b, ignore_attr = TRUE)
})

test_that("market model calibration estimates dynamics and persists metadata", {
  set.seed(42)
  n <- 80L
  eps1 <- stats::rnorm(n, sd = 0.01)
  eps2 <- 0.6 * eps1 + stats::rnorm(n, sd = 0.008)
  bars <- data.table::rbindlist(list(
    data.table::data.table(
      timestamp = as.POSIXct("2026-01-01", tz = "UTC") + seq_len(n) * 60,
      symbol = "AAPL",
      asset_id = 101L,
      close = 100 * exp(cumsum(eps1))
    ),
    data.table::data.table(
      timestamp = as.POSIXct("2026-01-01", tz = "UTC") + seq_len(n) * 60,
      symbol = "MSFT",
      asset_id = 102L,
      close = 90 * exp(cumsum(eps2))
    )
  ), fill = TRUE)
  bars[, `:=`(open = close, high = close * 1.001, low = close * 0.999)]

  config <- sim_market_model_calibrate(bars, model = "multi_asset_ar_garch", ar_order = 1L, seed = 99L, timeframe = "1m")

  expect_equal(config$model, "multi_asset_ar_garch")
  expect_true(!is.null(config$calibration))
  expect_equal(nrow(config$calibration$assets), 2)
  expect_true(all(c("drift", "vol", "ar", "garch11") %in% names(config$calibration$assets)))
  expect_gt(config$corr[1, 2], 0.3)
  expect_true(!is.null(config$factors$loadings))
  expect_true(!is.null(config$regimes$states))

  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "AAPL", asset_id = 101L, asset_class = "stock")
  sim_asset_add(exchange, "MSFT", asset_id = 102L, asset_class = "stock")
  sim_feed_configure(exchange, list(configs = list(
    list(symbol = "AAPL", asset_id = 101L, timeframe = "1m", random_walk = list(start_price = 100, vol = 0.5)),
    list(symbol = "MSFT", asset_id = 102L, timeframe = "1m", random_walk = list(start_price = 90, vol = 0.5))
  )))
  sim_market_model_configure(exchange, config)
  tradesimr:::.apply_calibration_to_feeds(exchange, config$calibration)
  expect_lt(exchange$feeds[["101"]]$random_walk$vol, 0.1)

  out_dir <- tempfile("tradesimr-calibration-")
  sim_exchange_save(exchange, out_dir)
  loaded <- sim_exchange_load(out_dir)
  expect_true(sim_market_model_status(loaded)$calibrated)
  expect_equal(nrow(loaded$market_model$calibration$assets), 2)
})

test_that("calibrated covariance drives synchronized market simulation", {
  set.seed(44)
  n <- 100L
  x <- stats::rnorm(n, sd = 0.012)
  y <- 0.8 * x + stats::rnorm(n, sd = 0.004)
  bars <- data.table::rbindlist(list(
    data.table::data.table(timestamp = as.POSIXct("2026-01-01", tz = "UTC") + seq_len(n) * 60, symbol = "AAA", asset_id = 11L, close = 100 * exp(cumsum(x))),
    data.table::data.table(timestamp = as.POSIXct("2026-01-01", tz = "UTC") + seq_len(n) * 60, symbol = "BBB", asset_id = 22L, close = 80 * exp(cumsum(y)))
  ))
  bars[, `:=`(open = close, high = close * 1.001, low = close * 0.999)]
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "AAA", asset_id = 11L)
  sim_asset_add(exchange, "BBB", asset_id = 22L)
  sim_feed_configure(exchange, list(configs = list(
    list(symbol = "AAA", asset_id = 11L, timeframe = "1m", random_walk = list(start_price = 100)),
    list(symbol = "BBB", asset_id = 22L, timeframe = "1m", random_walk = list(start_price = 80))
  )))
  sim_market_model_calibrate_exchange(exchange, bars, model = "multi_asset_random_walk", seed = 77L, timeframe = "1m")

  generated <- sim_feed_warmup(exchange, n_bars = 120, now = as.POSIXct("2026-01-01 02:00:30", tz = "UTC"))
  wide <- data.table::dcast(generated[, .(timestamp, symbol, close)], timestamp ~ symbol, value.var = "close")
  realized <- stats::cor(diff(log(wide$AAA)), diff(log(wide$BBB)))
  expect_gt(realized, 0.45)
})

test_that("saved market model config loads and continues simulation", {
  make_exchange <- function() {
    exchange <- sim_exchange_new()
    sim_asset_add(exchange, "AAPL", asset_id = 101L)
    sim_asset_add(exchange, "MSFT", asset_id = 102L)
    sim_feed_configure(exchange, list(configs = list(
      list(symbol = "AAPL", asset_id = 101L, timeframe = "1m", random_walk = list(start_price = 100, drift = 0, vol = 0.01, seed = 1L)),
      list(symbol = "MSFT", asset_id = 102L, timeframe = "1m", random_walk = list(start_price = 200, drift = 0, vol = 0.01, seed = 2L))
    )))
    sim_market_model_configure(exchange, sim_market_model_config(
      model = "multi_asset_random_walk",
      corr = matrix(c(1, 0.8, 0.8, 1), nrow = 2),
      seed = 77L
    ))
    exchange
  }
  out_dir <- tempfile("tradesimr-market-model-")
  exchange <- make_exchange()
  control <- make_exchange()
  sim_feed_start(exchange, now = as.POSIXct("2026-01-01 00:00:30", tz = "UTC"))
  sim_feed_start(control, now = as.POSIXct("2026-01-01 00:00:30", tz = "UTC"))
  sim_feed_step(exchange, now = as.POSIXct("2026-01-01 00:02:01", tz = "UTC"))
  sim_exchange_save(exchange, out_dir)

  loaded <- sim_exchange_load(out_dir)
  continued <- sim_feed_step(loaded, now = as.POSIXct("2026-01-01 00:04:01", tz = "UTC"))
  control_all <- sim_feed_step(control, now = as.POSIXct("2026-01-01 00:04:01", tz = "UTC"))

  expect_true(file.exists(file.path(out_dir, "market_model.csv")))
  expect_equal(loaded$market_model$model, "multi_asset_random_walk")
  expect_equal(continued, tail(control_all, nrow(continued)), ignore_attr = TRUE)
})

test_that("dashboard export includes market model and cross-asset risk metadata", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "AAPL", asset_id = 101L)
  sim_asset_add(exchange, "TLT", asset_id = 102L)
  sim_feed_configure(exchange, list(configs = list(
    list(symbol = "AAPL", asset_id = 101L, timeframe = "1m", random_walk = list(start_price = 100, vol = 0.01, seed = 1L)),
    list(symbol = "TLT", asset_id = 102L, timeframe = "1m", random_walk = list(start_price = 90, vol = 0.01, seed = 2L))
  )))
  sim_market_model_configure(exchange, sim_market_model_config(
    model = "multi_asset_random_walk",
    corr = matrix(c(1, -0.2, -0.2, 1), nrow = 2),
    seed = 9L
  ))
  sim_feed_warmup(exchange, n_bars = 2, now = as.POSIXct("2026-01-01 00:02:30", tz = "UTC"))
  sim_submit_order(exchange, "agent-a", symbol = "AAPL", asset_id = 101L, side = "buy", qty = 1, process = TRUE)
  sim_submit_order(exchange, "agent-a", symbol = "TLT", asset_id = 102L, side = "sell", qty = 1, process = TRUE)
  sim_exchange_step(exchange, exchange$market_events[nrow(exchange$market_events)])
  out_dir <- tempfile("tradesimr-dashboard-market-")

  sim_state_dashboard_export(exchange, out_dir)
  manifest <- data.table::fread(file.path(out_dir, "manifest.csv"))
  risk <- data.table::fread(file.path(out_dir, "cross_asset_risk.csv"))

  expect_true("market_model" %in% manifest$table)
  expect_true("cross_asset_risk" %in% manifest$table)
  expect_true(file.exists(file.path(out_dir, "market_model.csv")))
  expect_true(nrow(risk) > 0)
  expect_true(all(c("allocation", "asset_class_allocation", "factor_exposure", "max_drawdown", "risk_contribution", "concentration_hhi", "stress_loss") %in% names(risk)))
})

test_that("optional portfolio margin uses correlation-aware maintenance", {
  exchange <- sim_exchange_new(list(cash = 1000, ctr_step = 1, lev = 1, mmr = 0.4, portfolio_margin = TRUE, portfolio_margin_sigma = 1, portfolio_margin_floor = 0))
  sim_asset_add(exchange, "AAA", asset_id = 11L)
  sim_asset_add(exchange, "BBB", asset_id = 22L)
  sim_feed_configure(exchange, list(configs = list(
    list(symbol = "AAA", asset_id = 11L, timeframe = "1m", random_walk = list(start_price = 100, vol = 0.01)),
    list(symbol = "BBB", asset_id = 22L, timeframe = "1m", random_walk = list(start_price = 100, vol = 0.01))
  )))
  sim_market_model_configure(exchange, sim_market_model_config(
    model = "multi_asset_random_walk",
    corr = matrix(c(1, -0.95, -0.95, 1), nrow = 2),
    seed = 1L
  ))
  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
    symbol = c("AAA", "BBB"),
    asset_id = c(11L, 22L),
    open = c(100, 100),
    high = c(100, 100),
    low = c(100, 100),
    close = c(100, 100)
  )
  sim_exchange_add_bars(exchange, bars)
  sim_submit_order(exchange, "hedged", symbol = "AAA", asset_id = 11L, side = "buy", qty = 1, process = TRUE)
  sim_submit_order(exchange, "hedged", symbol = "BBB", asset_id = 22L, side = "buy", qty = 1, process = TRUE)
  sim_exchange_step(exchange, bars)

  expect_false(isTRUE(exchange$agent_accounts[["hedged"]]$liquidated))
  expect_true(tradesimr:::.portfolio_margin_required(exchange, "hedged", tradesimr:::.agent_position_snapshots(exchange, "hedged", Sys.time())) < 100)

  reject_exchange <- sim_exchange_new(list(cash = 100, ctr_step = 1, lev = 1, mmr = 0.4, portfolio_margin = TRUE, portfolio_margin_sigma = 2, portfolio_margin_floor = 0))
  sim_asset_add(reject_exchange, "VOL", asset_id = 33L)
  sim_feed_configure(reject_exchange, list(symbol = "VOL", asset_id = 33L, random_walk = list(start_price = 100, vol = 0.1)))
  reject_bar <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
    symbol = "VOL",
    asset_id = 33L,
    open = 100,
    high = 100,
    low = 100,
    close = 100
  )
  sim_exchange_add_bars(reject_exchange, reject_bar)
  sim_submit_order(reject_exchange, "risk-limited", symbol = "VOL", asset_id = 33L, side = "buy", qty = 8, process = TRUE)
  sim_exchange_step(reject_exchange, reject_bar)
  expect_equal(sim_exchange_orders(reject_exchange)$status, "failed")
})

test_that("C++ portfolio step applies shared-cash portfolio margin before accepting orders", {
  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
    symbol = c("AAA", "BBB"),
    asset_id = c(11L, 22L),
    open = c(100, 100),
    high = c(100, 100),
    low = c(100, 100),
    close = c(100, 100)
  )
  states <- list(
    `11` = sim_state(cash = 1000, asset = 11L, last_px = 100),
    `22` = sim_state(cash = 1000, asset = 22L, last_px = 100)
  )
  hedge_orders <- data.frame(
    order_id = c("a", "b"),
    asset_id = c(11L, 22L),
    action = c("open", "open"),
    dir = c("long", "long"),
    order_type = c("market", "market"),
    ctr_qty = c(8, 8),
    price = c(NA_real_, NA_real_),
    strat_id = c(0L, 0L),
    action_id = c(1L, 1L)
  )
  hedged <- sim_portfolio_step(
    states,
    bars,
    hedge_orders,
    cov = matrix(c(0.01, -0.0095, -0.0095, 0.01), nrow = 2),
    shared_cash = 1000,
    portfolio_margin_sigma = 1,
    portfolio_margin_floor = 0,
    fee_rt = 0
  )
  expect_equal(hedged$states[["11"]]$ctr_unit, 8)
  expect_equal(hedged$states[["22"]]$ctr_unit, 8)
  expect_false(isTRUE(hedged$liquidated))

  concentrated <- sim_portfolio_step(
    states[1],
    bars[1, ],
    hedge_orders[1, ],
    cov = matrix(0.01, nrow = 1),
    shared_cash = 100,
    portfolio_margin_sigma = 2,
    portfolio_margin_floor = 0,
    fee_rt = 0
  )
  expect_equal(concentrated$states[["11"]]$ctr_unit, 0)
  expect_true(any(concentrated$events$status_label == "failed"))
})

test_that("simulation feed uses asset-specific random streams", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "BTC-USDT-SWAP", asset_id = 101L)
  sim_asset_add(exchange, "AAPL", asset_id = 102L)
  same_walk <- list(start_price = 100, drift = 0, vol = 0.02, seed = 1L)
  sim_feed_configure(exchange, list(configs = list(
    list(symbol = "BTC-USDT-SWAP", asset_id = 101L, timeframe = "1m", random_walk = same_walk),
    list(symbol = "AAPL", asset_id = 102L, timeframe = "1m", random_walk = same_walk)
  )))

  bars <- sim_feed_warmup(exchange, n_bars = 3, now = as.POSIXct("2026-01-01 00:03:30", tz = "UTC"))
  wide <- data.table::dcast(
    bars[, .(timestamp, symbol, close)],
    timestamp ~ symbol,
    value.var = "close"
  )
  expect_false(isTRUE(all.equal(wide[["BTC-USDT-SWAP"]], wide[["AAPL"]])))
})

test_that("simulation feeds support deterministic AR dynamics", {
  exchange <- sim_exchange_new(list(auto_register_assets = TRUE))
  sim_feed_configure(exchange, sim_feed_config(
    timeframe = "1m",
    simulation_model = "ar",
    random_walk = list(start_price = 100, drift = 0.01, vol = 0, seed = 1L),
    simulation = list(ar = list(a = 0.5, sigma = 0), ohlc = list(wiggle_scale = 0))
  ))

  bars <- sim_feed_warmup(exchange, n_bars = 3, now = as.POSIXct("2026-01-01 00:03:30", tz = "UTC"))
  observed_returns <- diff(log(c(100, bars$close)))

  expect_equal(round(observed_returns, 6), c(0.01, 0.015, 0.0175))
  expect_equal(sim_feed_status(exchange)$feeds[[1]]$simulation_model, "ar")
})

test_that("simulation feeds support GARCH and AR-GARCH state", {
  exchange <- sim_exchange_new(list(auto_register_assets = TRUE))
  sim_feed_configure(exchange, sim_feed_config(
    timeframe = "1m",
    simulation_model = "garch11",
    random_walk = list(start_price = 100, drift = 0, vol = 0.02, seed = 2L),
    simulation = list(garch11 = list(alpha0 = 0.000001, alpha1 = 0.1, beta1 = 0.85, z_dist = "stdt", df = 8))
  ))
  bars <- sim_feed_warmup(exchange, n_bars = 5, now = as.POSIXct("2026-01-01 00:05:30", tz = "UTC"))
  expect_equal(nrow(bars), 5)
  expect_true(is.finite(exchange$feed$simulation_state$sigma2))
  expect_true(length(exchange$feed$simulation_state$ret_lags) > 0)

  sim_feed_configure(exchange, list(
    simulation_model = "ar_garch",
    random_walk = list(start_price = tail(bars$close, 1), drift = 0, vol = 0.02, seed = 3L),
    simulation = list(ar = list(a = c(0.3, -0.1)), garch11 = list(alpha0 = 0.000001, alpha1 = 0.08, beta1 = 0.9))
  ))
  more <- sim_feed_warmup(exchange, n_bars = 3, now = as.POSIXct("2026-01-01 00:08:30", tz = "UTC"))
  expect_equal(nrow(more), 3)
  expect_true(is.finite(exchange$feed$simulation_state$sigma2))
  expect_true(length(exchange$feed$simulation_state$ret_lags) >= 3)
})

test_that("feed simulation state persists and restores AR-GARCH continuation state", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "AAPL", asset_id = 101L)
  sim_feed_configure(exchange, sim_feed_config(
    symbol = "AAPL",
    asset_id = 101L,
    timeframe = "1m",
    simulation_model = "ar_garch",
    random_walk = list(start_price = 100, drift = 0, vol = 0.02, seed = 7L),
    simulation = list(ar = list(a = c(0.2)), garch11 = list(alpha1 = 0.08, beta1 = 0.9))
  ))
  sim_feed_warmup(exchange, n_bars = 4, now = as.POSIXct("2026-01-01 00:04:30", tz = "UTC"))
  expect_true(length(exchange$feeds[["101"]]$simulation_state$ret_lags) > 0)
  out_dir <- tempfile("tradesimr-feed-state-")
  sim_exchange_save(exchange, out_dir)
  loaded <- sim_exchange_load(out_dir)
  expect_equal(loaded$feeds[["101"]]$simulation_state$ret_lags, exchange$feeds[["101"]]$simulation_state$ret_lags)
  expect_equal(loaded$feeds[["101"]]$simulation_state$sigma2, exchange$feeds[["101"]]$simulation_state$sigma2)
})

test_that("single-asset regime, jump shocks, and Brownian bridge OHLC are configurable", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "JUMP", asset_id = 303L)
  sim_feed_configure(exchange, sim_feed_config(
    symbol = "JUMP",
    asset_id = 303L,
    timeframe = "1m",
    simulation_model = "regime",
    random_walk = list(start_price = 100, drift = 0, vol = 0.001, seed = 33L),
    simulation = list(
      regime = list(
        initial_state = 2L,
        transition = matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE),
        states = list(list(name = "normal", vol_multiplier = 1), list(name = "crash", drift = -0.05, vol_multiplier = 1))
      ),
      shock = list(jump_intensity = 1, jump_mean = -0.02, jump_sd = 0),
      ohlc = list(model = "brownian_bridge", bridge_steps = 16L)
    )
  ))
  bars <- sim_feed_warmup(exchange, n_bars = 2, now = as.POSIXct("2026-01-01 00:02:30", tz = "UTC"))
  expect_equal(exchange$feeds[["303"]]$simulation_state$current_regime, 2L)
  expect_true(all(bars$high >= pmax(bars$open, bars$close)))
  expect_true(all(bars$low <= pmin(bars$open, bars$close)))
  expect_true(all(is.finite(bars$close)))
  expect_true(any(abs(diff(log(c(100, bars$close)))) > 0.01))
})

test_that("live feed accepts external adapter functions", {
  exchange <- sim_exchange_new(list(auto_register_assets = TRUE))
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

test_that("simulation feed warmup appends historical bars without stepping agents", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10, auto_register_assets = TRUE))
  sim_feed_configure(exchange, sim_feed_config(
    timeframe = "1h",
    tz = "UTC",
    random_walk = list(start_price = 100, drift = 0, vol = 0.01, seed = 11L)
  ))
  bars <- sim_feed_warmup(exchange, n_bars = 5, now = as.POSIXct("2026-01-01 05:30:00", tz = "UTC"))
  expect_equal(nrow(bars), 5)
  expect_equal(nrow(exchange$market_events), 5)
  expect_equal(nrow(exchange$agent_decisions), 0)
  expect_equal(sim_feed_status(exchange)$last_completed_end, as.POSIXct("2026-01-01 05:00:00", tz = "UTC"))

  agent_id <- sim_agent_add(exchange, agent_id = "cash-agent", agent_type = "momentum", config = list(qty = 1, initial_cash = 25000))
  account <- sim_exchange_account(exchange)
  positions <- sim_exchange_positions(exchange)
  expect_equal(account$cash[account$agent_id == agent_id], 25000)
  expect_equal(positions$last_px[positions$agent_id == agent_id], tail(exchange$market_events$close, 1))

  rankings <- sim_agent_rankings(exchange)
  expect_true("unrealized_pnl" %in% names(rankings))
  expect_equal(rankings$unrealized_pnl[rankings$agent_id == agent_id], 0)
})

test_that("AI agents submit ordinary order commands", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10, auto_register_assets = TRUE))
  agent_id <- sim_agent_add(exchange, agent_type = "momentum", config = list(qty = 1))
  expect_match(agent_id, "^momentum-")
  expect_equal(exchange$agents$agent_type[1], "momentum")

  bar1 <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    open = 100,
    high = 101,
    low = 99,
    close = 100
  )
  bar2 <- data.frame(
    timestamp = as.POSIXct("2026-01-01 01:00:00", tz = "UTC"),
    open = 100,
    high = 103,
    low = 100,
    close = 102
  )

  sim_exchange_step(exchange, bar1)
  decisions <- sim_agents_step(exchange, bar2)
  expect_equal(nrow(decisions), 1)
  expect_equal(decisions$side[1], "buy")
  expect_equal(decisions$intended_action[1], "open")
  expect_equal(decisions$intended_dir[1], "long")
  expect_equal(exchange$order_requests$status[1], "pending")

  sim_exchange_process_commands(exchange)
  expect_equal(exchange$order_requests$status[1], "accepted")
  expect_equal(exchange$agent_orders$agent_id[1], agent_id)

  rankings <- sim_agent_rankings(exchange)
  expect_equal(rankings$agent_id[1], agent_id)
  expect_equal(rankings$orders[1], 1L)

  expect_true(sim_agent_set_status(exchange, agent_id, "paused"))
  expect_equal(exchange$agents$status[1], "paused")
})

test_that("AI agents can choose among registered assets", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10, fee_rt = 0, auto_register_assets = TRUE))
  sim_asset_add(exchange, "AAPL", asset_id = 101L, asset_class = "stock")
  sim_asset_add(exchange, "TLT", asset_id = 102L, asset_class = "etf")
  sim_agent_add(exchange, agent_id = "allocator", agent_type = "chaos", config = list(qty = 1, asset_policy = "random"))
  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC") + c(0, 0),
    symbol = c("AAPL", "TLT"),
    asset_id = c(101L, 102L),
    open = c(200, 90),
    high = c(201, 91),
    low = c(199, 89),
    close = c(200, 90)
  )
  sim_exchange_step(exchange, bars)
  set.seed(10)
  decision <- sim_agents_step(exchange)
  expect_equal(nrow(decision), 1)
  expect_true(decision$asset_id[1] %in% c(101L, 102L))
})

test_that("AI no-op decisions are not recorded as orders", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10, auto_register_assets = TRUE))
  sim_agent_add(exchange, agent_id = "flat-momentum", agent_type = "momentum", config = list(qty = 1))
  bar <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    open = 100,
    high = 101,
    low = 99,
    close = 100
  )

  decisions <- sim_agents_step(exchange, bar)

  expect_equal(nrow(decisions), 0)
  expect_equal(nrow(exchange$agent_decisions), 0)
  expect_equal(nrow(exchange$order_requests), 0)
  expect_equal(nrow(exchange$agent_orders), 0)
})

test_that("momentum agents do not duplicate selected market bars", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10, auto_register_assets = TRUE))
  sim_agent_add(exchange, agent_id = "history-momentum", agent_type = "momentum", config = list(qty = 1, asset_policy = "random"))
  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + c(0, 3600),
    open = c(100, 100),
    high = c(101, 103),
    low = c(99, 100),
    close = c(100, 102)
  )
  sim_exchange_step(exchange, bars)

  decisions <- sim_agents_step(exchange)

  expect_equal(nrow(decisions), 1)
  expect_equal(decisions$side[1], "buy")
  expect_equal(decisions$intended_action[1], "open")
  expect_equal(nrow(exchange$order_requests), 1)
})

test_that("AI flat orders do not leave stale orders that corrupt account state", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10, fee_rt = 0.0005, auto_register_assets = TRUE))
  sim_agent_add(exchange, agent_id = "Momentum", agent_type = "momentum", config = list(qty = 1))
  sim_agent_add(exchange, agent_id = "Contrarian", agent_type = "contrarian", config = list(qty = 1))
  sim_agent_add(exchange, agent_id = "Reversion", agent_type = "mean_reversion", config = list(qty = 1))
  sim_agent_add(exchange, agent_id = "Chaos", agent_type = "chaos", config = list(qty = 1))

  set.seed(1)
  px <- 100
  for (i in seq_len(12)) {
    open <- px
    close <- px * exp(stats::rnorm(1, 0, 0.02))
    bar <- data.frame(
      timestamp = as.POSIXct("2026-07-08 21:00:00", tz = "UTC") + i * 60,
      open = open,
      high = max(open, close) * 1.005,
      low = min(open, close) * 0.995,
      close = close
    )
    sim_agents_step(exchange, bar)
    sim_exchange_process_commands(exchange)
    sim_exchange_step(exchange, bar)
    account <- sim_exchange_account(exchange)
    expect_true(all(is.finite(account$equity)))
    expect_true(all(is.finite(account$cash)))
    px <- close
  }

  rankings <- sim_agent_rankings(exchange)
  expect_true(all(is.na(rankings$equity) | is.finite(rankings$equity)))
  expect_false(is.na(rankings$equity[1]))
  expect_false(any(exchange$agent_orders$status == "no_op"))
})

test_that("agent rankings sort non-finite account values below finite accounts", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10, auto_register_assets = TRUE))
  sim_agent_add(exchange, agent_id = "finite", agent_type = "momentum")
  sim_agent_add(exchange, agent_id = "broken", agent_type = "chaos")
  exchange$step_snapshots <- data.table::data.table(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
    agent_id = c("finite", "broken"),
    equity = c(10000, NaN),
    cash = c(10000, NaN),
    pos_dir = c(0L, 0L),
    ctr_unit = c(0, 0),
    avg_price = c(NA_real_, NA_real_),
    last_px = c(100, 100),
    notional = c(0, 0),
    abs_notional = c(0, 0),
    unrealized_pnl = c(0, 0),
    maintenance_margin = c(0, 0)
  )
  exchange$result <- exchange$step_snapshots

  rankings <- sim_agent_rankings(exchange)
  expect_equal(rankings$agent_id[1], "finite")
  expect_equal(rankings$equity[rankings$agent_id == "broken"], NA_real_)
  expect_equal(rankings$cash[rankings$agent_id == "broken"], NA_real_)
})

test_that("AI and human agents have separate exchange accounts", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10, auto_register_assets = TRUE))
  ai_id <- sim_agent_add(exchange, agent_id = "ai-momo", agent_type = "momentum", config = list(qty = 1))
  bar1 <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    open = 100,
    high = 101,
    low = 99,
    close = 100
  )
  bar2 <- data.frame(
    timestamp = as.POSIXct("2026-01-01 01:00:00", tz = "UTC"),
    open = 100,
    high = 103,
    low = 100,
    close = 102
  )
  sim_submit_order(exchange, "human-a", timestamp = bar1$timestamp[1], side = "sell", qty = 2, process = TRUE)

  initial_accounts <- sim_exchange_account(exchange)
  expect_setequal(initial_accounts$agent_id, c(ai_id, "human-a"))
  expect_true(all(initial_accounts$cash == 10000))

  sim_exchange_step(exchange, bar1)
  sim_agents_step(exchange, bar2)
  sim_exchange_process_commands(exchange)
  sim_exchange_step(exchange, bar2)

  expect_true(all(c("price", "fee", "realized_pnl") %in% names(exchange$agent_orders)))
  expect_true(is.finite(exchange$agent_orders$price[exchange$agent_orders$agent_id == "human-a"]))

  accounts <- sim_exchange_account(exchange)
  positions <- sim_exchange_positions(exchange)
  expect_setequal(accounts$agent_id, c(ai_id, "human-a"))
  expect_setequal(positions$agent_id, c(ai_id, "human-a"))
  expect_equal(positions$pos_dir[positions$agent_id == "human-a"], -1L)
  expect_equal(positions$pos_dir[positions$agent_id == ai_id], 1L)

  rankings <- sim_agent_rankings(exchange)
  expect_setequal(rankings$agent_id, c(ai_id, "human-a"))
  expect_true(all(c("equity", "cash") %in% names(rankings)))
})

test_that("live service state returns account history for equity curves", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10, auto_register_assets = TRUE))
  sim_agent_add(exchange, agent_id = "agent-history", agent_type = "momentum")
  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC") + 0:2 * 3600,
    open = c(100, 101, 102),
    high = c(101, 102, 103),
    low = c(99, 100, 101),
    close = c(101, 102, 103)
  )
  for (i in seq_len(nrow(bars))) sim_exchange_step(exchange, bars[i, ])

  state <- tradesimr:::.service_state(exchange)
  expect_equal(length(state$account), 3)
  expect_equal(length(state$account_latest), 1)
  expect_equal(vapply(state$account, `[[`, character(1), "agent_id"), rep("agent-history", 3))
  expect_equal(length(state$account[[1]]$agent_id), 1)
  expect_equal(length(state$account[[1]]$equity), 1)
})

test_that("strategy-backed agents submit explicit multi-asset order intents", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10))
  sim_asset_add(exchange, "AAPL", asset_id = 101L, asset_class = "stock")
  sim_asset_add(exchange, "ETH-USDT-SWAP", asset_id = 202L, asset_class = "crypto_perp")
  sim_strategy_register(exchange, "pair-orders", function(market, bar, ...) {
    data.table::data.table(
      symbol = c("AAPL", "ETH-USDT-SWAP"),
      asset_id = c(101L, 202L),
      side = c("buy", "sell"),
      qty = c(1, 2),
      order_type = "market"
    )
  })
  sim_agent_add(exchange, agent_id = "strategy-a", agent_type = "strategy", config = list(strategy_id = "pair-orders"))

  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    symbol = c("AAPL", "ETH-USDT-SWAP"),
    asset_id = c(101L, 202L),
    open = c(100, 2000),
    high = c(101, 2010),
    low = c(99, 1990),
    close = c(100, 2000)
  )
  sim_exchange_add_bars(exchange, bars)
  decisions <- sim_agents_step(exchange, bars)

  expect_equal(nrow(decisions), 2)
  expect_setequal(decisions$symbol, c("AAPL", "ETH-USDT-SWAP"))
  expect_setequal(exchange$order_requests$side, c("buy", "sell"))
  expect_equal(exchange$order_requests$qty_type, rep("contracts", 2))
  expect_equal(sim_strategy_list(exchange), "pair-orders")
  expect_true(sim_strategy_unregister(exchange, "pair-orders"))
})

test_that("strategy-backed agents consume strategyr target-position strategies when available", {
  skip_if_not_installed("strategyr")
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10))
  sim_asset_add(exchange, "BTC-USDT-SWAP", asset_id = 303L, asset_class = "crypto_perp")
  sim_agent_add(
    exchange,
    agent_id = "buy-hold",
    agent_type = "strategy",
    config = list(
      strategy_fun = "strategyr::strat_buy_and_hold_tgt_pos",
      param_value = 1
    )
  )
  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + 0:2 * 3600,
    symbol = "BTC-USDT-SWAP",
    asset_id = 303L,
    open = c(100, 101, 102),
    high = c(101, 102, 103),
    low = c(99, 100, 101),
    close = c(100, 101, 102)
  )
  sim_exchange_add_bars(exchange, bars)
  decisions <- sim_agents_step(exchange, bars[3, ])

  expect_equal(nrow(decisions), 1)
  expect_equal(decisions$side, "target")
  expect_equal(decisions$qty_type, "target_pos")
  expect_equal(decisions$tgt_pos, 1)
  expect_equal(exchange$order_requests$tgt_pos, 1)
  expect_equal(exchange$order_requests$side, "target")
})

test_that("strategy-backed agents isolate errors and record diagnostics", {
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10))
  sim_asset_add(exchange, "AAPL", asset_id = 101L, asset_class = "stock")
  sim_strategy_register(exchange, "broken", function(...) stop("strategy exploded", call. = FALSE))
  sim_strategy_register(exchange, "valid", function(DT, ...) rep(1, nrow(DT)))
  sim_agent_add(exchange, "broken-agent", "strategy", list(strategy_id = "broken"))
  sim_agent_add(exchange, "valid-agent", "strategy", list(strategy_id = "valid"))

  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + 0:1 * 3600,
    symbol = "AAPL",
    asset_id = 101L,
    open = c(100, 101),
    high = c(101, 102),
    low = c(99, 100),
    close = c(100, 101)
  )
  sim_exchange_add_bars(exchange, bars)
  decisions <- sim_agents_step(exchange, bars[2, ])

  expect_equal(nrow(decisions), 1)
  expect_equal(decisions$agent_id, "valid-agent")
  expect_equal(nrow(exchange$order_requests), 1)
  expect_true(any(exchange$agent_strategy_events$agent_id == "broken-agent" & exchange$agent_strategy_events$status == "error"))
  expect_true(any(grepl("strategy exploded", exchange$agent_strategy_events$message)))
  expect_true(any(exchange$agent_strategy_events$stage == "submitted_order"))
})

test_that("strategy config validation rejects unknown params", {
  exchange <- sim_exchange_new()
  sim_strategy_register(exchange, "strict", function(DT, fast = 10) rep(0, nrow(DT)))

  ok <- sim_strategy_validate_config(exchange, list(strategy_id = "strict", param_fast = 5))
  bad <- sim_strategy_validate_config(exchange, list(strategy_id = "strict", param_slow = 20))

  expect_true(ok$valid)
  expect_false(bad$valid)
  expect_match(bad$message, "Unknown strategy parameter")
  expect_error(
    sim_agent_add(exchange, "bad-strategy", "strategy", list(strategy_id = "strict", param_slow = 20)),
    "Unknown strategy parameter"
  )
})

test_that("strategy-backed agents consume strategyr-style multi-asset order intents", {
  skip_if_not_installed("strategyr")
  exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10))
  sim_asset_add(exchange, "AAPL", asset_id = 101L, asset_class = "stock")
  sim_asset_add(exchange, "ETH-USDT-SWAP", asset_id = 202L, asset_class = "crypto_perp")
  sim_strategy_register(exchange, "strategyr-intents", function(market, account, ...) {
    latest <- market[order(timestamp), .SD[.N], by = symbol]
    portfolio <- data.table::data.table(
      asset = latest$symbol,
      price = latest$close,
      current_units = 0,
      target_weight = c(0.25, -0.25),
      contract_size = 1,
      lot_step = 1
    )
    plan <- strategyr::plan_portfolio_adjustment(portfolio, equity = 10000)
    strategyr::build_order_intents(plan, pricing_method = "market")
  })
  sim_agent_add(exchange, "allocator", "strategy", list(strategy_id = "strategyr-intents"))

  bars <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    symbol = c("AAPL", "ETH-USDT-SWAP"),
    asset_id = c(101L, 202L),
    open = c(100, 2000),
    high = c(101, 2010),
    low = c(99, 1990),
    close = c(100, 2000)
  )
  sim_exchange_add_bars(exchange, bars)
  decisions <- sim_agents_step(exchange, bars)

  expect_equal(nrow(decisions), 2)
  expect_setequal(decisions$symbol, c("AAPL", "ETH-USDT-SWAP"))
  expect_true(all(decisions$decision_type == "order"))
  expect_true(any(exchange$agent_strategy_events$output_type == "order_intent_table"))
  expect_equal(nrow(exchange$order_requests), 2)
})
