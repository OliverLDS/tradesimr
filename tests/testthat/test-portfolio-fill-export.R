test_that("portfolio fill exports retain durable order and rebalance links", {
  skip_if_not_installed("jsonlite")
  exchange <- sim_exchange_new(list(cash = 100000, ctr_step = 1, lev = 1))
  symbols <- c("SPY", "TLT", "GLD", "EURUSD=X", "BTC-USD")
  prices <- c(100, 50, 200, 2, 5000)
  for (i in seq_along(symbols)) {
    sim_asset_add(exchange, symbols[i], asset_id = i, qty_step = 1)
  }
  execution <- sim_portfolio_execution(fee_rt = 0.001, lev = 1)
  first <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
    symbol = symbols,
    asset_id = seq_along(symbols),
    open = prices, high = prices, low = prices, close = prices
  )
  submitted <- sim_portfolio_target_step(
    exchange, "vox-candidate", first, stats::setNames(rep(0.2, length(symbols)), symbols), execution
  )
  second <- first
  second$timestamp <- second$timestamp + 86400
  filled <- sim_portfolio_target_step(exchange, "vox-candidate", second, NULL, execution)

  required <- c(
    "fill_id", "agent_id", "order_id", "rebalance_id", "timestamp", "symbol",
    "asset_id", "side", "action", "status", "qty", "price", "fee", "target_weight"
  )
  expect_equal(nrow(exchange$portfolio_fills), length(symbols))
  expect_true(all(required %in% names(filled$fills)))
  expect_equal(nrow(filled$fills), length(symbols))
  expect_true(all(filled$fills$status == "filled"))
  expect_true(all(filled$fills$order_id %in% submitted$orders$order_id))
  expect_true(all(filled$fills$rebalance_id %in% exchange$portfolio_rebalances$rebalance_id))
  expect_equal(length(unique(filled$fills$fill_id)), length(symbols))
  expect_equal(length(unique(filled$fills$order_id)), length(symbols))

  state_dir <- tempfile("tradesimr-portfolio-fill-state-")
  sim_exchange_save(exchange, state_dir)
  loaded <- sim_exchange_load(state_dir)
  expect_equal(names(loaded$portfolio_fills), names(exchange$portfolio_fills))
  expect_equal(as.numeric(loaded$portfolio_fills$timestamp), as.numeric(exchange$portfolio_fills$timestamp))
  expect_equal(loaded$portfolio_fills[, !"timestamp"], exchange$portfolio_fills[, !"timestamp"])

  export_dir <- tempfile("tradesimr-portfolio-fill-export-")
  paths <- sim_portfolio_export(loaded, "vox-candidate", export_dir)
  exported <- data.table::as.data.table(jsonlite::read_json(paths[["fills"]], simplifyDataFrame = TRUE))
  exported_orders <- data.table::as.data.table(jsonlite::read_json(paths[["orders"]], simplifyDataFrame = TRUE))
  exported_rebalances <- data.table::as.data.table(jsonlite::read_json(paths[["rebalances"]], simplifyDataFrame = TRUE))
  expect_equal(nrow(exported), length(symbols))
  expect_true(all(required %in% names(exported)))
  expect_true(all(exported$order_id %in% exported_orders[status == "filled", order_id]))
  expect_true(all(exported$rebalance_id %in% exported_rebalances$rebalance_id))
})
