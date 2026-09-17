v2_isolation_assets <- c("SPY", "BTC-USD")

v2_isolation_bars <- function(day = 0L) {
  prices <- c(100, 1000)
  close <- prices + day * c(2, 20)
  data.table::data.table(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC") + day * 86400,
    symbol = v2_isolation_assets, asset_id = 1:2,
    open = close, high = close + c(1, 10), low = close - c(1, 10), close = close
  )
}

v2_isolation_exchange <- function() {
  exchange <- sim_exchange_new(list(
    cash = 100000, fee_rt = 0.001, lev = 1,
    portfolio_margin = TRUE, execution_engine = "heterogeneous_v2",
    calendar_mode = "raw"
  ))
  sim_asset_add(exchange, "SPY", asset_id = 1L, asset_class = "etf",
    instrument_profile = "etf", quote_ccy = "USD", qty_step = 1)
  sim_asset_add(exchange, "BTC-USD", asset_id = 2L, asset_class = "crypto_spot",
    instrument_profile = "crypto_spot", quote_ccy = "USD", qty_step = .001)
  sim_agent_add(exchange, "only-spy", "human", config = list(initial_cash = 100000))
  sim_agent_add(exchange, "only-btc", "human", config = list(initial_cash = 100000))
  exchange
}

v2_isolation_decisions <- function() {
  list(
    `only-spy` = list(target_weights = c(SPY = .5), allowed_symbols = "SPY"),
    `only-btc` = list(target_weights = c(`BTC-USD` = .5), allowed_symbols = "BTC-USD")
  )
}

test_that("heterogeneous v2 isolates single-asset accounts and marks inventory P&L", {
  exchange <- v2_isolation_exchange()
  first <- v2_isolation_bars(0L)
  execution <- sim_portfolio_execution(fee_rt = .001, lev = 1)
  sim_portfolio_market_step(exchange, first, execution)
  sim_portfolio_target_submit_batch(exchange, first, v2_isolation_decisions(), execution)
  sim_portfolio_market_step(exchange, v2_isolation_bars(1L), execution)
  sim_portfolio_market_step(exchange, v2_isolation_bars(2L), execution)

  latest_position_timestamp <- max(exchange$inventory_positions$timestamp, na.rm = TRUE)
  positions <- exchange$inventory_positions[
    timestamp == latest_position_timestamp & abs(units) > 1e-12
  ]
  actual_keys <- sort(paste(positions$agent_id, positions$symbol, sep = "|"))
  expected_keys <- sort(paste(c("only-spy", "only-btc"), c("SPY", "BTC-USD"), sep = "|"))
  expect_identical(actual_keys, expected_keys)
  expect_equal(exchange$portfolio_fills[, .N, by = agent_id][order(agent_id), N], c(1L, 1L))
  expect_identical(
    unique(as.character(exchange$portfolio_fills$symbol[exchange$portfolio_fills$agent_id == "only-spy"])),
    "SPY"
  )
  expect_identical(
    unique(as.character(exchange$portfolio_fills$symbol[exchange$portfolio_fills$agent_id == "only-btc"])),
    "BTC-USD"
  )

  latest_snapshot_timestamp <- max(exchange$step_snapshots$timestamp, na.rm = TRUE)
  snapshots <- exchange$step_snapshots[
    timestamp == latest_snapshot_timestamp & accounting_model == "spot_inventory" &
      !is.na(ctr_unit) & abs(ctr_unit) > 1e-12
  ]
  snapshot_keys <- sort(paste(snapshots$agent_id, snapshots$symbol, sep = "|"))
  expect_identical(snapshot_keys, actual_keys)
  marked <- exchange$inventory_positions[
    timestamp == latest_position_timestamp & abs(units) > 1e-12
  ]
  expected_pnl <- (marked$last_price - marked$average_cost) * marked$units * marked$contract_size
  actual_pnl <- vapply(seq_len(nrow(marked)), function(i) {
    snapshots[agent_id == marked$agent_id[i] & asset_id == marked$asset_id[i], unrealized_pnl][1L]
  }, numeric(1L))
  expect_equal(actual_pnl, expected_pnl)
})

test_that("incremental and bulk typed v2 replay preserve isolated accounts", {
  bars <- data.table::rbindlist(lapply(0:2, v2_isolation_bars))
  panel <- data.table::rbindlist(lapply(unique(bars$timestamp), function(timestamp) {
    data.table::data.table(
      timestamp = timestamp,
      agent_id = c("only-spy", "only-btc"),
      symbol = c("SPY", "BTC-USD"),
      target_weight = if (as.integer(timestamp) %% (2 * 86400) == 0) c(.5, .5) else c(.6, .6),
      decision_label = "target_weight"
    )
  }))
  execution <- sim_portfolio_execution(fee_rt = .001, lev = 1)
  sequential <- v2_isolation_exchange()
  for (timestamp in unique(bars$timestamp)) {
      current_timestamp <- timestamp
      boundary <- bars[as.numeric(timestamp) == as.numeric(current_timestamp)]
    sim_portfolio_market_step(sequential, boundary, execution)
    weights <- panel[timestamp == current_timestamp, target_weight]
    decisions <- list(
      `only-btc` = list(target_weights = c(`BTC-USD` = weights[2L]), allowed_symbols = "BTC-USD"),
      `only-spy` = list(target_weights = c(SPY = weights[1L]), allowed_symbols = "SPY")
    )
    sim_portfolio_target_submit_batch(sequential, boundary, decisions, execution)
  }
  bulk <- sim_portfolio_target_replay(
    v2_isolation_exchange(), bars, panel,
    allowed_symbols = list(`only-spy` = "SPY", `only-btc` = "BTC-USD"),
    execution = execution
  )$exchange
  canonical <- function(x) {
    x <- data.table::as.data.table(x)
    for (name in names(x)) if (inherits(x[[name]], "POSIXt")) data.table::set(x, j = name, value = as.numeric(x[[name]]))
    sort_columns <- intersect(c("agent_id", "asset_id", "timestamp", "order_id", "rebalance_id", "symbol"), names(x))
    if (length(sort_columns)) data.table::setorderv(x, sort_columns)
    x[]
  }
  for (name in c("agent_orders", "portfolio_fills", "inventory_positions",
                 "cash_balances", "portfolio_targets", "portfolio_rebalances")) {
    expect_equal(canonical(bulk[[name]]), canonical(sequential[[name]]), info = name)
  }
  expect_equal(canonical(sim_portfolio_execution_quality(bulk)),
    canonical(sim_portfolio_execution_quality(sequential)))
})

test_that("typed v2 isolation survives CSV save/load and public export", {
  exchange <- v2_isolation_exchange()
  first <- v2_isolation_bars(0L)
  execution <- sim_portfolio_execution(fee_rt = .001, lev = 1)
  sim_portfolio_market_step(exchange, first, execution)
  sim_portfolio_target_submit_batch(exchange, first, v2_isolation_decisions(), execution)
  sim_portfolio_market_step(exchange, v2_isolation_bars(1L), execution)
  state_path <- tempfile("tradesimr-v2-isolation-state-")
  sim_exchange_save(exchange, state_path, format = "csv")
  restored <- sim_exchange_load(state_path)
  canonical <- function(x) {
    x <- data.table::as.data.table(x)
    for (name in names(x)) if (inherits(x[[name]], "POSIXt")) data.table::set(x, j = name, value = as.numeric(x[[name]]))
    x[]
  }
  expect_equal(canonical(restored$inventory_positions), canonical(exchange$inventory_positions))
  expect_equal(canonical(restored$portfolio_fills), canonical(exchange$portfolio_fills))
  export_path <- tempfile("tradesimr-v2-isolation-export-")
  exported <- sim_portfolio_export(restored, "only-spy", export_path)
  expect_true(all(file.exists(unname(exported))))
  exported_positions <- jsonlite::read_json(exported[["positions"]], simplifyVector = TRUE)
  exported_positions <- data.table::as.data.table(exported_positions)
  expect_true(all(exported_positions$symbol %in% "SPY"))
  expect_false(any(exported_positions$symbol == "BTC-USD"))
})
