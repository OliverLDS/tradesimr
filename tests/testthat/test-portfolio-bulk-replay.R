skip_on_cran()

bulk_replay_symbols <- c("SPY", "TLT", "GLD", "EURUSD=X", "BTC-USD", "USO", "EFA", "IWM")

bulk_replay_exchange <- function() {
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE))
  for (i in seq_along(bulk_replay_symbols)) {
    sim_asset_add(
      exchange, bulk_replay_symbols[i], asset_id = i,
      contract_size = if (bulk_replay_symbols[i] == "EURUSD=X") 0.1 else 1,
      qty_step = if (bulk_replay_symbols[i] == "BTC-USD") 0.001 else 1
    )
  }
  for (agent_id in c("balanced", "momentum", "defensive")) sim_agent_add(exchange, agent_id, "human")
  exchange
}

bulk_replay_bars <- function(n_days = 6L) {
  prices <- c(600, 90, 250, 1.1, 65000, 80, 90, 220)
  data.table::rbindlist(lapply(seq_len(n_days), function(day) {
    close <- prices * exp(0.002 * day + seq_along(prices) * 0.0001)
    data.table::data.table(
      timestamp = as.POSIXct("2026-08-04", tz = "UTC") + day * 86400,
      symbol = bulk_replay_symbols, asset_id = seq_along(bulk_replay_symbols),
      open = close * .999, high = close * 1.002, low = close * .998, close = close
    )
  }))
}

bulk_replay_panel <- function(bars) {
  weights <- list(
    balanced = c(SPY = .25, TLT = .2, GLD = .1, `EURUSD=X` = .05, `BTC-USD` = .1, USO = .05, EFA = .15, IWM = .1),
    momentum = c(SPY = .45, TLT = .1, GLD = .1, `EURUSD=X` = .05, `BTC-USD` = .1, USO = .05, EFA = .1, IWM = .05),
    defensive = c(SPY = .1, TLT = .35, GLD = .2, `EURUSD=X` = .1, `BTC-USD` = .025, USO = .025, EFA = .1, IWM = .1)
  )
  timestamps <- unique(bars$timestamp)
  data.table::rbindlist(lapply(seq_along(timestamps), function(i) {
    data.table::rbindlist(lapply(names(weights), function(agent_id) {
      current <- weights[[agent_id]]
      if (agent_id == "momentum" && i %% 2L == 0L) current[c("SPY", "TLT")] <- c(.1, .45)
      data.table::data.table(
        timestamp = timestamps[i], agent_id = agent_id, symbol = names(current),
        target_weight = unname(current), decision_label = "deterministic"
      )
    }))
  }))
}

bulk_replay_sequential <- function(exchange, bars, panel, execution) {
  for (current_timestamp in unique(bars$timestamp)) {
    boundary <- bars[as.numeric(timestamp) == as.numeric(current_timestamp)]
    sim_portfolio_market_step(exchange, boundary, execution)
    rows <- panel[as.numeric(timestamp) == as.numeric(current_timestamp)]
    decisions <- lapply(split(rows, rows$agent_id), function(agent_rows) list(
      target_weights = stats::setNames(agent_rows$target_weight, agent_rows$symbol),
      allowed_symbols = bulk_replay_symbols, decision_label = agent_rows$decision_label[1L]
    ))
    sim_portfolio_target_submit_batch(exchange, boundary, decisions, execution)
  }
  exchange
}

bulk_replay_tables <- function(exchange) {
  list(
    orders = exchange$agent_orders,
    fills = exchange$portfolio_fills,
    positions = sim_exchange_positions(exchange),
    account = sim_exchange_account(exchange),
    targets = exchange$portfolio_targets,
    rebalances = exchange$portfolio_rebalances,
    quality = sim_portfolio_execution_quality(exchange),
    snapshots = exchange$step_snapshots,
    events = exchange$step_events
  )
}

bulk_replay_persisted <- function(table) {
  table <- data.table::copy(table)
  for (column in names(table)) {
    if (inherits(table[[column]], "POSIXt")) data.table::set(table, j = column, value = as.numeric(table[[column]]))
  }
  for (attribute in setdiff(names(attributes(table)), c("names", "row.names", "class", ".internal.selfref"))) {
    data.table::setattr(table, attribute, NULL)
  }
  table
}

test_that("bulk target replay preserves eight-asset three-agent durable ledger parity", {
  bars <- bulk_replay_bars()
  panel <- bulk_replay_panel(bars)
  execution <- sim_portfolio_execution(fee_rt = .0007, lev = 1)
  allowed <- stats::setNames(rep(list(bulk_replay_symbols), 3L), c("balanced", "momentum", "defensive"))
  export_path <- tempfile("tradesimr-bulk-replay-export-")

  sequential <- bulk_replay_sequential(bulk_replay_exchange(), bars, panel, execution)
  replay <- sim_portfolio_target_replay(
    bulk_replay_exchange(), bars, panel, allowed, execution,
    export_path = export_path, profile = TRUE
  )
  bulk <- replay$exchange

  expected <- bulk_replay_tables(sequential)
  actual <- bulk_replay_tables(bulk)
  expect_equal(actual, expected)
  expect_true(all(actual$orders$eligible_after == actual$orders$timestamp))
  expect_true(all(actual$fills$timestamp > actual$fills$decision_timestamp))
  expect_true(all(actual$orders$qty_step[actual$orders$symbol == "BTC-USD"] == .001))
  expect_true(all(actual$orders$contract_size[actual$orders$symbol == "EURUSD=X"] == .1))
  expect_true(all(file.exists(unlist(replay$exports, use.names = FALSE))))
  expect_true(all(c("orchestration", "portfolio_step_rcpp", "ledger", "execution_quality", "serialization_export", "wall_time") %in% names(replay$timings)))
  state_path <- tempfile("tradesimr-bulk-replay-state-")
  sim_exchange_save(bulk, state_path)
  loaded <- sim_exchange_load(state_path)
  expect_equal(bulk_replay_persisted(loaded$agent_orders), bulk_replay_persisted(bulk$agent_orders))
  expect_equal(bulk_replay_persisted(loaded$portfolio_fills), bulk_replay_persisted(bulk$portfolio_fills))
  expect_equal(bulk_replay_persisted(loaded$portfolio_targets), bulk_replay_persisted(bulk$portfolio_targets))
  expect_equal(bulk_replay_persisted(loaded$step_snapshots), bulk_replay_persisted(bulk$step_snapshots))
})

test_that("bulk replay retains allowed universes and sparse-target supersession", {
  exchange <- bulk_replay_exchange()
  bars <- bulk_replay_bars(3L)
  # The middle boundary advances TLT only, so the first SPY target remains
  # pending until the newer SPY target can supersede it.
  boundaries <- unique(bars$timestamp)
  bars <- data.table::rbindlist(list(
    bars[timestamp == boundaries[1L] & symbol == "SPY"],
    bars[timestamp == boundaries[2L] & symbol == "TLT"],
    bars[timestamp == boundaries[3L] & symbol == "SPY"]
  ))
  panel <- data.table::data.table(
    timestamp = c(bars$timestamp[1L], bars$timestamp[2L]),
    agent_id = "single", symbol = "SPY", target_weight = c(.5, 1)
  )
  sim_agent_add(exchange, "single", "human")
  bulk <- sim_portfolio_target_replay(
    exchange, bars, panel, allowed_symbols = list(single = "SPY"),
    execution = sim_portfolio_execution(lev = 1)
  )$exchange
  expect_setequal(bulk$agent_orders[agent_id == "single", symbol], "SPY")
  expect_equal(bulk$agent_orders[agent_id == "single" & rebalance_id == "RB000001", status], "superseded")
  expect_equal(bulk$portfolio_fills[agent_id == "single", rebalance_id], "RB000002")
})

test_that("bulk replay retains target no-op outcomes", {
  bars <- bulk_replay_bars(1L)[symbol == "SPY"]
  panel <- data.table::data.table(
    timestamp = bars$timestamp, agent_id = "flat", symbol = "SPY", target_weight = 0
  )
  exchange <- bulk_replay_exchange()
  sim_agent_add(exchange, "flat", "human")
  result <- sim_portfolio_target_replay(
    exchange, bars, panel, allowed_symbols = list(flat = "SPY"),
    execution = sim_portfolio_execution(lev = 1)
  )
  expect_equal(result$rebalances$status, "no_op")
  expect_equal(result$quality$execution_quality, "no_op")
  expect_equal(nrow(result$orders), 0L)
})

test_that("policy-aware bulk replay matches sparse sequential Arena decisions", {
  bars <- bulk_replay_bars(5L)
  timestamps <- unique(bars$timestamp)
  panel <- data.table::data.table(
    timestamp = timestamps,
    agent_id = "policy",
    symbol = "SPY",
    target_weight = c(.5, .5, .5, .6, .6),
    # The third row is due and unchanged, but its post-market drift is nonzero.
    # The fifth row is not due, so it remains an absent decision.
    rebalance_due = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    decision_label = "deterministic"
  )
  execution <- sim_portfolio_execution(fee_rt = .0007, lev = 1)
  sequential <- bulk_replay_exchange()
  sim_agent_add(sequential, "policy", "human")
  for (i in seq_along(timestamps)) {
    boundary <- bars[as.numeric(timestamp) == as.numeric(timestamps[i])]
    sim_portfolio_market_step(sequential, boundary, execution)
    if (i %in% c(1L, 3L, 4L)) {
      sim_portfolio_target_submit_batch(sequential, boundary, list(policy = list(
        target_weights = c(SPY = panel$target_weight[i]),
        allowed_symbols = "SPY", decision_label = "deterministic"
      )), execution)
    }
  }

  bulk_exchange <- bulk_replay_exchange()
  sim_agent_add(bulk_exchange, "policy", "human")
  sequential_export <- tempfile("tradesimr-policy-sequential-")
  bulk_export <- tempfile("tradesimr-policy-bulk-")
  sim_portfolio_export(sequential, "policy", sequential_export)
  bulk <- sim_portfolio_target_replay(
    bulk_exchange, bars, panel,
    allowed_symbols = list(policy = "SPY"), execution = execution,
    rebalance_policy = list(drift_tolerance = 0), export_path = bulk_export
  )$exchange

  expect_equal(bulk_replay_tables(bulk), bulk_replay_tables(sequential))
  expect_equal(nrow(bulk$portfolio_rebalances), 3L)
  expect_equal(nrow(bulk$portfolio_targets), 3L)
  expect_equal(as.numeric(bulk$portfolio_rebalances$timestamp), as.numeric(timestamps[c(1L, 3L, 4L)]))
  expect_equal(bulk$agent_orders$status, c("filled", "filled"))
  expect_equal(sim_portfolio_execution_quality(bulk), sim_portfolio_execution_quality(sequential))
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    for (name in c("orders", "fills", "positions", "valuations", "account", "targets", "rebalances", "realized_weights")) {
      expect_equal(
        jsonlite::read_json(file.path(bulk_export, "policy", paste0(name, ".json")), simplifyVector = TRUE),
        jsonlite::read_json(file.path(sequential_export, paste0(name, ".json")), simplifyVector = TRUE)
      )
    }
  }
})
