performance_symbols <- c("SPY", "TLT", "GLD", "EURUSD=X", "BTC-USD", "USO", "EFA", "IWM")

performance_exchange <- function() {
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE))
  for (i in seq_along(performance_symbols)) {
    sim_asset_add(
      exchange, performance_symbols[i], asset_id = i,
      contract_size = if (performance_symbols[i] == "EURUSD=X") .1 else 1,
      qty_step = if (performance_symbols[i] == "BTC-USD") .001 else 1
    )
  }
  for (agent_id in c("balanced", "momentum", "defensive")) sim_agent_add(exchange, agent_id, "human")
  exchange
}

performance_fixture <- function(n_boundaries) {
  prices <- c(600, 90, 250, 1.1, 65000, 80, 90, 220)
  weights <- list(
    balanced = c(SPY = .25, TLT = .2, GLD = .1, `EURUSD=X` = .05, `BTC-USD` = .1, USO = .05, EFA = .15, IWM = .1),
    momentum = c(SPY = .45, TLT = .1, GLD = .1, `EURUSD=X` = .05, `BTC-USD` = .1, USO = .05, EFA = .1, IWM = .05),
    defensive = c(SPY = .1, TLT = .35, GLD = .2, `EURUSD=X` = .1, `BTC-USD` = .025, USO = .025, EFA = .1, IWM = .1)
  )
  bars <- data.table::rbindlist(lapply(seq_len(n_boundaries), function(day) {
    close <- prices * exp(.001 * day + seq_along(prices) * .0001)
    data.table::data.table(
      timestamp = as.POSIXct("2026-08-04", tz = "UTC") + day * 86400,
      symbol = performance_symbols, asset_id = seq_along(performance_symbols),
      open = close * .999, high = close * 1.002, low = close * .998, close = close
    )
  }))
  targets <- data.table::rbindlist(lapply(unique(bars$timestamp), function(timestamp) {
    data.table::rbindlist(lapply(names(weights), function(agent_id) {
      current <- weights[[agent_id]]
      if (agent_id == "momentum" && as.integer(timestamp) %% 2L == 0L) current[c("SPY", "TLT")] <- c(.1, .45)
      data.table::data.table(timestamp = timestamp, agent_id = agent_id, symbol = names(current), target_weight = unname(current))
    }))
  }))
  list(
    bars = bars,
    targets = targets,
    allowed_symbols = stats::setNames(rep(list(performance_symbols), length(weights)), names(weights))
  )
}

performance_required_timings <- c(
  "orchestration", "portfolio_step_rcpp", "ledger", "boundary_normalization",
  "boundary_bar_slicing", "target_panel_slicing", "account_position_lookup",
  "target_planning", "exchange_state_updates", "durable_append_bind",
  "supersession_checks", "boundary_snapshot_bookkeeping", "execution_quality",
  "serialization_export", "order_fill_event_ledger_writes", "snapshot_construction",
  "execution_quality_joins", "boundary_latency_seconds", "wall_time"
)

test_that("bulk replay profiling exposes stable, finite phase timings", {
  skip_on_cran()
  fixture <- performance_fixture(8L)
  result <- sim_portfolio_target_replay(
    performance_exchange(), fixture$bars, fixture$targets, fixture$allowed_symbols,
    execution = sim_portfolio_execution(fee_rt = .0007, lev = 1), profile = TRUE
  )
  expect_true(all(performance_required_timings %in% names(result$timings)))
  expect_true(all(is.finite(unlist(result$timings[performance_required_timings]))))
  expect_true(all(unlist(result$timings[performance_required_timings]) >= 0))
  expect_gt(result$timings$wall_time, 0)
  expect_gte(nrow(result$quality), nrow(result$rebalances))
})

test_that("optional Vox-scale benchmark reports a configurable replay budget", {
  skip_if_not(identical(tolower(Sys.getenv("TRADESIMR_RUN_PERF_TESTS", "false")), "true"))
  fixture <- performance_fixture(252L)
  result <- sim_portfolio_target_replay(
    performance_exchange(), fixture$bars, fixture$targets, fixture$allowed_symbols,
    execution = sim_portfolio_execution(fee_rt = .0007, lev = 1), profile = TRUE
  )
  budget <- suppressWarnings(as.numeric(Sys.getenv("TRADESIMR_MAX_BULK_REPLAY_SECONDS", "NA")))
  if (is.finite(budget)) expect_lte(result$timings$wall_time, budget)
  expect_true(all(is.finite(unlist(result$timings[performance_required_timings]))))
  expect_gt(nrow(result$orders), 0L)
  expect_gt(nrow(result$quality), 0L)
})

test_that("Vox-shaped benchmark fixture records local profiling metrics", {
  skip_on_cran()
  script <- system.file("examples", "vox_arena_replay_benchmark.R", package = "tradesimr")
  source(script, local = TRUE)
  artifact_path <- tempfile("tradesimr-vox-benchmark-")
  result <- run_vox_arena_replay_benchmark(
    n_days = 1L, use_bulk = TRUE, profile = TRUE,
    fixture = "vox", artifact_path = artifact_path
  )
  expect_equal(result$n_assets, 8L)
  expect_equal(result$n_deterministic_agents, 64L)
  expect_equal(result$n_llm_accounts, 2L)
  expect_equal(length(result$metrics$per_boundary_latency_seconds), 1L)
  expect_true(file.exists(file.path(artifact_path, "benchmark_metrics.rds")))
  expect_true(file.exists(file.path(artifact_path, "benchmark_timings.csv")))
  expect_true(file.exists(file.path(artifact_path, "boundary_latency.csv")))
})
