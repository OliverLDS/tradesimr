# Vox Arena clean-replay benchmark
#
# Run with:
# source(system.file("examples", "vox_arena_replay_benchmark.R", package = "tradesimr"))
# result <- run_vox_arena_replay_benchmark(n_days = 252, use_bulk = TRUE, profile = TRUE)
# result$timings

run_vox_arena_replay_benchmark <- function(n_days = 252L,
                                           use_bulk = TRUE,
                                           profile = TRUE,
                                           export_path = NULL,
                                           fixture = c("allocation", "vox"),
                                           artifact_path = NULL,
                                           memory_profile = FALSE,
                                           copy_diagnostics = FALSE) {
  stopifnot(length(n_days) == 1L, is.finite(n_days), n_days > 0)
  fixture <- match.arg(fixture)
  symbols <- c("SPY", "TLT", "GLD", "EURUSD=X", "BTC-USD", "USO", "EFA", "IWM")
  prices <- c(600, 90, 250, 1.1, 65000, 80, 90, 220)
  execution <- sim_portfolio_execution(fee_rt = 0.0007, lev = 1)
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE))
  for (i in seq_along(symbols)) {
    sim_asset_add(exchange, symbols[i], asset_id = i, qty_step = if (symbols[i] == "BTC-USD") 0.001 else 1)
  }
  deterministic_agents <- if (fixture == "allocation") {
    c("balanced", "momentum", "defensive")
  } else {
    as.vector(outer(
      c("buy_hold", "momentum", "trend", "value", "risk", "carry", "breakout", "defensive"),
      symbols, paste, sep = "--"
    ))
  }
  for (agent_id in deterministic_agents) sim_agent_add(exchange, agent_id, "human")
  sim_agent_add(exchange, "llm-a", "human")
  sim_agent_add(exchange, "llm-b", "human")

  bars_for_day <- function(day) {
    close <- prices * exp(0.002 * day + seq_along(prices) * 0.0001)
    data.frame(
      timestamp = as.POSIXct("2026-08-04", tz = "UTC") + day * 86400,
      symbol = symbols, asset_id = seq_along(symbols),
      open = close * 0.999, high = close * 1.002, low = close * 0.998, close = close
    )
  }
  target_weights_for_boundary <- function(day) {
    momentum <- if ((day %% 20L) < 10L) c(SPY = .45, GLD = .1, TLT = .1, `EURUSD=X` = .05, `BTC-USD` = .1, USO = .05, EFA = .1, IWM = .05) else
      c(SPY = .1, GLD = .2, TLT = .25, `EURUSD=X` = .05, `BTC-USD` = .05, USO = .05, EFA = .15, IWM = .15)
    allocation_targets <- list(
      balanced = c(SPY = .25, TLT = .2, GLD = .1, `EURUSD=X` = .05, `BTC-USD` = .1, USO = .05, EFA = .15, IWM = .1),
      momentum = momentum,
      defensive = c(SPY = .1, TLT = .35, GLD = .2, `EURUSD=X` = .1, `BTC-USD` = .025, USO = .025, EFA = .1, IWM = .1)
    )
    if (fixture == "allocation") return(allocation_targets)
    single_asset <- stats::setNames(lapply(deterministic_agents, function(agent_id) {
      symbol <- sub("^.*--", "", agent_id)
      # Deterministic single-asset competitors retain a fixed allocation but
      # alternate a small subset to exercise target changes and supersession.
      weight <- if (grepl("momentum|trend", agent_id) && (day %% 20L) >= 10L) .25 else .5
      stats::setNames(weight, symbol)
    }), deterministic_agents)
    c(single_asset, list(
      `llm-a` = allocation_targets$balanced,
      `llm-b` = allocation_targets$defensive
    ))
  }

  bars <- do.call(rbind, lapply(seq_len(as.integer(n_days)), bars_for_day))
  targets <- data.table::rbindlist(lapply(seq_len(as.integer(n_days)), function(day) {
    weights <- target_weights_for_boundary(day)
    data.table::rbindlist(lapply(names(weights), function(agent_id) {
      data.table::data.table(
        timestamp = bars_for_day(day)$timestamp[1L], agent_id = agent_id,
        symbol = names(weights[[agent_id]]), target_weight = unname(weights[[agent_id]]),
        decision_label = "deterministic"
      )
    }))
  }))
  allowed <- if (fixture == "allocation") {
    stats::setNames(rep(list(symbols), length(deterministic_agents)), deterministic_agents)
  } else {
    out <- stats::setNames(lapply(deterministic_agents, function(agent_id) sub("^.*--", "", agent_id)), deterministic_agents)
    c(out, list(`llm-a` = symbols, `llm-b` = symbols))
  }
  gc(reset = TRUE)
  memory_profile_file <- if (isTRUE(memory_profile)) tempfile("tradesimr-rprof-", fileext = ".out") else NULL
  allocation_profile_file <- if (isTRUE(memory_profile)) tempfile("tradesimr-rprofmem-", fileext = ".out") else NULL
  if (isTRUE(memory_profile)) {
    Rprof(memory_profile_file, interval = 0.01, memory.profiling = TRUE)
    try(Rprofmem(allocation_profile_file, threshold = 65536), silent = TRUE)
    on.exit({
      Rprof(NULL)
      try(Rprofmem(NULL), silent = TRUE)
    }, add = TRUE)
  }
  ledger_names <- c("agent_orders", "portfolio_targets", "portfolio_rebalances", "portfolio_fills", "step_events", "step_snapshots")
  ledger_addresses_before <- stats::setNames(vapply(ledger_names, function(name) {
    data.table::address(exchange[[name]])
  }, character(1L)), ledger_names)
  trace_tokens <- if (isTRUE(copy_diagnostics)) lapply(ledger_names, function(name) tracemem(exchange[[name]])) else NULL
  if (isTRUE(use_bulk)) {
    result <- sim_portfolio_target_replay(
      exchange = exchange,
      bars = bars,
      target_weights = targets,
      allowed_symbols = allowed,
      execution = execution,
      export_path = export_path,
      profile = profile
    )
  } else {
    timings <- new.env(parent = emptyenv())
    timings$portfolio_step_rcpp <- 0
    timings$ledger <- 0
    exchange$.profile_timings <- if (isTRUE(profile)) timings else NULL
    on.exit(exchange$.profile_timings <- NULL, add = TRUE)
    started <- proc.time()[["elapsed"]]
    for (day in seq_len(as.integer(n_days))) {
      boundary_started <- proc.time()[["elapsed"]]
      boundary <- bars[as.numeric(bars$timestamp) == as.numeric(bars_for_day(day)$timestamp[1L]), ]
      sim_portfolio_market_step(exchange, boundary, execution)
      weights <- target_weights_for_boundary(day)
      decisions <- stats::setNames(lapply(names(weights), function(agent_id) list(
        target_weights = weights[[agent_id]], allowed_symbols = symbols, decision_label = "deterministic"
      )), names(weights))
      sim_portfolio_target_submit_batch(exchange, boundary, decisions, execution)
      timings$orchestration <- (timings$orchestration %||% 0) + (proc.time()[["elapsed"]] - boundary_started)
    }
    quality_started <- proc.time()[["elapsed"]]
    quality <- sim_portfolio_execution_quality(exchange)
    timings$execution_quality <- proc.time()[["elapsed"]] - quality_started
    exports <- list()
    if (!is.null(export_path)) {
      export_started <- proc.time()[["elapsed"]]
      for (agent_id in deterministic_agents) {
        exports[[agent_id]] <- sim_portfolio_export(exchange, agent_id, file.path(export_path, agent_id))
      }
      timings$serialization_export <- proc.time()[["elapsed"]] - export_started
    } else {
      timings$serialization_export <- 0
    }
    timings$wall_time <- proc.time()[["elapsed"]] - started
    if (isTRUE(profile)) {
      timings$orchestration <- max(0, timings$orchestration - timings$portfolio_step_rcpp - timings$ledger)
    }
    result <- list(exchange = exchange, quality = quality, exports = exports, timings = as.list(timings))
  }
  if (isTRUE(memory_profile)) {
    Rprof(NULL)
    try(Rprofmem(NULL), silent = TRUE)
  }
  if (isTRUE(copy_diagnostics)) {
    for (i in seq_along(ledger_names)) try(untracemem(exchange[[ledger_names[i]]]), silent = TRUE)
  }
  gc_stats <- gc()
  max_used_column <- match("max used", colnames(gc_stats))
  peak_memory_mb <- if (!is.na(max_used_column) && max_used_column < ncol(gc_stats)) {
    sum(as.numeric(gc_stats[, max_used_column + 1L]), na.rm = TRUE)
  } else {
    NA_real_
  }
  allocation_lines <- if (!is.null(allocation_profile_file) && file.exists(allocation_profile_file)) readLines(allocation_profile_file, warn = FALSE) else character()
  rprof_lines <- if (!is.null(memory_profile_file) && file.exists(memory_profile_file)) readLines(memory_profile_file, warn = FALSE) else character()
  boundary_latency <- result$timings$boundary_latency_seconds
  if (is.null(boundary_latency)) boundary_latency <- numeric()
  ledger_addresses_after <- stats::setNames(vapply(ledger_names, function(name) {
    data.table::address(result$exchange[[name]])
  }, character(1L)), ledger_names)
  metrics <- list(
    wall_time_seconds = as.numeric(result$timings$wall_time),
    per_boundary_latency_seconds = as.numeric(boundary_latency),
    peak_memory_mb = peak_memory_mb,
    gc_samples = sum(grepl("<GC>", rprof_lines, fixed = TRUE)),
    large_allocation_samples = length(allocation_lines),
    ledger_addresses_before = ledger_addresses_before,
    ledger_addresses_after = ledger_addresses_after,
    ledger_replaced = ledger_addresses_before != ledger_addresses_after,
    tracemem_enabled = isTRUE(copy_diagnostics),
    rprof_memory_file = memory_profile_file,
    rprofmem_file = allocation_profile_file
  )
  if (!is.null(artifact_path)) {
    dir.create(artifact_path, recursive = TRUE, showWarnings = FALSE)
    if (!is.null(memory_profile_file) && file.exists(memory_profile_file)) {
      profile_destination <- file.path(artifact_path, "replay_rprof.out")
      file.copy(memory_profile_file, profile_destination, overwrite = TRUE)
      metrics$rprof_memory_file <- profile_destination
    }
    if (!is.null(allocation_profile_file) && file.exists(allocation_profile_file)) {
      allocation_destination <- file.path(artifact_path, "replay_rprofmem.out")
      file.copy(allocation_profile_file, allocation_destination, overwrite = TRUE)
      metrics$rprofmem_file <- allocation_destination
    }
    saveRDS(metrics, file.path(artifact_path, "benchmark_metrics.rds"))
    scalar_timings <- vapply(result$timings, function(value) {
      if (length(value) == 1L) as.numeric(value) else NA_real_
    }, numeric(1L))
    utils::write.csv(data.frame(phase = names(scalar_timings), seconds = scalar_timings),
      file.path(artifact_path, "benchmark_timings.csv"), row.names = FALSE)
    utils::write.csv(data.frame(boundary = seq_along(metrics$per_boundary_latency_seconds),
      seconds = metrics$per_boundary_latency_seconds),
      file.path(artifact_path, "boundary_latency.csv"), row.names = FALSE)
  }
  c(result, list(
    n_assets = length(symbols),
    n_deterministic_agents = length(deterministic_agents),
    n_llm_accounts = 2L,
    n_days = as.integer(n_days),
    use_bulk = isTRUE(use_bulk),
    fixture = fixture,
    metrics = metrics
  ))
}
