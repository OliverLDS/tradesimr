# Vox Arena clean-replay benchmark
#
# Run with:
# source(system.file("examples", "vox_arena_replay_benchmark.R", package = "tradesimr"))
# result <- run_vox_arena_replay_benchmark(n_days = 252, use_bulk = TRUE, profile = TRUE)
# result$timings

run_vox_arena_replay_benchmark <- function(n_days = 252L,
                                           use_bulk = TRUE,
                                           profile = TRUE,
                                           export_path = NULL) {
  stopifnot(length(n_days) == 1L, is.finite(n_days), n_days > 0)
  symbols <- c("SPY", "TLT", "GLD", "EURUSD=X", "BTC-USD", "USO", "EFA", "IWM")
  prices <- c(600, 90, 250, 1.1, 65000, 80, 90, 220)
  execution <- sim_portfolio_execution(fee_rt = 0.0007, lev = 1)
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE))
  for (i in seq_along(symbols)) {
    sim_asset_add(exchange, symbols[i], asset_id = i, qty_step = if (symbols[i] == "BTC-USD") 0.001 else 1)
  }
  deterministic_agents <- c("balanced", "momentum", "defensive")
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
    list(
      balanced = c(SPY = .25, TLT = .2, GLD = .1, `EURUSD=X` = .05, `BTC-USD` = .1, USO = .05, EFA = .15, IWM = .1),
      momentum = momentum,
      defensive = c(SPY = .1, TLT = .35, GLD = .2, `EURUSD=X` = .1, `BTC-USD` = .025, USO = .025, EFA = .1, IWM = .1)
    )
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
  allowed <- stats::setNames(rep(list(symbols), length(deterministic_agents)), deterministic_agents)
  if (isTRUE(use_bulk)) {
    result <- sim_portfolio_target_replay(exchange, bars, targets, allowed, execution, export_path, profile)
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
  c(result, list(
    n_assets = length(symbols),
    n_deterministic_agents = length(deterministic_agents),
    n_llm_accounts = 2L,
    n_days = as.integer(n_days),
    use_bulk = isTRUE(use_bulk)
  ))
}
