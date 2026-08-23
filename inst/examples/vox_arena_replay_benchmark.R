# Vox Arena clean-replay benchmark
#
# Run with:
# source(system.file("examples", "vox_arena_replay_benchmark.R", package = "tradesimr"))
# result <- run_vox_arena_replay_benchmark(n_days = 20, use_batch = TRUE, profile = TRUE)
# result$elapsed

run_vox_arena_replay_benchmark <- function(n_days = 20L,
                                           use_batch = TRUE,
                                           profile = FALSE,
                                           profile_path = tempfile("tradesimr-vox-replay-", fileext = ".out")) {
  stopifnot(length(n_days) == 1L, is.finite(n_days), n_days > 0)
  symbols <- c("SPY", "TLT", "GLD", "EURUSD=X", "BTC-USD", "USO", "EFA", "IWM")
  prices <- c(600, 90, 250, 1.1, 65000, 80, 90, 220)
  execution <- sim_portfolio_execution(fee_rt = 0.0007, lev = 1)
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE))
  for (i in seq_along(symbols)) {
    sim_asset_add(exchange, symbols[i], asset_id = i, qty_step = if (symbols[i] == "BTC-USD") 0.001 else 1)
  }
  deterministic_agents <- unlist(lapply(paste0("strategy", seq_len(8L)), function(strategy) {
    paste(strategy, symbols, sep = "--")
  }), use.names = FALSE)
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
  decisions_for_boundary <- function() {
    stats::setNames(lapply(deterministic_agents, function(agent_id) {
      symbol <- sub("^.*--", "", agent_id)
      list(target_weights = stats::setNames(1, symbol), allowed_symbols = symbol, decision_label = "deterministic")
    }), deterministic_agents)
  }

  if (isTRUE(profile)) Rprof(profile_path, interval = 0.01)
  started <- proc.time()[["elapsed"]]
  for (day in seq_len(as.integer(n_days))) {
    bars <- bars_for_day(day)
    sim_portfolio_market_step(exchange, bars, execution)
    decisions <- decisions_for_boundary()
    if (isTRUE(use_batch)) {
      sim_portfolio_target_submit_batch(exchange, bars, decisions, execution)
    } else {
      for (agent_id in names(decisions)) {
        decision <- decisions[[agent_id]]
        sim_portfolio_target_submit(exchange, agent_id, bars, decision$target_weights, execution,
          decision$decision_label, decision$allowed_symbols)
      }
    }
  }
  elapsed <- proc.time()[["elapsed"]] - started
  if (isTRUE(profile)) Rprof(NULL)
  list(
    exchange = exchange,
    elapsed = elapsed,
    profile_path = if (isTRUE(profile)) profile_path else NULL,
    profile = if (isTRUE(profile)) summaryRprof(profile_path) else NULL,
    n_assets = length(symbols),
    n_deterministic_agents = length(deterministic_agents),
    n_llm_accounts = 2L,
    n_days = as.integer(n_days),
    use_batch = isTRUE(use_batch)
  )
}
