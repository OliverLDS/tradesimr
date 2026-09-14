quality_parity_reference <- function(exchange) {
  targets <- data.table::copy(exchange$portfolio_targets)
  if (!nrow(targets)) return(tradesimr:::.portfolio_execution_quality_empty())
  data.table::setorderv(targets, c("timestamp", "rebalance_id", "asset_id"))
  data.table::rbindlist(lapply(seq_len(nrow(targets)), function(i) {
    # `context = NULL` is the retained pre-optimization full-ledger path.
    tradesimr:::.portfolio_execution_quality_row(exchange, targets[i], context = NULL)
  }), fill = TRUE)
}

quality_parity_exchange <- function() {
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE))
  sim_asset_add(exchange, "SPY", asset_id = 1L, qty_step = 1)
  sim_asset_add(exchange, "TLT", asset_id = 2L, qty_step = 1)
  exchange
}

quality_parity_bars <- function(timestamp, symbols = c("SPY", "TLT")) {
  data.table::data.table(
    timestamp = as.POSIXct(timestamp, tz = "UTC"), symbol = symbols,
    asset_id = match(symbols, c("SPY", "TLT")),
    open = c(SPY = 100, TLT = 50)[symbols], high = c(SPY = 100, TLT = 50)[symbols],
    low = c(SPY = 100, TLT = 50)[symbols], close = c(SPY = 100, TLT = 50)[symbols]
  )
}

test_that("indexed execution quality exactly matches the full-ledger reference", {
  execution <- sim_portfolio_execution(lev = 1, fee_rt = 0)
  exchange <- quality_parity_exchange()
  first <- quality_parity_bars("2026-01-01")
  second <- quality_parity_bars("2026-01-02")
  third <- quality_parity_bars("2026-01-03")

  sim_portfolio_market_step(exchange, first, execution)
  sim_portfolio_target_submit_batch(exchange, first, list(
    fulfilled = list(target_weights = c(SPY = .5), allowed_symbols = "SPY"),
    terminal = list(target_weights = c(TLT = .5), allowed_symbols = "TLT")
  ), execution)
  # A terminal ledger row exercises the same durable rejection path used for
  # an explicit infeasible execution, without changing target-order semantics.
  terminal_index <- which(exchange$agent_orders$agent_id == "terminal")
  data.table::set(exchange$agent_orders, i = terminal_index, j = "status", value = "rejected")
  data.table::set(exchange$agent_orders, i = terminal_index, j = "settlement_timestamp", value = first$timestamp[1L])
  data.table::set(exchange$agent_orders, i = terminal_index, j = "message", value = "Execution rejected at the eligible market boundary.")

  sim_portfolio_market_step(exchange, second, execution)
  sim_portfolio_target_submit_batch(exchange, second, list(
    fulfilled = list(target_weights = c(SPY = .5), allowed_symbols = "SPY"),
    partial = list(target_weights = c(TLT = -.5), allowed_symbols = "TLT"),
    pending = list(target_weights = c(SPY = .25), allowed_symbols = "SPY")
  ), execution)
  exchange$config$portfolio_margin_floor <- 3
  sim_portfolio_market_step(exchange, third[symbol == "TLT"], execution)

  reference <- quality_parity_reference(exchange)
  indexed <- sim_portfolio_execution_quality(exchange)
  expect_equal(indexed, reference)
  expect_setequal(indexed$execution_quality, c("fulfilled", "no_op", "partial", "terminal_rejected", "pending"))

  state_path <- tempfile("tradesimr-quality-parity-")
  sim_exchange_save(exchange, state_path)
  loaded <- sim_exchange_load(state_path)
  expect_equal(sim_portfolio_execution_quality(loaded), quality_parity_reference(loaded))
  export_path <- tempfile("tradesimr-quality-export-")
  paths <- sim_portfolio_export(loaded, "fulfilled", export_path)
  expect_true(all(file.exists(paths)))
})
