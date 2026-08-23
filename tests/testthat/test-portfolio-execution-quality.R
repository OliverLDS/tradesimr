make_quality_exchange <- function(symbols = c("SPY"), config = list(cash = 100000, lev = 1, portfolio_margin = TRUE)) {
  exchange <- sim_exchange_new(config)
  for (i in seq_along(symbols)) {
    sim_asset_add(exchange, symbols[i], asset_id = i, qty_step = if (symbols[i] == "BTC-USD") 0.001 else 1)
  }
  exchange
}

quality_bars <- function(timestamp, symbols, prices = seq_along(symbols) * 100) {
  data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"), symbol = symbols, asset_id = seq_along(symbols),
    open = prices, high = prices, low = prices, close = prices
  )
}

test_that("execution quality reports fulfilled fee-aware fractional BTC targets", {
  exchange <- make_quality_exchange("BTC-USD")
  execution <- sim_portfolio_execution(fee_rt = 0.0007, lev = 1)
  day_1 <- quality_bars("2026-01-01", "BTC-USD", 64000)
  day_2 <- quality_bars("2026-01-02", "BTC-USD", 64000)
  sim_portfolio_market_step(exchange, day_1, execution)
  sim_portfolio_target_submit_batch(exchange, day_1, list(agent = list(
    target_weights = c(`BTC-USD` = 1), allowed_symbols = "BTC-USD"
  )), execution)
  pending <- sim_portfolio_execution_quality(exchange)
  expect_equal(pending$execution_quality, "pending")
  expect_true(is.na(pending$settlement_timestamp))
  sim_portfolio_market_step(exchange, day_2, execution)

  quality <- sim_portfolio_execution_quality(exchange)
  expect_setequal(names(quality), c(
    "rebalance_id", "agent_id", "symbol", "asset_id", "decision_timestamp", "eligible_after",
    "settlement_timestamp", "target_weight", "decision_equity", "decision_price", "qty_step",
    "contract_size", "current_signed_quantity", "expected_signed_quantity", "expected_notional",
    "realized_signed_quantity", "realized_notional", "quantity_deviation", "notional_deviation",
    "weight_deviation", "execution_quality", "message"
  ))
  expect_equal(quality$execution_quality, "fulfilled")
  expect_equal(quality$current_signed_quantity, 0)
  expect_equal(quality$expected_signed_quantity, 1.561)
  expect_equal(quality$realized_signed_quantity, 1.561)
  expect_equal(quality$quantity_deviation, 0)
  expect_equal(quality$settlement_timestamp, day_2$timestamp)
})

test_that("execution quality distinguishes no-op, terminal rejection, and partial reversal", {
  execution <- sim_portfolio_execution(lev = 1, fee_rt = 0)
  day_1 <- quality_bars("2026-02-01", "SPY", 100)
  day_2 <- quality_bars("2026-02-02", "SPY", 100)
  day_3 <- quality_bars("2026-02-03", "SPY", 100)
  exchange <- make_quality_exchange("SPY")
  sim_portfolio_market_step(exchange, day_1, execution)
  sim_portfolio_target_submit_batch(exchange, day_1, list(agent = list(
    target_weights = c(SPY = 0.5), allowed_symbols = "SPY"
  )), execution)
  sim_portfolio_market_step(exchange, day_2, execution)
  sim_portfolio_target_submit_batch(exchange, day_2, list(agent = list(
    target_weights = c(SPY = 0.5), allowed_symbols = "SPY"
  )), execution)
  no_op <- sim_portfolio_execution_quality(exchange)
  expect_true(any(no_op$execution_quality == "no_op"))

  exchange$config$portfolio_margin_floor <- 3
  sim_portfolio_target_submit_batch(exchange, day_2, list(agent = list(
    target_weights = c(SPY = -0.5), allowed_symbols = "SPY"
  )), execution)
  sim_portfolio_market_step(exchange, day_3, execution)
  quality <- sim_portfolio_execution_quality(exchange)
  expect_true(any(quality$execution_quality == "partial"))

  rejected <- make_quality_exchange("SPY", list(cash = 100000, lev = 1, portfolio_margin = TRUE, portfolio_margin_floor = 2))
  sim_portfolio_market_step(rejected, day_1, execution)
  sim_portfolio_target_submit_batch(rejected, day_1, list(agent = list(
    target_weights = c(SPY = 1), allowed_symbols = "SPY"
  )), execution)
  sim_portfolio_market_step(rejected, day_2, execution)
  terminal <- sim_portfolio_execution_quality(rejected)
  expect_equal(terminal$execution_quality, "terminal_rejected")
  expect_match(terminal$message, "eligible market boundary")
})

test_that("execution quality summarizes multi-asset rebalances and survives save/load", {
  symbols <- c("SPY", "TLT")
  exchange <- make_quality_exchange(symbols)
  execution <- sim_portfolio_execution(lev = 1, fee_rt = 0)
  day_1 <- quality_bars("2026-03-01", symbols, c(100, 50))
  day_2 <- quality_bars("2026-03-02", symbols, c(100, 50))
  sim_portfolio_market_step(exchange, day_1, execution)
  sim_portfolio_target_submit_batch(exchange, day_1, list(agent = list(
    target_weights = c(SPY = 0.5, TLT = 0.5), allowed_symbols = symbols
  )), execution)
  sim_portfolio_market_step(exchange, day_2, execution)
  quality <- sim_portfolio_execution_quality(exchange)
  summary <- sim_portfolio_execution_quality(exchange, summary = TRUE)
  expect_equal(nrow(quality), 2L)
  expect_true(all(quality$execution_quality == "fulfilled"))
  expect_equal(nrow(summary), 1L)
  expect_equal(summary$symbol, "ALL")
  expect_equal(summary$execution_quality, "fulfilled")

  state_dir <- tempfile("tradesimr-quality-")
  sim_exchange_save(exchange, state_dir)
  loaded <- sim_exchange_load(state_dir)
  expect_equal(sim_portfolio_execution_quality(loaded), quality)
})

test_that("execution quality respects the persisted allowed universe", {
  symbols <- c("SPY", "TLT")
  exchange <- make_quality_exchange(symbols)
  execution <- sim_portfolio_execution(lev = 1, fee_rt = 0)
  day_1 <- quality_bars("2026-04-01", symbols, c(100, 50))
  day_2 <- quality_bars("2026-04-02", symbols, c(100, 50))
  sim_portfolio_market_step(exchange, day_1, execution)
  sim_portfolio_target_submit_batch(exchange, day_1, list(
    spy_agent = list(target_weights = c(SPY = 1), allowed_symbols = "SPY"),
    tlt_agent = list(target_weights = c(TLT = 1), allowed_symbols = "TLT")
  ), execution)
  sim_portfolio_market_step(exchange, day_2, execution)
  spy <- sim_portfolio_execution_quality(exchange, "spy_agent")
  tlt <- sim_portfolio_execution_quality(exchange, "tlt_agent")
  expect_equal(spy$symbol, "SPY")
  expect_equal(tlt$symbol, "TLT")
  expect_true(all(spy$execution_quality == "fulfilled"))
  expect_true(all(tlt$execution_quality == "fulfilled"))
})
