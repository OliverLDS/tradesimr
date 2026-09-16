policy_exchange <- function() {
  exchange <- sim_exchange_new(list(cash = 100000, portfolio_margin = TRUE, lev = 1))
  sim_asset_add(exchange, "SPY", asset_id = 1L, instrument_profile = "etf", quote_ccy = "USD")
  sim_asset_add(exchange, "BTC-USD", asset_id = 2L, instrument_profile = "crypto_spot", quote_ccy = "USD")
  exchange
}

policy_bars <- function(timestamp, symbols = c("SPY", "BTC-USD"), tradable = TRUE) {
  prices <- c(SPY = 100, `BTC-USD` = 50000)
  data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"), symbol = symbols,
    asset_id = match(symbols, c("SPY", "BTC-USD")),
    open = unname(prices[symbols]), high = unname(prices[symbols]),
    low = unname(prices[symbols]), close = unname(prices[symbols]),
    is_completed = TRUE, is_tradable = tradable
  )
}

test_that("as-of valuation permits a fresh asset plus a bounded carried mark", {
  exchange <- policy_exchange()
  execution <- sim_portfolio_execution(lev = 1)
  initial <- policy_bars("2026-01-01")
  sim_portfolio_market_step(exchange, initial, execution)
  spy_only <- policy_bars("2026-01-02", "SPY")
  sim_portfolio_market_step(exchange, spy_only, execution)

  submitted <- sim_portfolio_target_submit(
    exchange, "agent", spy_only, c(SPY = .5, `BTC-USD` = .5), execution,
    allowed_symbols = c("SPY", "BTC-USD"),
    decision_policy = sim_portfolio_decision_policy("as_of_valuation", max_staleness = 2 * 86400)
  )
  expect_equal(nrow(submitted$orders), 2L)
  expect_true(all(submitted$orders$status == "accepted"))
  expect_equal(submitted$orders$symbol, c("SPY", "BTC-USD"))
})

test_that("as-of valuation rejects marks beyond its configured staleness bound", {
  exchange <- policy_exchange()
  execution <- sim_portfolio_execution(lev = 1)
  initial <- policy_bars("2026-01-01")
  sim_portfolio_market_step(exchange, initial, execution)
  spy_only <- policy_bars("2026-01-04", "SPY")
  sim_portfolio_market_step(exchange, spy_only, execution)

  expect_error(
    sim_portfolio_target_submit(
      exchange, "agent", spy_only, c(SPY = .5, `BTC-USD` = .5), execution,
      allowed_symbols = c("SPY", "BTC-USD"),
      decision_policy = sim_portfolio_decision_policy("as_of_valuation", max_staleness = 86400)
    ),
    "exceeds `max_staleness`"
  )
})

test_that("per-asset decisions require fresh bars only for targeted assets", {
  exchange <- policy_exchange()
  execution <- sim_portfolio_execution(lev = 1)
  initial <- policy_bars("2026-01-01")
  sim_portfolio_market_step(exchange, initial, execution)
  spy_only <- policy_bars("2026-01-02", "SPY")
  sim_portfolio_market_step(exchange, spy_only, execution)

  submitted <- sim_portfolio_target_submit(
    exchange, "agent", spy_only, c(SPY = .5), execution,
    allowed_symbols = c("SPY", "BTC-USD"),
    decision_policy = sim_portfolio_decision_policy("per_asset_decision")
  )
  expect_equal(submitted$orders$symbol, "SPY")
  expect_error(
    sim_portfolio_target_submit(
      exchange, "other", spy_only, c(`BTC-USD` = .5), execution,
      allowed_symbols = c("SPY", "BTC-USD"),
      decision_policy = sim_portfolio_decision_policy("per_asset_decision")
    ),
    "missing: BTC-USD"
  )
})

test_that("non-tradable observations cannot create portfolio target decisions", {
  exchange <- policy_exchange()
  execution <- sim_portfolio_execution(lev = 1)
  closed <- policy_bars("2026-01-03", "SPY", tradable = FALSE)
  sim_portfolio_market_step(exchange, closed, execution)
  expect_error(
    sim_portfolio_target_submit(
      exchange, "agent", closed, c(SPY = 1), execution,
      allowed_symbols = "SPY",
      decision_policy = sim_portfolio_decision_policy("per_asset_decision")
    ),
    "fresh, completed, tradable"
  )
})

test_that("bulk replay applies as-of policy after each accepted boundary", {
  exchange <- policy_exchange()
  execution <- sim_portfolio_execution(lev = 1)
  first <- policy_bars("2026-02-01")
  second <- policy_bars("2026-02-02", "SPY")
  third <- policy_bars("2026-02-03", "SPY")
  panel <- data.frame(
    timestamp = rep(second$timestamp[1L], 2L), agent_id = "agent",
    symbol = c("SPY", "BTC-USD"), target_weight = c(.5, .5)
  )
  replay <- sim_portfolio_target_replay(
    exchange, rbind(first, second, third), panel,
    allowed_symbols = list(agent = c("SPY", "BTC-USD")), execution = execution,
    decision_policy = sim_portfolio_decision_policy("as_of_valuation", max_staleness = 2 * 86400)
  )
  expect_equal(nrow(replay$rebalances), 1L)
  expect_equal(replay$orders$symbol, c("SPY", "BTC-USD"))
  expect_equal(replay$orders[symbol == "SPY", status], "filled")
  expect_equal(replay$orders[symbol == "BTC-USD", status], "accepted")
})
