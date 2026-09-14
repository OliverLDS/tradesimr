complete_universe_exchange <- function() {
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE))
  sim_asset_add(exchange, "SPY", asset_id = 1L, asset_class = "etf", qty_step = 1)
  sim_asset_add(exchange, "TLT", asset_id = 2L, asset_class = "etf", qty_step = 1)
  exchange
}

complete_universe_bars <- function(timestamp, symbols = c("SPY", "TLT")) {
  prices <- c(SPY = 100, TLT = 50)
  data.table::data.table(
    timestamp = as.POSIXct(timestamp, tz = "UTC"),
    symbol = symbols,
    asset_id = match(symbols, c("SPY", "TLT")),
    open = unname(prices[symbols]), high = unname(prices[symbols]),
    low = unname(prices[symbols]), close = unname(prices[symbols])
  )
}

complete_universe_tables <- function(exchange) {
  list(
    orders = exchange$agent_orders,
    fills = exchange$portfolio_fills,
    targets = exchange$portfolio_targets,
    rebalances = exchange$portfolio_rebalances,
    positions = sim_exchange_positions(exchange),
    account = sim_exchange_account(exchange),
    quality = sim_portfolio_execution_quality(exchange)
  )
}

test_that("multi-asset incremental target submissions reject incomplete boundaries", {
  exchange <- complete_universe_exchange()
  execution <- sim_portfolio_execution(lev = 1)
  partial <- complete_universe_bars("2026-09-01", "SPY")
  sim_portfolio_market_step(exchange, partial, execution)

  expect_error(
    sim_portfolio_target_submit(
      exchange, "portfolio", partial, c(SPY = .5, TLT = .5), execution,
      allowed_symbols = c("SPY", "TLT")
    ),
    "requires exactly one completed bar"
  )
  expect_error(
    sim_portfolio_target_submit_batch(exchange, partial, list(portfolio = list(
      target_weights = c(SPY = .5, TLT = .5), allowed_symbols = c("SPY", "TLT")
    )), execution),
    "requires exactly one completed bar"
  )
  expect_equal(nrow(exchange$portfolio_rebalances), 0L)
  expect_equal(nrow(exchange$agent_orders), 0L)
  expect_false("portfolio" %in% exchange$agents$agent_id)

  single <- sim_portfolio_target_submit(
    exchange, "single", partial, c(SPY = 1), execution, allowed_symbols = "SPY"
  )
  expect_equal(single$rebalances$status, "accepted")
})

test_that("bulk replay skips partial multi-asset decisions and matches complete-boundary replay", {
  execution <- sim_portfolio_execution(fee_rt = .0007, lev = 1)
  first <- complete_universe_bars("2026-09-01", "SPY")
  second <- complete_universe_bars("2026-09-02")
  third <- complete_universe_bars("2026-09-03")
  bars <- data.table::rbindlist(list(first, second, third))
  panel <- data.table::rbindlist(list(
    data.table::data.table(timestamp = first$timestamp[1L], agent_id = "portfolio", symbol = c("SPY", "TLT"), target_weight = c(.5, .5)),
    data.table::data.table(timestamp = second$timestamp[1L], agent_id = "portfolio", symbol = c("SPY", "TLT"), target_weight = c(.5, .5))
  ))

  sequential <- complete_universe_exchange()
  sim_portfolio_market_step(sequential, first, execution)
  sim_portfolio_market_step(sequential, second, execution)
  sim_portfolio_target_submit_batch(sequential, second, list(portfolio = list(
    target_weights = c(SPY = .5, TLT = .5), allowed_symbols = c("SPY", "TLT")
  )), execution)
  sim_portfolio_market_step(sequential, third, execution)

  bulk <- sim_portfolio_target_replay(
    complete_universe_exchange(), bars, panel,
    allowed_symbols = list(portfolio = c("SPY", "TLT")), execution = execution
  )$exchange

  expect_equal(complete_universe_tables(bulk), complete_universe_tables(sequential))
  expect_equal(nrow(bulk$portfolio_rebalances), 1L)
  expect_equal(as.numeric(bulk$portfolio_rebalances$timestamp), as.numeric(second$timestamp[1L]))
  expect_true(all(bulk$portfolio_fills$timestamp > bulk$portfolio_targets$timestamp))
})
