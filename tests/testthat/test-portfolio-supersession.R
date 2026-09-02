make_supersession_exchange <- function() {
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE))
  sim_asset_add(exchange, "SPY", asset_id = 1L, asset_class = "etf", qty_step = 1)
  sim_asset_add(exchange, "TLT", asset_id = 2L, asset_class = "etf", qty_step = 1)
  exchange
}

supersession_bars <- function(timestamp, symbols, prices = c(SPY = 100, TLT = 50)) {
  data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"), symbol = symbols,
    asset_id = match(symbols, c("SPY", "TLT")),
    open = unname(prices[symbols]), high = unname(prices[symbols]),
    low = unname(prices[symbols]), close = unname(prices[symbols])
  )
}

submit_initial_spy_target <- function(exchange, execution, agent_id = "agent") {
  eleven <- supersession_bars("2026-01-05 11:00:00", "SPY")
  sim_portfolio_market_step(exchange, eleven, execution)
  sim_portfolio_target_submit(
    exchange, agent_id, eleven, c(SPY = 0.5), execution,
    allowed_symbols = c("SPY", "TLT")
  )
  eleven
}

test_that("a later same-asset target supersedes an unfilled target order", {
  exchange <- make_supersession_exchange()
  execution <- sim_portfolio_execution(lev = 1)
  eleven <- submit_initial_spy_target(exchange, execution)
  two <- supersession_bars("2026-01-05 14:00:00", "TLT")
  sim_portfolio_market_step(exchange, two, execution)
  newer <- sim_portfolio_target_submit(
    exchange, "agent", two, c(SPY = 1), execution,
    allowed_symbols = c("SPY", "TLT")
  )
  old_order <- exchange$agent_orders[rebalance_id == "RB000001"]
  new_order <- exchange$agent_orders[rebalance_id == newer$rebalance_id]
  expect_equal(old_order$status, "superseded")
  expect_equal(old_order$reason_code, "superseded")
  expect_equal(old_order$superseded_by_rebalance_id, newer$rebalance_id)
  expect_equal(new_order$supersedes_rebalance_id, "RB000001")
  expect_equal(exchange$portfolio_rebalances[rebalance_id == "RB000001", status], "superseded")

  next_spy <- supersession_bars("2026-01-06 09:00:00", "SPY")
  market <- sim_portfolio_market_step(exchange, next_spy, execution)
  expect_equal(market$fills$order_id, new_order$order_id)
  expect_equal(nrow(exchange$portfolio_fills[rebalance_id == "RB000001"]), 0L)
  quality <- sim_portfolio_execution_quality(exchange)
  expect_equal(quality[rebalance_id == "RB000001", execution_quality], "superseded")
  expect_equal(quality[rebalance_id == newer$rebalance_id, execution_quality], "fulfilled")
  expect_gt(market$fills$timestamp, two$timestamp)
  expect_gt(two$timestamp, eleven$timestamp)
})

test_that("the newest same-boundary multi-asset decision supersedes an unfilled group", {
  exchange <- make_supersession_exchange()
  execution <- sim_portfolio_execution(lev = 1)
  boundary <- supersession_bars("2026-01-12 11:00:00", c("SPY", "TLT"))
  sim_portfolio_market_step(exchange, boundary, execution)
  sim_portfolio_target_submit(
    exchange, "agent", boundary, c(SPY = 0.5, TLT = 0.5), execution,
    allowed_symbols = c("SPY", "TLT")
  )
  newest <- sim_portfolio_target_submit(
    exchange, "agent", boundary, c(SPY = 0.4, TLT = 0.6), execution,
    allowed_symbols = c("SPY", "TLT")
  )
  expect_true(all(exchange$agent_orders[rebalance_id == "RB000001", status] == "superseded"))
  expect_equal(exchange$portfolio_rebalances[rebalance_id == "RB000001", status], "superseded")
  expect_true(all(exchange$agent_orders[rebalance_id == newest$rebalance_id, supersedes_rebalance_id] == "RB000001"))
  market <- sim_portfolio_market_step(exchange, supersession_bars("2026-01-13 09:00:00", c("SPY", "TLT")), execution)
  expect_setequal(market$fills$rebalance_id, newest$rebalance_id)
  expect_true(all(sim_portfolio_execution_quality(exchange)[rebalance_id == "RB000001", execution_quality] == "superseded"))
})

test_that("partial multi-asset rebalances retain filled legs and supersede pending overlaps", {
  exchange <- make_supersession_exchange()
  execution <- sim_portfolio_execution(lev = 1)
  eleven <- supersession_bars("2026-02-05 11:00:00", c("SPY", "TLT"))
  sim_portfolio_market_step(exchange, eleven, execution)
  sim_portfolio_target_submit(
    exchange, "agent", eleven, c(SPY = 0.5, TLT = 0.5), execution,
    allowed_symbols = c("SPY", "TLT")
  )
  two <- supersession_bars("2026-02-05 14:00:00", "TLT")
  sim_portfolio_market_step(exchange, two, execution)
  newer <- sim_portfolio_target_submit(
    exchange, "agent", two, c(SPY = 0.5), execution,
    allowed_symbols = c("SPY", "TLT")
  )
  old <- exchange$agent_orders[rebalance_id == "RB000001"]
  expect_equal(old[symbol == "TLT", status], "filled")
  expect_equal(old[symbol == "SPY", status], "superseded")
  expect_equal(exchange$portfolio_rebalances[rebalance_id == "RB000001", status], "partially_superseded")
  expect_equal(nrow(exchange$portfolio_fills[rebalance_id == "RB000001" & symbol == "TLT"]), 1L)
  expect_equal(exchange$portfolio_targets[rebalance_id == newer$rebalance_id, realized_weight_before], 0)
  expect_equal(sim_portfolio_execution_quality(exchange, summary = TRUE)[rebalance_id == "RB000001", execution_quality], "superseded")
})

test_that("non-overlapping pending symbols remain eligible and save/load preserves lineage", {
  exchange <- make_supersession_exchange()
  execution <- sim_portfolio_execution(lev = 1)
  submit_initial_spy_target(exchange, execution)
  two <- supersession_bars("2026-03-05 14:00:00", "TLT")
  sim_portfolio_market_step(exchange, two, execution)
  newer <- sim_portfolio_target_submit(
    exchange, "agent", two, c(TLT = 0.5), execution,
    allowed_symbols = c("SPY", "TLT")
  )
  expect_equal(exchange$agent_orders[rebalance_id == "RB000001", status], "accepted")
  expect_true(is.na(exchange$agent_orders[rebalance_id == newer$rebalance_id, supersedes_rebalance_id]))

  state_dir <- tempfile("tradesimr-supersession-")
  sim_exchange_save(exchange, state_dir)
  loaded <- sim_exchange_load(state_dir)
  expect_equal(loaded$agent_orders[, .(order_id, rebalance_id, status, superseded_by_rebalance_id, supersedes_rebalance_id)],
    exchange$agent_orders[, .(order_id, rebalance_id, status, superseded_by_rebalance_id, supersedes_rebalance_id)])
  expect_equal(loaded$portfolio_targets[, .(rebalance_id, asset_id, status, superseded_by_rebalance_id, supersedes_rebalance_id)],
    exchange$portfolio_targets[, .(rebalance_id, asset_id, status, superseded_by_rebalance_id, supersedes_rebalance_id)])
  next_bars <- supersession_bars("2026-03-06 09:00:00", c("SPY", "TLT"))
  market <- sim_portfolio_market_step(loaded, next_bars, execution)
  expect_setequal(market$fills$rebalance_id, c("RB000001", newer$rebalance_id))
})

test_that("sequential and batch submission preserve identical supersession outcomes", {
  execution <- sim_portfolio_execution(lev = 1)
  sequential <- make_supersession_exchange()
  batched <- make_supersession_exchange()
  submit_initial_spy_target(sequential, execution)
  submit_initial_spy_target(batched, execution)
  two <- supersession_bars("2026-04-05 14:00:00", "TLT")
  sim_portfolio_market_step(sequential, two, execution)
  sim_portfolio_market_step(batched, two, execution)
  sim_portfolio_target_submit(sequential, "agent", two, c(SPY = 1), execution, allowed_symbols = c("SPY", "TLT"))
  sim_portfolio_target_submit_batch(batched, two, list(agent = list(
    target_weights = c(SPY = 1), allowed_symbols = c("SPY", "TLT")
  )), execution)
  expect_equal(batched$agent_orders, sequential$agent_orders)
  expect_equal(batched$portfolio_targets, sequential$portfolio_targets)
  expect_equal(batched$portfolio_rebalances, sequential$portfolio_rebalances)
})
