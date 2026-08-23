make_portfolio_boundary_exchange <- function() {
  exchange <- sim_exchange_new(list(cash = 100000, ctr_step = 1, lev = 1))
  for (i in 1:2) sim_asset_add(exchange, c("SPY", "TLT")[i], asset_id = i, qty_step = 1)
  exchange
}

make_portfolio_boundary_bars <- function(timestamp, prices = c(100, 50)) {
  data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"),
    symbol = c("SPY", "TLT"), asset_id = 1:2,
    open = prices, high = prices, low = prices, close = prices
  )
}

test_that("one market boundary plus multiple submissions matches the combined API", {
  execution <- sim_portfolio_execution(fee_rt = 0.001, lev = 1)
  day_1 <- make_portfolio_boundary_bars("2026-01-01")
  day_2 <- make_portfolio_boundary_bars("2026-01-02", c(101, 49))
  weights_a <- c(SPY = 0.6, TLT = 0.4)
  weights_b <- c(SPY = -0.5, TLT = 0.5)

  combined <- make_portfolio_boundary_exchange()
  sim_portfolio_target_step(combined, "alpha", day_1, weights_a, execution)
  sim_portfolio_target_step(combined, "beta", day_1, weights_b, execution)
  sim_portfolio_target_step(combined, "alpha", day_2, NULL, execution)
  sim_portfolio_target_step(combined, "beta", day_2, NULL, execution)

  boundary <- make_portfolio_boundary_exchange()
  first <- sim_portfolio_market_step(boundary, day_1, execution)
  alpha <- sim_portfolio_target_submit(boundary, "alpha", day_1, weights_a, execution)
  beta <- sim_portfolio_target_submit(boundary, "beta", day_1, weights_b, execution)
  expect_equal(nrow(first$fills), 0L)
  expect_true(all(alpha$orders$status == "accepted"))
  expect_true(all(beta$orders$status == "accepted"))
  second <- sim_portfolio_market_step(boundary, day_2, execution)
  sim_portfolio_target_submit(boundary, "alpha", day_2, NULL, execution)
  sim_portfolio_target_submit(boundary, "beta", day_2, NULL, execution)

  expect_equal(boundary$agent_orders, combined$agent_orders)
  expect_equal(boundary$portfolio_rebalances, combined$portfolio_rebalances)
  expect_equal(boundary$portfolio_targets, combined$portfolio_targets)
  expect_equal(boundary$portfolio_fills, combined$portfolio_fills)
  expect_equal(sim_exchange_positions(boundary), sim_exchange_positions(combined))
  expect_equal(sim_exchange_account(boundary), sim_exchange_account(combined))
  expect_equal(nrow(second$fills), 4L)
  expect_true(all(second$fills$timestamp == day_2$timestamp[1L]))
})

test_that("market boundaries reject stale bars and submissions require an accepted boundary", {
  exchange <- make_portfolio_boundary_exchange()
  execution <- sim_portfolio_execution()
  day_1 <- make_portfolio_boundary_bars("2026-01-01")
  expect_error(
    sim_portfolio_target_submit(exchange, "alpha", day_1, c(SPY = 1), execution),
    "accepted by `sim_portfolio_market_step\\(\\)`"
  )
  sim_portfolio_market_step(exchange, day_1, execution)
  expect_error(sim_portfolio_market_step(exchange, day_1, execution), "duplicate or stale")
  stale <- make_portfolio_boundary_bars("2025-12-31")
  expect_error(sim_portfolio_market_step(exchange, stale, execution), "duplicate or stale")
})

test_that("market-boundary state survives save and load", {
  exchange <- make_portfolio_boundary_exchange()
  execution <- sim_portfolio_execution()
  day_1 <- make_portfolio_boundary_bars("2026-01-01")
  sim_portfolio_market_step(exchange, day_1, execution)
  state_dir <- tempfile("tradesimr-boundary-state-")
  sim_exchange_save(exchange, state_dir)
  loaded <- sim_exchange_load(state_dir)
  expect_equal(as.numeric(loaded$portfolio_market_boundaries$timestamp), as.numeric(exchange$portfolio_market_boundaries$timestamp))
  expect_equal(loaded$portfolio_market_boundaries[, !"timestamp"], exchange$portfolio_market_boundaries[, !"timestamp"])
  submitted <- sim_portfolio_target_submit(loaded, "alpha", day_1, c(SPY = 1), execution)
  expect_true(all(submitted$orders$status == "accepted"))
})

test_that("batched target submissions preserve sequential boundary semantics", {
  execution <- sim_portfolio_execution(fee_rt = 0.001, lev = 1)
  day_1 <- make_portfolio_boundary_bars("2026-02-01")
  day_2 <- make_portfolio_boundary_bars("2026-02-02", c(101, 49))
  decisions <- list(
    alpha = list(target_weights = c(SPY = 0.6, TLT = 0.4), decision_label = "alpha-target"),
    beta = list(target_weights = c(SPY = -0.5, TLT = 0.5), decision_label = "beta-target")
  )

  sequential <- make_portfolio_boundary_exchange()
  sim_portfolio_market_step(sequential, day_1, execution)
  lapply(names(decisions), function(agent_id) {
    decision <- decisions[[agent_id]]
    sim_portfolio_target_submit(sequential, agent_id, day_1, decision$target_weights, execution, decision$decision_label)
  })
  sim_portfolio_market_step(sequential, day_2, execution)

  batched <- make_portfolio_boundary_exchange()
  sim_portfolio_market_step(batched, day_1, execution)
  submitted <- sim_portfolio_target_submit_batch(batched, day_1, decisions, execution)
  filled <- sim_portfolio_market_step(batched, day_2, execution)

  expect_named(submitted$submissions, names(decisions))
  expect_true(all(vapply(submitted$submissions, function(x) all(x$orders$status == "accepted"), logical(1L))))
  expect_equal(batched$agent_orders, sequential$agent_orders)
  expect_equal(batched$portfolio_rebalances, sequential$portfolio_rebalances)
  expect_equal(batched$portfolio_targets, sequential$portfolio_targets)
  expect_equal(batched$portfolio_fills, sequential$portfolio_fills)
  expect_equal(sim_exchange_positions(batched), sim_exchange_positions(sequential))
  expect_equal(sim_exchange_account(batched), sim_exchange_account(sequential))
  expect_equal(nrow(filled$fills), 4L)
})
