make_vox_batch_exchange <- function() {
  symbols <- c("SPY", "TLT", "GLD", "EURUSD=X", "BTC-USD", "USO", "EFA", "IWM")
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE))
  for (i in seq_along(symbols)) {
    sim_asset_add(exchange, symbols[i], asset_id = i, qty_step = if (symbols[i] == "BTC-USD") 0.001 else 1)
  }
  agents <- unlist(lapply(paste0("strategy", seq_len(8L)), function(strategy) paste(strategy, symbols, sep = "--")), use.names = FALSE)
  for (agent_id in agents) sim_agent_add(exchange, agent_id, "human")
  sim_agent_add(exchange, "llm-a", "human")
  sim_agent_add(exchange, "llm-b", "human")
  list(exchange = exchange, symbols = symbols, agents = agents)
}

make_vox_batch_bars <- function(timestamp, symbols, bump = 0) {
  prices <- c(600, 90, 250, 1.1, 65000, 80, 90, 220) + bump
  data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"), symbol = symbols, asset_id = seq_along(symbols),
    open = prices, high = prices * 1.01, low = prices * 0.99, close = prices
  )
}

make_vox_batch_decisions <- function(agents) {
  stats::setNames(lapply(agents, function(agent_id) {
    symbol <- sub("^.*--", "", agent_id)
    list(target_weights = stats::setNames(1, symbol), allowed_symbols = symbol, decision_label = "deterministic")
  }), agents)
}

test_that("eight-asset 66-account replay matches sequential and batch submissions", {
  execution <- sim_portfolio_execution(fee_rt = 0.0007, lev = 1)
  sequential_setup <- make_vox_batch_exchange()
  batch_setup <- make_vox_batch_exchange()
  day_1 <- make_vox_batch_bars("2026-08-04", sequential_setup$symbols)
  day_2 <- make_vox_batch_bars("2026-08-05", sequential_setup$symbols, bump = 1)
  decisions <- make_vox_batch_decisions(sequential_setup$agents)

  sim_portfolio_market_step(sequential_setup$exchange, day_1, execution)
  for (agent_id in names(decisions)) {
    decision <- decisions[[agent_id]]
    sim_portfolio_target_submit(
      sequential_setup$exchange, agent_id, day_1, decision$target_weights, execution,
      decision$decision_label, decision$allowed_symbols
    )
  }
  sequential_boundary <- sim_portfolio_market_step(sequential_setup$exchange, day_2, execution)

  sim_portfolio_market_step(batch_setup$exchange, day_1, execution)
  batch_submission <- sim_portfolio_target_submit_batch(batch_setup$exchange, day_1, decisions, execution)
  batch_boundary <- sim_portfolio_market_step(batch_setup$exchange, day_2, execution)

  expect_length(batch_submission$submissions, 64L)
  expect_equal(nrow(batch_boundary$fills), nrow(sequential_boundary$fills))
  expect_equal(batch_setup$exchange$agent_orders, sequential_setup$exchange$agent_orders)
  expect_equal(batch_setup$exchange$portfolio_rebalances, sequential_setup$exchange$portfolio_rebalances)
  expect_equal(batch_setup$exchange$portfolio_targets, sequential_setup$exchange$portfolio_targets)
  expect_equal(batch_setup$exchange$portfolio_fills, sequential_setup$exchange$portfolio_fills)
  expect_equal(sim_exchange_positions(batch_setup$exchange), sim_exchange_positions(sequential_setup$exchange))
  expect_equal(sim_exchange_account(batch_setup$exchange), sim_exchange_account(sequential_setup$exchange))

  first_boundary_orders <- batch_setup$exchange$agent_orders[
    batch_setup$exchange$agent_orders$timestamp == day_1$timestamp[1L]
  ]
  expect_true(all(first_boundary_orders$status == "filled"))
  linked_fills <- batch_setup$exchange$portfolio_fills[
    match(first_boundary_orders$order_id, batch_setup$exchange$portfolio_fills$order_id)
  ]
  expect_equal(linked_fills$order_id, first_boundary_orders$order_id)
  expect_equal(linked_fills$rebalance_id, first_boundary_orders$rebalance_id)
  expect_equal(linked_fills$agent_id, first_boundary_orders$agent_id)
  positions <- sim_exchange_positions(batch_setup$exchange)
  expect_true(all(positions$pos_dir == 1L))
})

test_that("a satisfied target is no-op and an execution rejection is terminal", {
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE))
  sim_asset_add(exchange, "SPY", asset_id = 1, qty_step = 1)
  bars_1 <- data.frame(
    timestamp = as.POSIXct("2026-08-04", tz = "UTC"), symbol = "SPY", asset_id = 1,
    open = 100, high = 100, low = 100, close = 100
  )
  bars_2 <- data.table::copy(bars_1)
  bars_2$timestamp <- bars_2$timestamp + 86400
  execution <- sim_portfolio_execution(lev = 1, fee_rt = 0)
  sim_portfolio_market_step(exchange, bars_1, execution)
  sim_portfolio_target_submit_batch(exchange, bars_1, list(agent = list(
    target_weights = c(SPY = 0.5), allowed_symbols = "SPY"
  )), execution)
  sim_portfolio_market_step(exchange, bars_2, execution)
  no_op <- sim_portfolio_target_submit_batch(exchange, bars_2, list(agent = list(
    target_weights = c(SPY = 0.5), allowed_symbols = "SPY"
  )), execution)$submissions$agent
  expect_equal(no_op$rebalances$status, "no_op")
  expect_equal(nrow(no_op$orders), 0L)

  rejected <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE, portfolio_margin_floor = 2))
  sim_asset_add(rejected, "SPY", asset_id = 1, qty_step = 1)
  sim_portfolio_market_step(rejected, bars_1, execution)
  sim_portfolio_target_submit_batch(rejected, bars_1, list(agent = list(
    target_weights = c(SPY = 1), allowed_symbols = "SPY"
  )), execution)
  sim_portfolio_market_step(rejected, bars_2, execution)
  terminal <- rejected$agent_orders
  expect_equal(terminal$status, "rejected")
  expect_match(terminal$message, "eligible market boundary")
  expect_equal(nrow(rejected$portfolio_fills), 0L)
})
