arena_symbols <- c("SPY", "TLT", "GLD", "USO", "EFA", "IWM", "QQQ", "BTC-USD")

arena_bars <- function(timestamp, prices = seq(50, by = 10, length.out = length(arena_symbols))) {
  data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"),
    symbol = arena_symbols,
    asset_id = seq_along(arena_symbols),
    open = prices, high = prices, low = prices, close = prices
  )
}

run_single_asset_arena <- function() {
  exchange <- sim_exchange_new(list(init_cash = 100000, lev = 1, portfolio_margin = TRUE))
  for (i in seq_along(arena_symbols)) sim_asset_add(exchange, arena_symbols[i], asset_id = i, qty_step = 0.001)
  agents <- paste0("buy_hold--", tolower(gsub("[^A-Za-z0-9]", "", arena_symbols)))
  for (agent in agents) sim_agent_add(exchange, agent, agent_type = "human")
  execution <- sim_portfolio_execution(fee_rt = 0, lev = 1)
  first <- arena_bars("2026-08-04")
  sim_portfolio_market_step(exchange, first, execution)
  for (i in seq_along(agents)) {
    sim_portfolio_target_submit(
      exchange, agents[i], first,
      stats::setNames(1, arena_symbols[i]), execution,
      allowed_symbols = arena_symbols[i]
    )
  }
  second <- arena_bars("2026-08-05", seq(51, by = 10, length.out = length(arena_symbols)))
  sim_portfolio_market_step(exchange, second, execution)
  list(exchange = exchange, agents = agents, execution = execution, first = first, second = second)
}

test_that("single-asset Arena agents remain isolated across all active assets", {
  arena <- run_single_asset_arena()
  exchange <- arena$exchange
  positions <- sim_exchange_positions(exchange)
  orders <- sim_exchange_orders(exchange)

  for (i in seq_along(arena$agents)) {
    current_agent <- arena$agents[i]
    expected_symbol <- arena_symbols[i]
    expect_setequal(positions[positions$agent_id == current_agent, "symbol"], expected_symbol)
    expect_setequal(orders[orders$agent_id == current_agent, "symbol"], expected_symbol)
    expect_setequal(exchange$portfolio_fills[exchange$portfolio_fills$agent_id == current_agent, "symbol"], expected_symbol)
    fills <- exchange$portfolio_fills[exchange$portfolio_fills$agent_id == current_agent]
    expect_equal(unname(positions$ctr_unit[positions$agent_id == current_agent]), unname(fills$ctr_qty))
  }

  third <- arena_bars("2026-08-06", seq(52, by = 10, length.out = length(arena_symbols)))
  sim_portfolio_market_step(exchange, third, arena$execution)
  uso_agent <- arena$agents[match("USO", arena_symbols)]
  result <- sim_portfolio_target_submit(exchange, uso_agent, third, c(USO = 1), arena$execution)
  expect_true(all(result$orders$symbol == "USO"))
  rejected <- sim_portfolio_target_submit(exchange, uso_agent, third, c(SPY = 1), arena$execution)
  expect_equal(rejected$outcomes$status, "rejected")
  expect_match(rejected$outcomes$message, "outside the agent allowed universe")
})

test_that("allowed universes survive save/load and replay deterministically", {
  first <- run_single_asset_arena()
  state_dir <- tempfile("tradesimr-arena-isolation-")
  sim_exchange_save(first$exchange, state_dir)
  loaded <- sim_exchange_load(state_dir)
  uso_agent <- first$agents[match("USO", arena_symbols)]
  next_bars <- arena_bars("2026-08-06", seq(52, by = 10, length.out = length(arena_symbols)))
  sim_portfolio_market_step(loaded, next_bars, first$execution)
  resumed <- sim_portfolio_target_submit(loaded, uso_agent, next_bars, c(USO = 1), first$execution)
  expect_true(all(resumed$targets$symbol == "USO"))
  loaded_positions <- sim_exchange_positions(loaded)
  expect_setequal(loaded_positions[loaded_positions$agent_id == uso_agent, "symbol"], "USO")

  second <- run_single_asset_arena()
  expect_equal(second$exchange$agent_orders, first$exchange$agent_orders)
  expect_equal(second$exchange$portfolio_fills, first$exchange$portfolio_fills)
  expect_equal(sim_exchange_positions(second$exchange), sim_exchange_positions(first$exchange))
})
