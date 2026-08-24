make_margin_clip_bars <- function(timestamp, price = 100) {
  data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"), symbol = "TLT", asset_id = 1L,
    open = price, high = price, low = price, close = price
  )
}

test_that("target-derived TLT short increases clip at portfolio margin capacity", {
  agents <- c("buy_hold--tlt", "ema_cross_adx--tlt", "donchian_turtle--tlt", "vol_target--tlt")
  exchange <- sim_exchange_new(list(
    cash = 125230.2, lev = 1, portfolio_margin = TRUE, portfolio_margin_floor = 1.01
  ))
  sim_asset_add(exchange, "TLT", asset_id = 1L, asset_class = "etf", qty_step = 0.001)
  execution <- sim_portfolio_execution(lev = 1, fee_rt = 0)
  day_1 <- make_margin_clip_bars("2026-08-15", 100)
  day_2 <- make_margin_clip_bars("2026-08-16", 100)
  day_3 <- make_margin_clip_bars("2026-08-17", 100)
  day_4 <- make_margin_clip_bars("2026-08-18", 100.1)
  initial_weight <- -1224.603 * 100 / 125230.2

  sim_portfolio_market_step(exchange, day_1, execution)
  sim_portfolio_target_submit_batch(exchange, day_1, stats::setNames(lapply(agents, function(...) {
    list(target_weights = c(TLT = initial_weight), allowed_symbols = "TLT")
  }), agents), execution)
  sim_portfolio_market_step(exchange, day_2, execution)
  sim_portfolio_market_step(exchange, day_3, execution)
  submitted <- sim_portfolio_target_submit_batch(exchange, day_3, stats::setNames(lapply(agents, function(...) {
    list(target_weights = c(TLT = -1), allowed_symbols = "TLT")
  }), agents), execution)
  sim_portfolio_market_step(exchange, day_4, execution)

  rebalances <- exchange$portfolio_targets[
    timestamp == day_3$timestamp[1L] & agent_id %in% agents,
    rebalance_id
  ]
  orders <- exchange$agent_orders[exchange$agent_orders$rebalance_id %in% rebalances]
  fills <- exchange$portfolio_fills[exchange$portfolio_fills$rebalance_id %in% rebalances]
  quality <- sim_portfolio_execution_quality(exchange)[rebalance_id %in% rebalances]

  expect_equal(nrow(orders), length(agents))
  expect_equal(nrow(fills), length(agents))
  expect_true(all(orders$status == "filled"))
  expect_equal(exchange$portfolio_targets[rebalance_id %in% rebalances, planned_signed_quantity], rep(-1252.302, length(agents)))
  expect_equal(exchange$portfolio_targets[rebalance_id %in% rebalances, decision_equity], rep(125230.2, length(agents)))
  expect_equal(orders$qty, rep(27.699, length(agents)))
  expect_true(all(orders$reason_code == "margin_clipped"))
  expect_true(all(fills$reason_code == "margin_clipped"))
  expect_true(all(fills$qty > 0 & fills$qty < orders$qty))
  expect_true(all(quality$execution_quality == "partial"))
  expect_true(all(grepl("portfolio-margin capacity", quality$message, fixed = TRUE)))
  expect_true(all(abs(quality$realized_signed_quantity) < abs(quality$expected_signed_quantity)))
  expect_true(all(quality$realized_signed_quantity < 0))
})

test_that("explicit infeasible contract orders remain terminally rejected", {
  exchange <- sim_exchange_new(list(
    cash = 10000, lev = 1, portfolio_margin = TRUE, portfolio_margin_floor = 1.2
  ))
  sim_asset_add(exchange, "TLT", asset_id = 1L, asset_class = "etf", qty_step = 0.001)
  execution <- sim_portfolio_execution(lev = 1, fee_rt = 0)
  bar <- make_margin_clip_bars("2026-08-18", 100)
  sim_exchange_place_order(
    exchange, agent_id = "explicit", symbol = "TLT", side = "sell", qty = 1000,
    qty_type = "contracts", timestamp = bar$timestamp - 1
  )

  sim_portfolio_market_step(exchange, bar, execution)
  order <- exchange$agent_orders[agent_id == "explicit"]
  expect_equal(order$status, "rejected")
  expect_equal(order$reason_code, "execution_rejected")
  expect_equal(nrow(exchange$portfolio_fills[agent_id == "explicit"]), 0L)
})
