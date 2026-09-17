vox_target_bars <- function(timestamp, symbols, prices = NULL) {
  if (is.null(prices)) prices <- stats::setNames(rep(100, length(symbols)), symbols)
  data.table::data.table(
    timestamp = as.POSIXct(timestamp, tz = "UTC"),
    symbol = symbols,
    asset_id = seq_along(symbols),
    open = as.numeric(prices[symbols]), high = as.numeric(prices[symbols]) * 1.01,
    low = as.numeric(prices[symbols]) * .99, close = as.numeric(prices[symbols])
  )
}

test_that("complete-universe target groups reserve fees without losing atomicity", {
  exchange <- sim_exchange_new(list(
    init_cash = 1000000, lev = 1, portfolio_margin = TRUE,
    execution_engine = "heterogeneous_v2", calendar_mode = "raw"
  ))
  symbols <- paste0("ETF", seq_len(8))
  for (i in seq_along(symbols)) {
    sim_asset_add(exchange, symbols[i], asset_id = i, asset_class = "etf", qty_step = 1,
                  quote_ccy = "USD")
  }
  execution <- sim_portfolio_execution(fee_rt = 0.0005, lev = 1)
  first <- vox_target_bars("2026-10-01", symbols, stats::setNames(rep(100, 8), symbols))
  second <- vox_target_bars("2026-10-02", symbols, stats::setNames(rep(100.05, 8), symbols))
  sim_portfolio_market_step(exchange, first, execution)
  submitted <- sim_portfolio_target_submit(
    exchange, "arena-a", first, stats::setNames(rep(1 / 8, 8), symbols), execution,
    allowed_symbols = symbols
  )
  expect_true(all(submitted$outcomes$status == "accepted"))
  sim_portfolio_market_step(exchange, second, execution)
  expect_true(all(exchange$agent_orders$agent_id == "arena-a"))
  expect_true(all(exchange$agent_orders$status == "filled"))
  expect_true(all(exchange$agent_orders$reason_code == "fee_scaled"))
  expect_gt(sum(exchange$portfolio_fills$qty), 0)
  expect_lt(sum(exchange$portfolio_fills$qty * exchange$portfolio_fills$price), 1000000)
  quality <- sim_portfolio_execution_quality(exchange)
  expect_true(all(quality$execution_quality %in% c("partial", "fulfilled")))
  expect_true(any(quality$execution_quality == "partial"))
  expect_true(all(exchange$portfolio_fills$rebalance_id == exchange$agent_orders$rebalance_id))
})

test_that("synthetic price-return profile supports signed BTC target exposure", {
  exchange <- sim_exchange_new(list(
    init_cash = 1000000, lev = 1, portfolio_margin = TRUE,
    execution_engine = "heterogeneous_v2", calendar_mode = "raw"
  ))
  sim_asset_add(exchange, "SPY", asset_id = 1, asset_class = "etf", qty_step = 1, quote_ccy = "USD")
  sim_asset_add(exchange, "BTC-USD-RETURN", asset_id = 2,
                instrument_profile = "synthetic_price_return", qty_step = 0.001,
                quote_ccy = "USD")
  expect_equal(sim_asset_profile <- sim_assets(exchange)[asset_id == 2, instrument_profile], "synthetic_price_return")
  expect_match(sim_instrument_profile("synthetic_price_return")$limitations, "Synthetic signed price exposure")
  execution <- sim_portfolio_execution(fee_rt = 0.0005, lev = 1)
  symbols <- c("SPY", "BTC-USD-RETURN")
  first <- vox_target_bars("2026-10-01", symbols, c(SPY = 100, `BTC-USD-RETURN` = 50000))
  second <- vox_target_bars("2026-10-02", symbols, c(SPY = 100.25, `BTC-USD-RETURN` = 49900))
  sim_portfolio_market_step(exchange, first, execution)
  submitted <- sim_portfolio_target_submit(
    exchange, "arena-short", first, c(SPY = .5, `BTC-USD-RETURN` = -.5), execution,
    allowed_symbols = symbols
  )
  expect_true(all(submitted$outcomes$status == "accepted"))
  expect_false(any(exchange$portfolio_targets$status == "rejected"))
  sim_portfolio_market_step(exchange, second, execution)
  btc_order <- exchange$agent_orders[symbol == "BTC-USD-RETURN"]
  expect_equal(btc_order$status, "filled")
  expect_true(btc_order$side %in% c("sell", "buy"))
  expect_lt(exchange$typed_margin_positions[asset_id == 2, signed_units], 0)
  quality <- sim_portfolio_execution_quality(exchange)
  expect_true(all(quality$execution_quality %in% c("fulfilled", "partial")))
  expect_true(all(c("agent_id", "order_id", "rebalance_id", "symbol", "asset_id") %in% names(exchange$portfolio_fills)))
})
