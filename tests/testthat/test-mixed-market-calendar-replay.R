test_that("mixed SPY TLT EURUSD and BTC replay respects independent sessions", {
  exchange <- sim_exchange_new(list(calendar_mode = "calendarize", cash = 100000, portfolio_margin = TRUE, lev = 1))
  sim_asset_add(exchange, "SPY", asset_id = 1L, instrument_profile = "equity", quote_ccy = "USD")
  sim_asset_add(exchange, "TLT", asset_id = 2L, instrument_profile = "etf", quote_ccy = "USD")
  sim_asset_add(exchange, "EURUSD", asset_id = 3L, instrument_profile = "fx_spot", quote_ccy = "USD")
  sim_asset_add(exchange, "BTC-USD", asset_id = 4L, instrument_profile = "crypto_spot", quote_ccy = "USD", qty_step = .001)
  bar <- function(timestamp, symbol, asset_id, price) data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"), symbol = symbol, asset_id = asset_id,
    open = price, high = price, low = price, close = price
  )
  weekend <- data.table::rbindlist(list(
    bar("2026-01-04 16:00:00", "SPY", 1L, 600), bar("2026-01-04 16:00:00", "TLT", 2L, 90),
    bar("2026-01-04 16:00:00", "EURUSD", 3L, 1.1), bar("2026-01-04 16:00:00", "BTC-USD", 4L, 40000)
  ))
  weekend_result <- sim_portfolio_market_step(exchange, weekend)
  expect_equal(weekend_result$outcomes$status[weekend_result$bars$symbol == "BTC-USD"], "market_stepped")
  expect_true(all(weekend_result$outcomes$status[weekend_result$bars$symbol != "BTC-USD"] == "valuation_only"))
  expect_setequal(exchange$portfolio_market_boundaries$symbol, "BTC-USD")

  monday <- data.table::rbindlist(list(
    bar("2026-01-05 15:00:00", "SPY", 1L, 601), bar("2026-01-05 15:00:00", "TLT", 2L, 91),
    bar("2026-01-05 15:00:00", "EURUSD", 3L, 1.11), bar("2026-01-05 15:00:00", "BTC-USD", 4L, 40100)
  ))
  sim_portfolio_market_step(exchange, monday)
  expect_setequal(exchange$portfolio_market_boundaries[timestamp == monday$timestamp[1L], symbol], c("SPY", "TLT", "EURUSD", "BTC-USD"))
  panel <- data.table::data.table(timestamp = monday$timestamp[1L], agent_id = "arena", symbol = c("SPY", "TLT", "EURUSD", "BTC-USD"), target_weight = c(.25, .25, .25, .25))
  replay_exchange <- sim_exchange_new(list(calendar_mode = "calendarize", cash = 100000))
  source_assets <- sim_assets(exchange)
  for (i in seq_len(nrow(source_assets))) {
    row <- source_assets[i]
    sim_asset_add(replay_exchange, row$symbol, row$asset_id, instrument_profile = row$instrument_profile, quote_ccy = row$quote_ccy, qty_step = row$qty_step)
  }
  expect_error(sim_portfolio_target_replay(replay_exchange, monday, panel, production_calendar = TRUE), NA)
  raw_exchange <- sim_exchange_new(list(cash = 100000))
  for (i in seq_len(nrow(source_assets))) {
    row <- source_assets[i]
    sim_asset_add(raw_exchange, row$symbol, row$asset_id, instrument_profile = row$instrument_profile, quote_ccy = row$quote_ccy, qty_step = row$qty_step)
  }
  expect_error(sim_portfolio_target_replay(raw_exchange, monday, panel, production_calendar = TRUE), "Production portfolio replay requires")
})
