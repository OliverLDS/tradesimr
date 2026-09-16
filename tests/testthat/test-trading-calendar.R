test_that("built-in calendars apply sessions and observed fixed holidays", {
  ny_open <- as.POSIXct("2026-01-05 15:00:00", tz = "UTC") # 10:00 ET
  ny_closed <- as.POSIXct("2026-01-05 13:00:00", tz = "UTC") # 08:00 ET
  july_observed <- as.POSIXct("2026-07-03 15:00:00", tz = "UTC")
  expect_true(sim_calendar_is_open(ny_open, "XNYS"))
  expect_false(sim_calendar_is_open(ny_closed, "XNYS"))
  expect_false(sim_calendar_is_open(july_observed, "XNYS"))
  expect_true(sim_calendar_is_open(as.POSIXct("2026-01-04 12:00:00", tz = "UTC"), "CRYPTO_24_7"))
  expect_false(sim_calendar_is_open(as.POSIXct("2026-01-04 12:00:00", tz = "UTC"), "FX_24_5"))
})

test_that("calendarization and cadence are registered-asset aware", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "SPY", asset_id = 1L, asset_class = "equity", bar_cadence_seconds = 300)
  bars <- data.frame(
    timestamp = as.POSIXct(c("2026-01-05 14:30:00", "2026-01-05 14:35:00", "2026-01-05 14:37:00"), tz = "UTC"),
    symbol = "SPY", asset_id = 1L, open = 100, high = 101, low = 99, close = 100
  )
  checked <- sim_exchange_validate_cadence(exchange, bars)
  expect_true(checked$is_tradable[1L])
  expect_true(checked$cadence_ok[2L])
  expect_false(checked$cadence_ok[3L])
  expect_error(sim_exchange_validate_cadence(exchange, bars, strict = TRUE), "cadence")
})

test_that("valuation-only bars never execute accepted orders", {
  exchange <- sim_exchange_new(list(cash = 1000, lev = 1))
  sim_asset_add(exchange, "SPY", asset_id = 1L, asset_class = "equity")
  decision_time <- as.POSIXct("2026-01-05", tz = "UTC")
  sim_exchange_place_order(exchange, "alice", decision_time, symbol = "SPY", side = "buy", qty = 1)
  closed_bar <- data.frame(
    timestamp = decision_time + 86400, symbol = "SPY", asset_id = 1L,
    open = 100, high = 100, low = 100, close = 100,
    is_completed = TRUE, is_tradable = FALSE
  )
  sim_exchange_step(exchange, closed_bar)
  expect_equal(nrow(exchange$market_events), 1L)
  expect_equal(nrow(sim_fills(exchange$result)), 0L)
  expect_equal(sim_exchange_orders(exchange)$status, "accepted")

  open_bar <- closed_bar
  open_bar$timestamp <- open_bar$timestamp + 86400
  open_bar$is_tradable <- TRUE
  sim_exchange_step(exchange, open_bar)
  expect_equal(sim_exchange_orders(exchange)$status, "filled")
})
