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

test_that("calendar modes control executable portfolio boundaries", {
  early_nyse <- as.POSIXct("2026-01-05 13:00:00", tz = "UTC") # 08:00 ET
  bar <- data.frame(
    timestamp = early_nyse, symbol = "SPY", asset_id = 1L,
    open = 100, high = 100, low = 100, close = 100
  )
  calendarized <- sim_exchange_new(list(calendar_mode = "calendarize"))
  sim_asset_add(calendarized, "SPY", asset_id = 1L, asset_class = "equity")
  sim_exchange_place_order(calendarized, "alice", early_nyse - 60, symbol = "SPY", side = "buy", qty = 1)
  result <- sim_portfolio_market_step(calendarized, bar)
  expect_equal(result$outcomes$status, "valuation_only")
  expect_equal(nrow(calendarized$portfolio_market_boundaries), 0L)
  expect_equal(sim_exchange_orders(calendarized)$status, "accepted")
  expect_error(
    sim_portfolio_target_submit(calendarized, "alice", result$bars, c(SPY = 1)),
    "fresh, completed, tradable"
  )

  strict <- sim_exchange_new(list(calendar_mode = "strict"))
  sim_asset_add(strict, "SPY", asset_id = 1L, asset_class = "equity")
  expect_error(sim_exchange_step(strict, bar), "outside its registered calendar session")
})

test_that("calendar modes validate cadence against the prior accepted bar", {
  exchange <- sim_exchange_new(list(calendar_mode = "strict"))
  sim_asset_add(exchange, "BTC-USD", asset_id = 1L, asset_class = "crypto_spot", bar_cadence_seconds = 300)
  first <- data.frame(
    timestamp = as.POSIXct("2026-01-05 00:00:00", tz = "UTC"), symbol = "BTC-USD", asset_id = 1L,
    open = 100, high = 100, low = 100, close = 100
  )
  sim_exchange_step(exchange, first)
  misaligned <- first
  misaligned$timestamp <- misaligned$timestamp + 7 * 60
  expect_error(sim_exchange_step(exchange, misaligned), "bar cadence")
})

test_that("calendar specifications generate holidays, early closes, and expected bars", {
  holidays <- sim_calendar_holidays("XNYS", as.Date("2026-11-25"), as.Date("2026-11-28"))
  expect_true(any(holidays$label == "Thanksgiving"))
  expect_true(any(holidays$label == "Black Friday" & holidays$close_time == "13:00"))
  expected <- sim_calendar_expected_bars(
    "XNYS", as.POSIXct("2026-11-27 14:00:00", tz = "UTC"),
    as.POSIXct("2026-11-27 20:00:00", tz = "UTC"), 3600
  )
  expect_true(nrow(expected) > 0L)
  expect_true(all(sim_calendar_is_open(expected$timestamp - 1, "XNYS")))
  expect_false(sim_calendar_is_open(as.POSIXct("2026-11-27 18:30:00", tz = "UTC"), "XNYS")) # 13:30 ET
})

test_that("durable exchange exceptions override calendar sessions", {
  exchange <- sim_exchange_new(list(calendar_mode = "calendarize"))
  sim_asset_add(exchange, "SPY", asset_id = 1L, instrument_profile = "equity")
  sim_exchange_calendar_exception(exchange, "2026-01-05", "closed", symbol = "SPY", message = "Test closure")
  bar <- data.frame(timestamp = as.POSIXct("2026-01-05 15:00:00", tz = "UTC"), symbol = "SPY", asset_id = 1L,
    open = 100, high = 100, low = 100, close = 100)
  expect_false(sim_exchange_calendarize_bars(exchange, bar)$is_tradable)
  path <- tempfile("calendar-exception-")
  sim_exchange_save(exchange, path)
  expect_equal(sim_exchange_load(path)$calendar_exceptions[, !"created_at"], exchange$calendar_exceptions[, !"created_at"])
})
