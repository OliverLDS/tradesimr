test_that("instrument profiles provide deterministic asset defaults", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "SPY", asset_id = 1L, asset_class = "stock", quote_ccy = "USD")
  sim_asset_add(exchange, "BTC-USD", asset_id = 2L, instrument_profile = "crypto_spot", qty_step = .001, quote_ccy = "USD")
  assets <- sim_assets(exchange)
  expect_equal(assets[symbol == "SPY", instrument_profile], "equity")
  expect_equal(assets[symbol == "SPY", calendar_id], "XNYS")
  expect_equal(assets[symbol == "BTC-USD", accounting_model], "spot")
  expect_error(sim_asset_add(exchange, "bad", instrument_profile = "option"), "Unsupported")
})

test_that("market bars retain explicit observation and valuation timestamps", {
  timestamp <- as.POSIXct("2026-09-14 16:00:00", tz = "UTC")
  bars <- as_market_bars(data.frame(
    timestamp = timestamp, symbol = "SPY", asset_id = 1L,
    open = 100, high = 101, low = 99, close = 100
  ))
  expect_equal(bars$observation_timestamp, timestamp)
  expect_equal(bars$bar_end, timestamp)
  expect_equal(bars$valuation_timestamp, timestamp)
  expect_true(bars$is_completed)
  expect_true(bars$is_tradable)
})

test_that("schema migration adds typed fields without discarding extensions", {
  migrated <- sim_schema_migrate(list(
    assets = data.table::data.table(asset_id = 1L, symbol = "SPY", custom_value = "kept"),
    market_events = data.table::data.table(timestamp = as.POSIXct("2026-09-14", tz = "UTC"), symbol = "SPY", asset_id = 1L, open = 1, high = 1, low = 1, close = 1)
  ))
  expect_true(all(c("instrument_profile", "calendar_id", "settlement_lag_days") %in% names(migrated$assets)))
  expect_true(all(c("observation_timestamp", "valuation_timestamp", "is_completed") %in% names(migrated$market_events)))
  expect_equal(migrated$assets$custom_value, "kept")
  expect_type(migrated$assets$settlement_lag_days, "integer")
  expect_s3_class(migrated$market_events$observation_timestamp, "POSIXct")
  expect_identical(migrated$assets$instrument_profile, "other")
  expect_identical(attr(migrated, "source_schema_version"), NA_character_)
})

test_that("exchange save/load upgrades legacy asset tables", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "SPY", asset_id = 1L, asset_class = "stock")
  path <- tempfile("tradesimr-schema-")
  sim_exchange_save(exchange, path)
  data.table::fwrite(data.table::data.table(
    asset_id = 1L, symbol = "SPY", status = "active", asset_class = "stock",
    contract_size = 1, tick_size = NA_real_, qty_step = 1,
    base_ccy = NA_character_, quote_ccy = "USD", created_at = Sys.time()
  ), file.path(path, "assets.csv"))
  loaded <- sim_exchange_load(path)
  expect_true(all(c("instrument_profile", "calendar_id", "accounting_model") %in% names(loaded$assets)))
  expect_identical(loaded$assets$instrument_profile, "equity")
  expect_identical(loaded$assets$calendar_id, "XNYS")
  expect_true("carry_accruals" %in% names(as.list(loaded, all.names = TRUE)))
  expect_equal(loaded$config$schema_version, sim_schema_version())
})

test_that("legacy manifests import through explicit versioned migration", {
  path <- tempfile("tradesimr-legacy-export-")
  dir.create(path)
  data.table::fwrite(data.table::data.table(
    schema_version = "0.12.0", package_version = "0.12.0", created_at = "2025-01-01T00:00:00Z",
    format = "csv", table = "assets", file = "assets.csv", rows = 1L, config = ""
  ), file.path(path, "manifest.csv"))
  data.table::fwrite(data.table::data.table(asset_id = 1L, symbol = "TLT", asset_class = "etf"),
    file.path(path, "assets.csv"))
  imported <- sim_import(path)
  expect_identical(imported$assets$instrument_profile, "etf")
  expect_identical(imported$assets$calendar_id, "XNYS")
  expect_identical(attr(imported$assets, "schema_version"), NULL)
  expect_identical(attr(sim_schema_migrate(list(assets = imported$assets), from_version = "0.12.0"), "source_schema_version"), "0.12.0")
})

test_that("instrument contract states explicit generic-profile limitations", {
  profiles <- sim_instrument_profiles()
  expect_true(all(c("settlement_model", "funding_or_carry", "corporate_action_hooks", "limitations") %in% names(profiles)))
  expect_match(profiles[instrument_profile == "other", limitations], "unsupported")
})

test_that("generic other assets reject unsupported lifecycle actions", {
  exchange <- sim_exchange_new()
  sim_asset_add(exchange, "CUSTOM-INDEX", asset_id = 99L, instrument_profile = "other")
  expect_error(
    sim_exchange_corporate_action(exchange, "CUSTOM-INDEX", "dividend", 1,
      as.POSIXct("2025-01-01", tz = "UTC")),
    "does not support corporate actions"
  )
})
