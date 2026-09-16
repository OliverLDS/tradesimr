test_that("heterogeneous account kernel settles long and short futures variation margin", {
  positions <- data.frame(asset_id = c(1L, 2L), currency = c("USD", "USD"),
    signed_units = c(2, -3), settlement_price = c(100, 100), last_price = c(100, 100),
    contract_size = c(10, 10), maintenance_rate = c(.1, .1))
  bars <- data.frame(asset_id = c(1L, 2L), close = c(110, 90), instrument_profile = c("future", "future"))
  out <- sim_heterogeneous_account_step("USD", data.frame(currency = "USD", settled = 1000, unsettled = 0), data.frame(), positions, bars, data.frame(currency = "USD", rate_to_base = 1), timestamp = as.POSIXct("2025-01-01", tz = "UTC"))
  expect_equal(out$cash_balances$settled, 1500)
  expect_equal(out$margin_positions$settlement_price, c(110, 90))
  expect_equal(out$equity, 1500)
  expect_equal(out$maintenance_margin, 490)
  expect_identical(out$events$event_type, c("variation_margin", "variation_margin"))
})

test_that("heterogeneous account kernel settles non-base futures in native currency", {
  positions <- data.frame(asset_id = 7L, currency = "EUR", signed_units = 2,
    settlement_price = 100, last_price = 100, contract_size = 10,
    maintenance_rate = .1)
  inventory <- data.frame(asset_id = 8L, currency = "EUR", units = 3,
    average_cost = 40, last_price = 40, contract_size = 1)
  bars <- data.frame(asset_id = c(7L, 8L), close = c(110, 50),
    instrument_profile = c("future", "equity"))
  out <- sim_heterogeneous_account_step(
    "USD",
    data.frame(currency = c("USD", "EUR"), settled = c(1000, 500), unsettled = c(0, 0)),
    inventory, positions, bars,
    data.frame(currency = c("USD", "EUR"), rate_to_base = c(1, 1.2)),
    timestamp = as.POSIXct("2025-01-01", tz = "UTC")
  )
  expect_equal(out$cash_balances$settled, c(1000, 700))
  expect_equal(out$margin_positions$settlement_price, 110)
  expect_equal(out$inventory_positions$last_price, 50)
  expect_equal(out$equity, 2020)
  expect_equal(out$maintenance_margin, 264)
  expect_equal(out$events$amount, 200)
  expect_equal(out$events$currency, "EUR")
})

test_that("heterogeneous account kernel books explicit bond lifecycle actions", {
  bond <- data.frame(
    asset_id = 9L, currency = "USD", units = 10, average_cost = 98,
    last_price = 101, contract_size = 1
  )
  actions <- data.frame(
    action_id = c("CA1", "CA2", "CA3"), asset_id = c(9L, 9L, 9L),
    action_type = c("coupon", "bond_accrual", "redemption"),
    amount = c(2, 1, 100), currency = c("USD", "USD", "USD"),
    effective_timestamp = rep(as.numeric(as.POSIXct("2025-01-01", tz = "UTC")), 3L)
  )
  out <- sim_heterogeneous_account_step(
    "USD", data.frame(currency = "USD", settled = 1000, unsettled = 0), bond,
    data.frame(asset_id = integer(), currency = character(), signed_units = numeric(),
      settlement_price = numeric(), last_price = numeric(), contract_size = numeric(), maintenance_rate = numeric()),
    data.frame(asset_id = 9L, close = 101, instrument_profile = "bond"),
    data.frame(currency = "USD", rate_to_base = 1), corporate_actions = actions,
    timestamp = as.POSIXct("2025-01-01", tz = "UTC")
  )
  expect_equal(out$cash_balances$settled, 2030)
  expect_equal(out$inventory_positions$units, 0)
  expect_true(is.na(out$inventory_positions$average_cost))
  expect_identical(out$events$event_type, c("bond_coupon", "bond_accrual", "redemption"))
  expect_equal(out$events$amount, c(20, 10, 1010))
  expect_equal(out$events$cash_effect, c(TRUE, FALSE, TRUE))
})

test_that("heterogeneous account kernel evaluates a fixed ACT bond schedule", {
  issue <- as.POSIXct("2025-01-01", tz = "UTC")
  coupon_boundary <- issue + 365 * 86400 / 2
  out <- sim_heterogeneous_account_step(
    "USD", data.frame(currency = "USD", settled = 1000, unsettled = 0),
    data.frame(asset_id = 10L, currency = "USD", units = 10, average_cost = 100,
      last_price = 100, contract_size = 1),
    data.frame(asset_id = integer(), currency = character(), signed_units = numeric(),
      settlement_price = numeric(), last_price = numeric(), contract_size = numeric(), maintenance_rate = numeric()),
    data.frame(asset_id = 10L, close = 100, instrument_profile = "bond"),
    data.frame(currency = "USD", rate_to_base = 1),
    corporate_actions = data.frame(asset_id = 10L, schedule_type = "bond", currency = "USD",
      coupon_rate = .1, coupon_frequency = 2, face_value = 100, accrual_day_count = 365,
      last_accrual_timestamp = as.numeric(issue), next_coupon_timestamp = as.numeric(coupon_boundary),
      maturity_timestamp = as.numeric(issue + 365 * 86400)),
    timestamp = coupon_boundary
  )
  expect_equal(out$cash_balances$settled, 1050)
  expect_identical(out$events$event_type, c("bond_accrual", "bond_coupon"))
  expect_identical(out$events$cash_effect, c(FALSE, TRUE))
  expect_equal(out$events$amount, c(50, 50))
})

test_that("bond schedules retain post-coupon accrual across sparse boundaries", {
  issue <- as.POSIXct("2025-01-01", tz = "UTC")
  cutoff <- issue + 365 * 86400 * 3 / 4
  out <- sim_heterogeneous_account_step(
    "USD", data.frame(currency = "USD", settled = 1000, unsettled = 0),
    data.frame(asset_id = 11L, currency = "USD", units = 10, average_cost = 100,
      last_price = 100, contract_size = 1),
    data.frame(asset_id = integer(), currency = character(), signed_units = numeric(),
      settlement_price = numeric(), last_price = numeric(), contract_size = numeric(), maintenance_rate = numeric()),
    data.frame(asset_id = 11L, close = 100, instrument_profile = "bond"),
    data.frame(currency = "USD", rate_to_base = 1),
    corporate_actions = data.frame(asset_id = 11L, schedule_type = "bond", currency = "USD",
      coupon_rate = .1, coupon_frequency = 2, face_value = 100, accrual_day_count = 365,
      last_accrual_timestamp = as.numeric(issue), next_coupon_timestamp = as.numeric(issue + 365 * 86400 / 2),
      maturity_timestamp = as.numeric(issue + 365 * 86400)),
    timestamp = cutoff
  )
  expect_equal(out$cash_balances$settled, 1050)
  expect_equal(out$inventory_positions$accrued_interest, 25)
  expect_identical(out$events$event_type, c("bond_accrual", "bond_coupon", "bond_accrual"))
  expect_equal(out$events$amount, c(50, 50, 25))
  expect_equal(out$equity, 2075)
})

test_that("bond accrual is a durable inventory receivable before coupon settlement", {
  issue <- as.POSIXct("2025-01-01", tz = "UTC")
  accrual_boundary <- issue + 90 * 86400
  out <- sim_heterogeneous_account_step(
    "USD", data.frame(currency = "USD", settled = 1000, unsettled = 0),
    data.frame(asset_id = 12L, currency = "USD", units = 10, average_cost = 100,
      last_price = 100, contract_size = 1),
    data.frame(asset_id = integer(), currency = character(), signed_units = numeric(),
      settlement_price = numeric(), last_price = numeric(), contract_size = numeric(), maintenance_rate = numeric()),
    data.frame(asset_id = 12L, close = 100, instrument_profile = "bond"),
    data.frame(currency = "USD", rate_to_base = 1),
    corporate_actions = data.frame(asset_id = 12L, schedule_type = "bond", currency = "USD",
      coupon_rate = .1, coupon_frequency = 2, face_value = 100, accrual_day_count = 365,
      last_accrual_timestamp = as.numeric(issue), next_coupon_timestamp = as.numeric(issue + 365 * 86400 / 2),
      maturity_timestamp = as.numeric(issue + 365 * 86400)),
    timestamp = accrual_boundary
  )
  accrued <- 1000 * .1 * 90 / 365
  expect_equal(out$cash_balances$settled, 1000)
  expect_equal(out$inventory_positions$accrued_interest, accrued)
  expect_equal(out$equity, 2000 + accrued)
  expect_identical(out$events$cash_effect, FALSE)
})

test_that("heterogeneous account kernel executes profile-tagged spot and futures orders", {
  out <- sim_heterogeneous_account_step(
    "USD", data.frame(currency = "USD", settled = 1000, unsettled = 0),
    data.frame(asset_id = 1L, currency = "USD", units = 0, average_cost = NA_real_, last_price = 100, contract_size = 1),
    data.frame(asset_id = 2L, currency = "USD", signed_units = 0, settlement_price = NA_real_, last_price = 50, contract_size = 10, maintenance_rate = .1),
    data.frame(asset_id = integer(), close = numeric()),
    data.frame(currency = "USD", rate_to_base = 1),
    orders = data.frame(
      order_id = c("O1", "O2"), asset_id = c(1L, 2L),
      instrument_profile = c("equity", "future"), side = c("buy", "sell"),
      qty = c(2, 1), order_type = c("market", "market"), limit_price = c(NA_real_, NA_real_),
      execution_price = c(100, 50), fee_rt = c(.01, .01),
      eligible_after = as.POSIXct(c("2024-12-31", "2024-12-31"), tz = "UTC"),
      atomic_group_id = c("G1", "G1"), target_derived = c(FALSE, FALSE), time_in_force = c("gtc", "gtc")
    ), timestamp = as.POSIXct("2025-01-01", tz = "UTC")
  )
  expect_true(all(out$fills$status == "filled"))
  expect_equal(out$inventory_positions$units, 2)
  expect_equal(out$margin_positions$signed_units, -1)
  expect_equal(out$cash_balances$settled, 793)
})

test_that("heterogeneous mixed account liquidates on unified inventory plus margin equity", {
  out <- sim_heterogeneous_account_step(
    "USD", data.frame(currency = "USD", settled = 100, unsettled = 0),
    data.frame(asset_id = 1L, currency = "USD", units = 1, average_cost = 100, last_price = 100, contract_size = 1),
    data.frame(asset_id = 2L, currency = "USD", signed_units = -10, settlement_price = 100, last_price = 100, contract_size = 1, maintenance_rate = .1),
    data.frame(asset_id = c(1L, 2L), close = c(200, 200), instrument_profile = c("equity", "future")),
    data.frame(currency = "USD", rate_to_base = 1),
    timestamp = as.POSIXct("2025-01-02", tz = "UTC")
  )
  expect_true(out$liquidated)
  expect_lt(out$equity, out$maintenance_margin)
  expect_equal(out$events$event_type, "variation_margin")
})

test_that("incremental future exchange books durable variation margin without changing the legacy portfolio kernel", {
  exchange <- sim_exchange_new(list(cash = 1000, lev = 10, mmr = .02))
  sim_asset_add(exchange, "ES", asset_id = 1L, instrument_profile = "future", quote_ccy = "USD", contract_size = 10)
  day1 <- as.POSIXct("2025-01-01", tz = "UTC")
  sim_exchange_place_order(exchange, "trader", day1, symbol = "ES", side = "buy", qty = 1)
  sim_exchange_step(exchange, data.frame(timestamp = day1, symbol = "ES", asset_id = 1L, open = 100, high = 101, low = 99, close = 100))
  sim_exchange_step(exchange, data.frame(timestamp = day1 + 86400, symbol = "ES", asset_id = 1L, open = 110, high = 111, low = 109, close = 110))
  state <- exchange$agent_states[[tradesimr:::.agent_state_key("trader", 1L)]]
  expect_equal(state$cash, 1100)
  expect_equal(state$avg_price, 110)
  expect_true(any(exchange$step_events$event_type_label == "variation_margin"))
  expect_true(any(exchange$profile_cash_ledger$event_type == "variation_margin"))
})

test_that("incremental futures settle variation margin in native currency", {
  exchange <- sim_exchange_new(list(cash = 1000, lev = 10, mmr = .02, base_currency = "USD"))
  sim_asset_add(exchange, "FESX", asset_id = 2L, instrument_profile = "future", quote_ccy = "EUR", contract_size = 10)
  sim_exchange_fx_rate(exchange, "EUR", "USD", 1.2)
  day1 <- as.POSIXct("2025-01-01", tz = "UTC")
  sim_exchange_place_order(exchange, "trader", day1, symbol = "FESX", side = "buy", qty = 1)
  sim_exchange_step(exchange, data.frame(timestamp = day1, symbol = "FESX", asset_id = 2L, open = 100, high = 101, low = 99, close = 100))
  sim_exchange_step(exchange, data.frame(timestamp = day1 + 86400, symbol = "FESX", asset_id = 2L, open = 110, high = 111, low = 109, close = 110))
  balances <- sim_exchange_cash_balances(exchange, "trader")
  expect_equal(balances[currency == "EUR", amount], 100)
  expect_equal(balances[currency == "EUR", base_value], 120)
  expect_equal(tail(exchange$step_snapshots$equity, 1L), 1120)
  expect_equal(tail(exchange$profile_cash_ledger[event_type == "variation_margin", amount], 1L), 100)
})

test_that("heterogeneous account liquidation clears spot and futures together", {
  exchange <- sim_exchange_new(list(cash = 100, lev = 10, mmr = 1, base_currency = "USD", portfolio_margin = TRUE))
  sim_asset_add(exchange, "EQ", asset_id = 3L, instrument_profile = "equity", quote_ccy = "USD")
  sim_asset_add(exchange, "FUT", asset_id = 4L, instrument_profile = "future", quote_ccy = "USD")
  day1 <- as.POSIXct("2025-01-01", tz = "UTC")
  # Inventory orders require a strictly later eligible boundary. Submit this
  # leg before day 1 so both profile exposures exist before the adverse bar.
  sim_exchange_place_order(exchange, "trader", day1 - 86400, symbol = "EQ", side = "buy", qty = 1)
  sim_exchange_place_order(exchange, "trader", day1, symbol = "FUT", side = "sell", qty = 1)
  sim_exchange_step(exchange, data.frame(
    timestamp = c(day1, day1), symbol = c("EQ", "FUT"), asset_id = c(3L, 4L),
    open = c(80, 10), high = c(81, 11), low = c(79, 9), close = c(80, 10)
  ))
  sim_exchange_step(exchange, data.frame(
    timestamp = c(day1 + 86400, day1 + 86400), symbol = c("EQ", "FUT"), asset_id = c(3L, 4L),
    open = c(1, 20), high = c(1, 21), low = c(1, 19), close = c(1, 20)
  ))
  expect_true(isTRUE(exchange$agent_accounts[["trader"]]$liquidated))
  expect_equal(exchange$spot_states[[tradesimr:::.agent_state_key("trader", 3L)]]$units, 0)
  expect_equal(exchange$agent_states[[tradesimr:::.agent_state_key("trader", 4L)]]$ctr_unit, 0)
  expect_true(any(exchange$event_log$event == "cross_margin_liquidation"))
})

test_that("portfolio-margin future batches run post-step risk through heterogeneous valuation", {
  exchange <- sim_exchange_new(list(cash = 1000, lev = 10, mmr = .02, portfolio_margin = TRUE))
  sim_asset_add(exchange, "ES", asset_id = 5L, instrument_profile = "future", quote_ccy = "USD", contract_size = 10)
  day1 <- as.POSIXct("2025-01-01", tz = "UTC")
  sim_exchange_place_order(exchange, "trader", day1, symbol = "ES", side = "buy", qty = 1)
  expect_no_error(sim_exchange_step(exchange, data.frame(timestamp = day1, symbol = "ES", asset_id = 5L, open = 100, high = 101, low = 99, close = 100)))
  sim_exchange_step(exchange, data.frame(timestamp = day1 + 86400, symbol = "ES", asset_id = 5L, open = 110, high = 111, low = 109, close = 110))
  expect_false(isTRUE(exchange$agent_accounts[["trader"]]$liquidated))
  expect_true(any(exchange$step_events$event_type_label == "variation_margin"))
})

hetero_order_fixture <- function(orders, bars = data.frame(asset_id = 1L, open = 10, high = 10, low = 10, close = 10)) {
  sim_heterogeneous_account_step(
    "USD", data.frame(currency = "USD", settled = 100, unsettled = 0),
    data.frame(asset_id = 1L, currency = "USD", units = 0, average_cost = NA_real_, last_price = 10, contract_size = 1),
    data.frame(asset_id = integer(), currency = character(), signed_units = numeric(), settlement_price = numeric(), last_price = numeric(), contract_size = numeric(), maintenance_rate = numeric()),
    bars, data.frame(currency = "USD", rate_to_base = 1), orders = orders,
    timestamp = as.POSIXct("2026-01-02", tz = "UTC")
  )
}

hetero_order_rows <- function(qty, order_type = "market", limit_price = NA_real_, tif = "gtc", group = "G1") {
  data.frame(order_id = paste0("O", seq_along(qty)), asset_id = 1L, instrument_profile = "equity",
    side = "buy", qty = qty, order_type = order_type, limit_price = limit_price,
    execution_price = 10, fee_rt = 0, eligible_after = as.POSIXct("2026-01-01", tz = "UTC"),
    atomic_group_id = group, target_derived = FALSE, time_in_force = tif)
}

test_that("heterogeneous filled atomic group commits every leg", {
  out <- hetero_order_fixture(hetero_order_rows(c(2, 3)))
  expect_true(all(out$fills$status == "filled"))
  expect_equal(nrow(out$groups), 1L)
  expect_equal(out$groups$group_status, "committed")
  expect_true(out$groups$committed)
  expect_equal(out$inventory_positions$units, 5)
})

test_that("heterogeneous atomic rollback restores state after a later rejection", {
  out <- hetero_order_fixture(hetero_order_rows(c(2, 20)))
  expect_true(all(out$fills$status == "rejected"))
  expect_true(all(out$fills$reason_code == "atomic_group_rejected"))
  expect_equal(out$inventory_positions$units, 0)
  expect_equal(out$cash_balances$settled, 100)
  expect_false(out$groups$committed)
})

test_that("heterogeneous GTC limit remains pending then fills", {
  order <- hetero_order_rows(1, "limit", 9, "gtc")
  pending <- hetero_order_fixture(order)
  expect_equal(pending$fills$status, "pending")
  filled <- hetero_order_fixture(order, data.frame(asset_id = 1L, open = 10, high = 10, low = 9, close = 10))
  expect_equal(filled$fills$status, "filled")
})

test_that("heterogeneous IOC limit is terminally cancelled", {
  out <- hetero_order_fixture(hetero_order_rows(1, "limit", 9, "ioc"))
  expect_equal(out$fills$status, "cancelled")
  expect_equal(out$fills$reason_code, "limit_not_eligible")
})

test_that("heterogeneous FOK limit rolls back its group", {
  orders <- rbind(hetero_order_rows(2, group = "G1"), hetero_order_rows(1, "limit", 9, "fok", "G1"))
  orders$order_id <- c("O1", "O2")
  out <- hetero_order_fixture(orders)
  expect_true(all(out$fills$status == "rejected"))
  expect_equal(out$inventory_positions$units, 0)
  expect_equal(out$cash_balances$settled, 100)
})

test_that("heterogeneous fills and groups have a stable group contract", {
  out <- hetero_order_fixture(hetero_order_rows(c(1, 2)))
  expect_true(all(out$fills$atomic_group_id %in% out$groups$atomic_group_id))
  expect_equal(length(unique(out$groups$atomic_group_id)), nrow(out$groups))
  expect_true(all(c("equity", "maintenance_margin", "liquidated") %in% names(out$groups)))
  expect_equal(out$groups$committed, all(out$fills$committed == 1L))
})
