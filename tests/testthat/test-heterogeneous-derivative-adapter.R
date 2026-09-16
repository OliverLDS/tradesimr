test_that("derivative portfolio adapter persists heterogeneous margin state", {
  make_exchange <- function() {
    exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE, fee_rt = 0.001))
    sim_asset_add(exchange, "BTC-PERP", asset_id = 7L, asset_class = "crypto_perp",
      contract_size = 1, qty_step = 0.001)
    exchange
  }
  bar <- function(timestamp, price) data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"), symbol = "BTC-PERP", asset_id = 7L,
    open = price, high = price * 1.01, low = price * 0.99, close = price
  )
  execution <- sim_portfolio_execution(lev = 1, fee_rt = 0.001)
  exchange <- make_exchange()
  first <- bar("2026-09-01", 100)
  second <- bar("2026-09-02", 101)
  third <- bar("2026-09-03", 102)
  sim_portfolio_market_step(exchange, first, execution)
  sim_portfolio_target_submit(exchange, "agent", first, c(`BTC-PERP` = 0.5), execution,
    allowed_symbols = "BTC-PERP")
  sim_portfolio_market_step(exchange, second, execution)

  expect_equal(nrow(exchange$margin_positions), 1L)
  expect_equal(exchange$margin_positions$agent_id, "agent")
  position <- sim_exchange_positions(exchange)[agent_id == "agent"]
  expect_equal(exchange$margin_positions$signed_units, position$pos_dir * position$ctr_unit)
  expect_true(nrow(exchange$portfolio_fills) == 1L)
  expect_true(all(c("order_id", "atomic_group_id", "status", "qty", "fee") %in% names(
    tradesimr:::.heterogeneous_derivatives_account_step(
      exchange, "agent", tradesimr:::.heterogeneous_derivative_account_input(exchange, "agent", second),
      second, data.frame(), matrix(0.01, 1, 1)
    )$fills
  )))

  path <- tempfile("tradesimr-heterogeneous-derivative-")
  sim_exchange_save(exchange, path)
  resumed <- sim_exchange_load(path)
  margin_state <- function(x) {
    out <- data.table::copy(x)
    data.table::setkey(out, NULL)
    as.data.frame(out)
  }
  durable_table <- function(x) {
    out <- data.table::copy(x)
    for (column in names(out)) if (inherits(out[[column]], "POSIXt")) {
      data.table::set(out, j = column, value = as.numeric(out[[column]]))
    }
    as.data.frame(out)
  }
  expect_equal(margin_state(resumed$margin_positions), margin_state(exchange$margin_positions))

  sim_portfolio_market_step(exchange, third, execution)
  sim_portfolio_market_step(resumed, third, execution)
  expect_equal(durable_table(sim_exchange_orders(resumed)), durable_table(sim_exchange_orders(exchange)))
  expect_equal(durable_table(resumed$portfolio_fills), durable_table(exchange$portfolio_fills))
  expect_equal(sim_exchange_positions(resumed), sim_exchange_positions(exchange))
  expect_equal(sim_exchange_account(resumed), sim_exchange_account(exchange))
})

test_that("complete native derivative boundaries do not need a duplicate R margin pass", {
  exchange <- sim_exchange_new(list(cash = 100000, lev = 1, portfolio_margin = TRUE, fee_rt = 0.001))
  sim_asset_add(exchange, "BTC-PERP", asset_id = 7L, asset_class = "crypto_perp", contract_size = 1, qty_step = 0.001)
  sim_asset_add(exchange, "ETH-PERP", asset_id = 8L, asset_class = "crypto_perp", contract_size = 1, qty_step = 0.001)
  execution <- sim_portfolio_execution(lev = 1, fee_rt = 0.001)
  first <- data.frame(
    timestamp = rep(as.POSIXct("2026-09-01", tz = "UTC"), 2L),
    symbol = c("BTC-PERP", "ETH-PERP"), asset_id = c(7L, 8L),
    open = c(100, 50), high = c(101, 51), low = c(99, 49), close = c(100, 50)
  )
  second <- first
  second$timestamp <- second$timestamp + 86400
  second[, c("open", "high", "low", "close")] <- list(c(101, 49), c(102, 50), c(100, 48), c(101, 49))
  sim_portfolio_market_step(exchange, first, execution)
  sim_portfolio_target_submit(exchange, "agent", first,
    c(`BTC-PERP` = 0.4, `ETH-PERP` = -0.2), execution,
    allowed_symbols = c("BTC-PERP", "ETH-PERP")
  )
  sim_portfolio_market_step(exchange, second, execution)

  expect_true(tradesimr:::.portfolio_native_derivative_account_complete(exchange, "agent", second))
  before <- list(
    cash = tradesimr:::.shared_cash(exchange, "agent"),
    margin = data.table::copy(exchange$margin_positions),
    states = exchange$agent_states,
    liquidated = exchange$agent_accounts[["agent"]]$liquidated
  )
  expect_false(tradesimr:::.enforce_cross_margin(exchange, "agent", second$timestamp[1L]))
  after <- list(
    cash = tradesimr:::.shared_cash(exchange, "agent"),
    margin = data.table::copy(exchange$margin_positions),
    states = exchange$agent_states,
    liquidated = exchange$agent_accounts[["agent"]]$liquidated
  )
  expect_equal(after, before)
})

test_that("default exchange routes derivative-only target replay through typed state", {
  exchange <- sim_exchange_new(list(
    cash = 10000, portfolio_margin = TRUE, lev = 1
  ))
  expect_identical(exchange$config$execution_engine, "heterogeneous_v2")
  sim_asset_add(exchange, "ES", asset_id = 1L, instrument_profile = "future")
  execution <- sim_portfolio_execution(lev = 1)
  first <- data.frame(
    timestamp = as.POSIXct("2026-10-01", tz = "UTC"), symbol = "ES", asset_id = 1L,
    open = 100, high = 101, low = 99, close = 100
  )
  sim_portfolio_market_step(exchange, first, execution)
  sim_portfolio_target_submit(exchange, "agent", first, c(ES = .5), execution,
    allowed_symbols = "ES")
  sim_portfolio_market_step(exchange, transform(first, timestamp = timestamp + 86400), execution)

  expect_equal(exchange$margin_positions$signed_units, 50)
  expect_equal(exchange$typed_margin_positions$signed_units, 50)
  expect_equal(exchange$cash_balances$settled, 10000)
  expect_equal(sim_exchange_orders(exchange)$status, "filled")
  expect_equal(nrow(exchange$portfolio_fills), 1L)
  expect_equal(sim_exchange_positions(exchange)$ctr_unit, 50)

  path <- tempfile("tradesimr-v2-state-")
  sim_exchange_save(exchange, path)
  restored <- sim_exchange_load(path)
  expect_equal(restored$config$execution_engine, "heterogeneous_v2")
  expect_equal(restored$typed_margin_positions, exchange$typed_margin_positions)
  expect_equal(restored$cash_balances, exchange$cash_balances)
})
