#' Create an in-memory simulated exchange state
#'
#' @param config Named list of simulation parameters passed to `sim_backtest()`.
#' @return A mutable environment containing market, intent, order, and result
#'   tables.
#' @export
sim_exchange_new <- function(config = list()) {
  state <- new.env(parent = emptyenv())
  state$config <- config
  state$market_events <- sim_schemas()$market_events[0]
  state$intents <- sim_schemas()$intents[0]
  state$agent_orders <- data.table::data.table(
    order_id = character(),
    agent_id = character(),
    timestamp = as.POSIXct(character()),
    tgt_pos = numeric(),
    tol_pos = numeric(),
    status = character()
  )
  state$result <- NULL
  state$next_order_id <- 1L
  class(state) <- c("tradesimr_exchange", "environment")
  state
}

#' Append market bars to a simulated exchange
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars Market bars coercible by `as_market_bars()`.
#' @return The exchange, invisibly.
#' @export
sim_exchange_add_bars <- function(exchange, bars) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  new_bars <- as_market_bars(bars)
  exchange$market_events <- data.table::rbindlist(list(exchange$market_events, new_bars), fill = TRUE)
  invisible(exchange)
}

#' Place a target-position order into a simulated exchange
#'
#' This is an intent-level order API for the current replay/live architecture.
#' It records a desired target exposure at a timestamp and delegates accounting
#' to the same backtest engine used by `sim_backtest()`.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Agent identifier.
#' @param timestamp Order timestamp.
#' @param tgt_pos Target exposure.
#' @param tol_pos Target-position tolerance.
#' @return The generated order id.
#' @export
sim_exchange_place_order <- function(exchange, agent_id, timestamp, tgt_pos, tol_pos = 0) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  order_id <- paste0("ORD", sprintf("%06d", exchange$next_order_id))
  exchange$next_order_id <- exchange$next_order_id + 1L
  row <- data.table::data.table(
    order_id = order_id,
    agent_id = as.character(agent_id),
    timestamp = timestamp,
    tgt_pos = as.numeric(tgt_pos),
    tol_pos = as.numeric(tol_pos),
    status = "accepted"
  )
  exchange$agent_orders <- data.table::rbindlist(list(exchange$agent_orders, row), fill = TRUE)
  invisible(order_id)
}

#' Cancel an intent-level order in a simulated exchange
#'
#' @param exchange A `tradesimr_exchange`.
#' @param order_id Order id returned by `sim_exchange_place_order()`.
#' @return Invisibly returns `TRUE` if an order was cancelled.
#' @export
sim_exchange_cancel_order <- function(exchange, order_id) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  idx <- which(exchange$agent_orders$order_id == order_id & exchange$agent_orders$status == "accepted")
  if (length(idx) == 0L) return(invisible(FALSE))
  data.table::set(exchange$agent_orders, i = idx, j = "status", value = "cancelled")
  invisible(TRUE)
}

#' Run or refresh a simulated exchange replay
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A simulation result returned by `sim_backtest()`.
#' @export
sim_exchange_run <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  bars <- data.table::copy(exchange$market_events)
  if (nrow(bars) == 0L) stop("No market bars have been added.", call. = FALSE)

  active_orders <- exchange$agent_orders[exchange$agent_orders$status == "accepted"]
  if (nrow(active_orders) == 0L) {
    data.table::set(bars, j = "tgt_pos", value = rep.int(0, nrow(bars)))
    data.table::set(bars, j = "tol_pos", value = rep.int(0, nrow(bars)))
  } else {
    order_intents <- active_orders[, .SD, .SDcols = c("timestamp", "tgt_pos", "tol_pos")]
    data.table::setorderv(order_intents, "timestamp")
    data.table::set(bars, j = "tgt_pos", value = rep.int(0, nrow(bars)))
    data.table::set(bars, j = "tol_pos", value = rep.int(0, nrow(bars)))
    for (i in seq_len(nrow(order_intents))) {
      idx <- which(bars$timestamp >= order_intents$timestamp[i])
      if (length(idx) > 0L) {
        data.table::set(bars, i = idx, j = "tgt_pos", value = order_intents$tgt_pos[i])
        data.table::set(bars, i = idx, j = "tol_pos", value = order_intents$tol_pos[i])
      }
    }
  }

  args <- c(list(data = bars, tol_pos_col = "tol_pos"), exchange$config)
  exchange$result <- do.call(sim_backtest, args)
  exchange$result
}

#' Step a simulated exchange with one or more bars
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars Market bars coercible by `as_market_bars()`.
#' @return The refreshed simulation result.
#' @export
sim_exchange_step <- function(exchange, bars) {
  sim_exchange_add_bars(exchange, bars)
  sim_exchange_run(exchange)
}

#' Get simulated exchange orders
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A data.table of accepted/cancelled intent-level orders.
#' @export
sim_exchange_orders <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  data.table::copy(exchange$agent_orders)
}

#' Get simulated exchange account state
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A one-row data.table with the latest account snapshot.
#' @export
sim_exchange_account <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$result)) return(data.table::data.table())
  tail(sim_account(exchange$result), 1L)
}

#' Get simulated exchange positions
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A one-row data.table with the latest position snapshot.
#' @export
sim_exchange_positions <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$result)) return(data.table::data.table())
  tail(sim_positions(exchange$result), 1L)
}
