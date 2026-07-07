#' Create a simulation state object
#'
#' @param cash Account cash.
#' @param pos_dir Position direction: `-1`, `0`, or `1`.
#' @param ctr_unit Contract units held.
#' @param avg_price Average entry price.
#' @param last_px Last mark price.
#' @param strat,asset Integer identifiers.
#' @param action_id_now Next action id.
#' @param old_timestamp Previous bar timestamp, used for funding accrual.
#' @param liquidated Whether the account is liquidated.
#' @return A list suitable for `sim_step()`.
#' @export
sim_state <- function(cash = 10000,
                      pos_dir = 0L,
                      ctr_unit = 0,
                      avg_price = NA_real_,
                      last_px = 0,
                      strat = 0L,
                      asset = 0L,
                      action_id_now = 1L,
                      old_timestamp = NA_real_,
                      liquidated = FALSE) {
  list(
    strat = as.integer(strat),
    asset = as.integer(asset),
    cash = as.numeric(cash),
    pos_dir = as.integer(pos_dir),
    ctr_unit = as.numeric(ctr_unit),
    avg_price = as.numeric(avg_price),
    last_px = as.numeric(last_px),
    action_id_now = as.integer(action_id_now),
    old_timestamp = as.numeric(old_timestamp),
    liquidated = isTRUE(liquidated)
  )
}

#' Step the C++ exchange kernel once
#'
#' Processes one bar and a batch of explicit order actions against prior account
#' state. This is the incremental primitive underneath future live exchange
#' workflows; it does not replay earlier bars.
#'
#' @param state Prior state created by `sim_state()` or returned by `sim_step()`.
#' @param bar One-row market bar coercible by `as_market_bars()`.
#' @param orders Data frame with order columns: `action`, `dir`, `order_type`,
#'   `ctr_qty`, `price`, and optional `strat_id`, `action_id`.
#' @param asset Integer asset identifier.
#' @inheritParams sim_backtest
#' @return A list with `state` and `events`.
#' @export
sim_step <- function(state,
                     bar,
                     orders = data.frame(),
                     asset = state$asset %||% 0L,
                     ctr_size = 1,
                     ctr_step = 1,
                     lev = 10,
                     fee_rt = 0,
                     maker_fee_rt = NA_real_,
                     taker_fee_rt = NA_real_,
                     fund_rt = 0,
                     funding_interval_hours = 8,
                     mmr = 0.02,
                     slippage = 0,
                     spread = 0,
                     record = TRUE) {
  bars <- as_market_bars(bar)
  if (nrow(bars) != 1L) stop("`bar` must contain exactly one row.", call. = FALSE)
  order_batch <- .normalize_step_orders(orders, state)
  out <- step_rcpp(
    state = state,
    timestamp = as.numeric(bars$timestamp[1L]),
    open = as.numeric(bars$open[1L]),
    high = as.numeric(bars$high[1L]),
    low = as.numeric(bars$low[1L]),
    close = as.numeric(bars$close[1L]),
    action = order_batch$action,
    dir = order_batch$dir,
    order_type = order_batch$order_type,
    ctr_qty = order_batch$ctr_qty,
    price = order_batch$price,
    strat_id = order_batch$strat_id,
    action_id = order_batch$action_id,
    asset = as.integer(asset),
    ctr_size = as.numeric(ctr_size),
    ctr_step = as.numeric(ctr_step),
    lev = as.numeric(lev),
    fee_rt = as.numeric(fee_rt),
    maker_fee_rt = as.numeric(maker_fee_rt),
    taker_fee_rt = as.numeric(taker_fee_rt),
    fund_rt = as.numeric(fund_rt),
    funding_interval_hours = as.numeric(funding_interval_hours),
    mmr = as.numeric(mmr),
    old_timestamp = as.numeric(state$old_timestamp %||% NA_real_),
    slippage = as.numeric(slippage),
    spread = as.numeric(spread),
    rec = isTRUE(record)
  )
  events <- sim_events(out$events)
  if (inherits(bars$timestamp, "POSIXt") && nrow(events) > 0L) {
    data.table::set(
      events,
      j = "timestamp",
      value = as.POSIXct(events$timestamp, origin = "1970-01-01", tz = attr(bars$timestamp, "tzone") %||% "UTC")
    )
  }
  list(state = out$state, events = events)
}

#' @keywords internal
.normalize_step_orders <- function(orders, state) {
  DT <- data.table::as.data.table(orders)
  if (nrow(DT) == 0L) {
    return(data.table::data.table(
      action = integer(),
      dir = integer(),
      order_type = integer(),
      ctr_qty = numeric(),
      price = numeric(),
      strat_id = integer(),
      action_id = integer()
    ))
  }
  required <- c("action", "dir", "ctr_qty")
  missing <- setdiff(required, names(DT))
  if (length(missing) > 0L) {
    stop("Missing step order column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!"order_type" %in% names(DT)) data.table::set(DT, j = "order_type", value = rep.int("market", nrow(DT)))
  if (!"price" %in% names(DT)) data.table::set(DT, j = "price", value = rep.int(NA_real_, nrow(DT)))
  if (!"strat_id" %in% names(DT)) data.table::set(DT, j = "strat_id", value = rep.int(as.integer(state$strat %||% 0L), nrow(DT)))
  if (!"action_id" %in% names(DT)) {
    start <- as.integer(state$action_id_now %||% 1L)
    data.table::set(DT, j = "action_id", value = seq.int(start, length.out = nrow(DT)))
  }
  data.table::data.table(
    action = .encode_step_action(DT$action),
    dir = .encode_step_dir(DT$dir),
    order_type = .encode_step_order_type(DT$order_type),
    ctr_qty = as.numeric(DT$ctr_qty),
    price = as.numeric(DT$price),
    strat_id = as.integer(DT$strat_id),
    action_id = as.integer(DT$action_id)
  )
}

#' @keywords internal
.encode_step_action <- function(x) {
  if (is.numeric(x)) return(as.integer(x))
  map <- c(reduce = -2L, close = -1L, none = 0L, open = 1L, increase = 2L)
  out <- unname(map[tolower(as.character(x))])
  if (anyNA(out)) stop("Unsupported step action label.", call. = FALSE)
  as.integer(out)
}

#' @keywords internal
.encode_step_dir <- function(x) {
  if (is.numeric(x)) return(as.integer(x))
  map <- c(short = -1L, flat = 0L, long = 1L)
  out <- unname(map[tolower(as.character(x))])
  if (anyNA(out)) stop("Unsupported step direction label.", call. = FALSE)
  as.integer(out)
}

#' @keywords internal
.encode_step_order_type <- function(x) {
  if (is.numeric(x)) return(as.integer(x))
  map <- c(market = 0L, limit = 1L)
  out <- unname(map[tolower(as.character(x))])
  if (anyNA(out)) stop("Unsupported step order type label.", call. = FALSE)
  as.integer(out)
}
