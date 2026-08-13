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

#' Step the C++ portfolio-margin kernel once
#'
#' Processes one timestamp batch of bars and explicit orders for one agent under
#' one shared cash balance. This is the multi-asset primitive used by live
#' exchanges when `portfolio_margin = TRUE`.
#'
#' @param states Named list of prior `sim_state()` objects keyed by `asset_id`.
#' @param bars One timestamp batch of market bars.
#' @param orders Order data frame with `asset_id`, `action`, `dir`,
#'   `order_type`, `ctr_qty`, `price`, `strat_id`, and `action_id`.
#' @param cov Return covariance matrix aligned to `bars$asset_id`.
#' @param shared_cash Shared account cash before this step.
#' @param portfolio_margin_sigma Sigma multiplier for covariance margin.
#' @param portfolio_margin_floor Floor margin rate applied to gross exposure.
#' @inheritParams sim_step
#' @return A list with `states`, `cash`, `equity`, `maintenance_margin`,
#'   `liquidated`, and `events`.
#' @export
sim_portfolio_step <- function(states,
                               bars,
                               orders = data.frame(),
                               cov = diag(nrow(as_market_bars(bars))),
                               shared_cash = 10000,
                               ctr_size = 1,
                               ctr_step = 1,
                               lev = 10,
                               fee_rt = 0,
                               maker_fee_rt = NA_real_,
                               taker_fee_rt = NA_real_,
                               fund_rt = 0,
                               funding_interval_hours = 8,
                               mmr = 0.02,
                               portfolio_margin_sigma = 3,
                               portfolio_margin_floor = mmr,
                               slippage = 0,
                               spread = 0,
                               record = TRUE) {
  bars <- as_market_bars(bars)
  if (nrow(bars) == 0L) stop("`bars` must contain at least one row.", call. = FALSE)
  if (length(unique(bars$timestamp)) != 1L) {
    stop("`bars` must contain one timestamp batch.", call. = FALSE)
  }
  if (!"asset_id" %in% names(bars)) stop("`bars` must contain `asset_id`.", call. = FALSE)
  order_batch <- .normalize_portfolio_step_orders(orders)
  cov <- as.matrix(cov)
  if (!all(dim(cov) == c(nrow(bars), nrow(bars)))) {
    stop("`cov` must be aligned to `bars` and have dimension nrow(bars) x nrow(bars).", call. = FALSE)
  }
  out <- portfolio_step_rcpp(
    states = states,
    bars = data.frame(
      asset_id = as.integer(bars$asset_id),
      timestamp = as.numeric(bars$timestamp),
      open = as.numeric(bars$open),
      high = as.numeric(bars$high),
      low = as.numeric(bars$low),
      close = as.numeric(bars$close)
    ),
    orders = order_batch,
    cov = cov,
    shared_cash = as.numeric(shared_cash),
    ctr_size = as.numeric(ctr_size),
    ctr_step = as.numeric(ctr_step),
    lev = as.numeric(lev),
    fee_rt = as.numeric(fee_rt),
    maker_fee_rt = as.numeric(maker_fee_rt),
    taker_fee_rt = as.numeric(taker_fee_rt),
    fund_rt = as.numeric(fund_rt),
    funding_interval_hours = as.numeric(funding_interval_hours),
    mmr = as.numeric(mmr),
    portfolio_margin_sigma = as.numeric(portfolio_margin_sigma),
    portfolio_margin_floor = as.numeric(portfolio_margin_floor),
    old_timestamp = as.numeric(if (length(states)) states[[1L]]$old_timestamp %||% NA_real_ else NA_real_),
    slippage = as.numeric(slippage),
    spread = as.numeric(spread),
    # Rcpp 1.0.x cannot safely serialize the portfolio recorder. The R layer
    # derives the same order lifecycle table from authoritative kernel states.
    rec = FALSE
  )
  out$events <- if (isTRUE(record)) {
    .portfolio_step_events(states, out, bars, order_batch, ctr_size, fee_rt, maker_fee_rt, taker_fee_rt)
  } else {
    data.table::data.table()
  }
  out
}

#' @keywords internal
.portfolio_step_events <- function(states,
                                   result,
                                   bars,
                                   orders,
                                   ctr_size,
                                   fee_rt,
                                   maker_fee_rt,
                                   taker_fee_rt) {
  if (nrow(orders) == 0L) return(data.table::data.table())
  rows <- lapply(seq_len(nrow(orders)), function(i) {
    order <- orders[i, , drop = FALSE]
    asset_key <- as.character(order$asset_id)
    requested_asset_id <- as.integer(order$asset_id)
    before <- states[[asset_key]] %||% sim_state(asset = order$asset_id)
    after <- result$states[[asset_key]] %||% before
    before_qty <- as.numeric(before$pos_dir %||% 0) * as.numeric(before$ctr_unit %||% 0)
    after_qty <- as.numeric(after$pos_dir %||% 0) * as.numeric(after$ctr_unit %||% 0)
    action <- as.integer(order$action)
    filled <- if (action %in% c(1L, 2L)) abs(after_qty) > abs(before_qty) else abs(after_qty) < abs(before_qty)
    bar <- bars[asset_id == requested_asset_id][1L]
    fill_price <- if (as.integer(order$order_type) == 1L && is.finite(order$price)) as.numeric(order$price) else as.numeric(bar$open)
    fee_rate <- if (as.integer(order$order_type) == 1L) maker_fee_rt %||% fee_rt else taker_fee_rt %||% fee_rt
    if (!is.finite(fee_rate)) fee_rate <- fee_rt
    data.table::data.table(
      timestamp = bar$timestamp,
      event_id = as.integer(i),
      event_type = 1L,
      event_type_label = "trade",
      bar_stage = if (as.integer(order$order_type) == 1L) 2L else 1L,
      bar_stage_label = if (as.integer(order$order_type) == 1L) "intra" else "open",
      action_id = as.integer(order$action_id),
      strat_id = as.integer(order$strat_id),
      asset_id = as.integer(order$asset_id),
      tx_id = as.integer(i),
      status = if (filled) 1L else -1L,
      status_label = if (filled) "filled" else "failed",
      liquidation = isTRUE(result$liquidated),
      action = action,
      action_label = c(`1` = "open", `2` = "increase", `-1` = "close", `-2` = "reduce")[[as.character(action)]] %||% "none",
      dir = as.integer(order$dir),
      dir_label = c(`1` = "long", `-1` = "short", `0` = "flat")[[as.character(order$dir)]] %||% "flat",
      ctr_qty = as.numeric(order$ctr_qty),
      price = fill_price,
      equity = as.numeric(result$equity),
      cash = as.numeric(result$cash),
      fee = if (filled) abs(as.numeric(order$ctr_qty) * fill_price * ctr_size) * fee_rate else 0,
      realized_pnl = NA_real_,
      funding_fee = 0,
      maintenance_margin = as.numeric(result$maintenance_margin)
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
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
.normalize_portfolio_step_orders <- function(orders) {
  DT <- data.table::as.data.table(orders)
  if (nrow(DT) == 0L) {
    return(data.frame(
      order_id = character(),
      asset_id = integer(),
      action = integer(),
      dir = integer(),
      order_type = integer(),
      ctr_qty = numeric(),
      price = numeric(),
      strat_id = integer(),
      action_id = integer()
    ))
  }
  required <- c("asset_id", "action", "dir", "ctr_qty")
  missing <- setdiff(required, names(DT))
  if (length(missing) > 0L) {
    stop("Missing portfolio step order column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!"order_id" %in% names(DT)) data.table::set(DT, j = "order_id", value = rep.int(NA_character_, nrow(DT)))
  if (!"order_type" %in% names(DT)) data.table::set(DT, j = "order_type", value = rep.int("market", nrow(DT)))
  if (!"price" %in% names(DT)) data.table::set(DT, j = "price", value = rep.int(NA_real_, nrow(DT)))
  if (!"strat_id" %in% names(DT)) data.table::set(DT, j = "strat_id", value = rep.int(0L, nrow(DT)))
  if (!"action_id" %in% names(DT)) data.table::set(DT, j = "action_id", value = seq_len(nrow(DT)))
  data.frame(
    order_id = as.character(DT$order_id),
    asset_id = as.integer(DT$asset_id),
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
