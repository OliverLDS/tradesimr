#' Run a stateful trading simulation backtest
#'
#' `sim_backtest()` executes target-position intentions through the package's
#' C++ exchange/accounting engine. Orders are planned at bar close and market
#' orders are filled on the next bar open.
#'
#' @param data A data frame/data.table with timestamp, open, high, low, close,
#'   and target-position columns.
#' @param timestamp_col,open_col,high_col,low_col,close_col,tgt_pos_col Column
#'   names in `data`.
#' @param pos_strat_col Optional strategy-id column. If absent, `strat` is used.
#' @param tol_pos_col Optional target-position tolerance column. If absent,
#'   `tol_pos` is used.
#' @param order_type_col Optional order type column using `market` or `limit`.
#' @param limit_price_col Optional limit price column.
#' @param strat,asset Integer identifiers for the simulation and asset.
#' @param init_cash Initial account cash.
#' @param ctr_size Contract size.
#' @param ctr_step Minimum contract increment.
#' @param lev Leverage used for initial margin.
#' @param fee_rt Trading fee rate on notional.
#' @param maker_fee_rt,taker_fee_rt Optional maker/taker fee rates. Missing
#'   values fall back to `fee_rt`.
#' @param fund_rt Funding rate per 8 hours on notional.
#' @param funding_interval_hours Funding interval in hours.
#' @param mmr Maintenance margin rate.
#' @param fill_model Fill timing model: `next_open` or `same_close`.
#' @param slippage Absolute slippage added against trade direction.
#' @param spread Absolute bid/ask spread; half spread is added against trade
#'   direction.
#' @param tol_pos Scalar default target-position tolerance used when
#'   `tol_pos_col` is absent.
#' @param record Whether to attach the execution recorder.
#'
#' @return A data.table with timestamp and equity. If `record = TRUE`, an
#'   execution recorder is attached as attribute `orders`.
#' @export
sim_backtest <- function(data,
                         timestamp_col = "timestamp",
                         open_col = "open",
                         high_col = "high",
                         low_col = "low",
                         close_col = "close",
                         tgt_pos_col = "tgt_pos",
                         pos_strat_col = NULL,
                         tol_pos_col = NULL,
                         order_type_col = NULL,
                         limit_price_col = NULL,
                         strat = 0L,
                         asset = 0L,
                         init_cash = 10000,
                         ctr_size = 1,
                         ctr_step = 1,
                         lev = 10,
                         fee_rt = 0,
                         maker_fee_rt = NA_real_,
                         taker_fee_rt = NA_real_,
                         fund_rt = 0,
                         funding_interval_hours = 8,
                         mmr = 0.02,
                         fill_model = c("next_open", "same_close"),
                         slippage = 0,
                         spread = 0,
                         tol_pos = 0,
                         record = TRUE) {
  fill_model <- match.arg(fill_model)
  DT <- data.table::as.data.table(data)
  required <- c(timestamp_col, open_col, high_col, low_col, close_col, tgt_pos_col)
  missing <- setdiff(required, names(DT))
  if (length(missing) > 0L) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  n <- nrow(DT)
  timestamp_raw <- DT[[timestamp_col]]
  timestamp <- as.numeric(timestamp_raw)
  if (anyNA(timestamp)) stop("Timestamp column cannot contain NA values.", call. = FALSE)

  pos_strat <- if (is.null(pos_strat_col)) {
    rep.int(as.integer(strat), n)
  } else {
    as.integer(DT[[pos_strat_col]])
  }

  tol_pos_vec <- if (is.null(tol_pos_col)) {
    rep.int(as.numeric(tol_pos), n)
  } else {
    as.numeric(DT[[tol_pos_col]])
  }

  order_type <- if (is.null(order_type_col)) {
    rep.int(0L, n)
  } else {
    raw_type <- tolower(as.character(DT[[order_type_col]]))
    if (any(!raw_type %in% c("market", "limit"))) {
      stop("Order type column must contain only `market` or `limit`.", call. = FALSE)
    }
    as.integer(raw_type == "limit")
  }

  limit_price <- if (is.null(limit_price_col)) {
    rep.int(NA_real_, n)
  } else {
    as.numeric(DT[[limit_price_col]])
  }

  engine <- backtest_rcpp(
    timestamp = timestamp,
    open = as.numeric(DT[[open_col]]),
    high = as.numeric(DT[[high_col]]),
    low = as.numeric(DT[[low_col]]),
    close = as.numeric(DT[[close_col]]),
    tgt_pos = as.numeric(DT[[tgt_pos_col]]),
    pos_strat = pos_strat,
    tol_pos = tol_pos_vec,
    order_type = order_type,
    limit_price = limit_price,
    strat = as.integer(strat),
    asset = as.integer(asset),
    init_cash = as.numeric(init_cash),
    ctr_size = as.numeric(ctr_size),
    ctr_step = as.numeric(ctr_step),
    lev = as.numeric(lev),
    fee_rt = as.numeric(fee_rt),
    maker_fee_rt = as.numeric(maker_fee_rt),
    taker_fee_rt = as.numeric(taker_fee_rt),
    fund_rt = as.numeric(fund_rt),
    funding_interval_hours = as.numeric(funding_interval_hours),
    mmr = as.numeric(mmr),
    fill_model = as.integer(match(fill_model, c("next_open", "same_close")) - 1L),
    slippage = as.numeric(slippage),
    spread = as.numeric(spread),
    rec = isTRUE(record)
  )

  out <- data.table::data.table(
    timestamp = timestamp_raw,
    open = as.numeric(DT[[open_col]]),
    high = as.numeric(DT[[high_col]]),
    low = as.numeric(DT[[low_col]]),
    close = as.numeric(DT[[close_col]]),
    equity = as.numeric(engine$equity),
    cash = as.numeric(engine$cash),
    pos_dir = as.integer(engine$pos_dir),
    ctr_unit = as.numeric(engine$ctr_unit),
    avg_price = as.numeric(engine$avg_price),
    last_px = as.numeric(engine$last_px),
    notional = as.numeric(engine$notional),
    abs_notional = as.numeric(engine$abs_notional),
    unrealized_pnl = as.numeric(engine$unrealized_pnl),
    maintenance_margin = as.numeric(engine$maintenance_margin)
  )
  data.table::setattr(out, "sim_config", list(
    strat = strat,
    asset = asset,
    init_cash = init_cash,
    ctr_size = ctr_size,
    ctr_step = ctr_step,
    lev = lev,
    fee_rt = fee_rt,
    maker_fee_rt = maker_fee_rt,
    taker_fee_rt = taker_fee_rt,
    fund_rt = fund_rt,
    funding_interval_hours = funding_interval_hours,
    mmr = mmr,
    fill_model = fill_model,
    slippage = slippage,
    spread = spread,
    tol_pos = tol_pos
  ))

  recorder <- engine$recorder
  if (!is.null(recorder)) {
    events <- sim_events(recorder)
    if (inherits(timestamp_raw, "POSIXt") && nrow(events) > 0L) {
      data.table::set(
        events,
        j = "timestamp",
        value = as.POSIXct(events$timestamp, origin = "1970-01-01", tz = attr(timestamp_raw, "tzone") %||% "UTC")
      )
    }
    data.table::setattr(out, "events", events)
    data.table::setattr(out, "orders", sim_orders(events))
  }

  out
}

#' Replay historical bars through the simulation engine
#'
#' Alias for `sim_backtest()` reserved for replay-oriented workflows.
#'
#' @inheritParams sim_backtest
#' @param ... Additional arguments passed to `sim_backtest()`.
#' @export
sim_replay <- function(data, ...) {
  sim_backtest(data, ...)
}

#' Convert a simulation recorder into an event table
#'
#' @param x A simulation result returned by `sim_backtest()` or a raw recorder
#'   list from the C++ engine.
#' @return A data.table of recorded simulation events.
#' @export
sim_events <- function(x) {
  if (is.data.frame(x) && all(c("event_id", "event_type") %in% names(x))) {
    recorder <- data.table::as.data.table(x)
  } else {
    recorder <- if (is.data.frame(x)) attr(x, "events", exact = TRUE) else x
  }
  if (is.null(recorder)) return(data.table::data.table())
  if (data.table::is.data.table(recorder)) return(data.table::copy(recorder))

  out <- data.table::as.data.table(recorder)
  if (nrow(out) == 0L) return(out)

  data.table::set(
    out,
    j = "event_type_label",
    value = data.table::fifelse(out$event_type == 1L, "trade",
      data.table::fifelse(out$event_type == 2L, "funding",
        data.table::fifelse(out$event_type == 3L, "liquidation", NA_character_)
      )
    )
  )
  data.table::set(
    out,
    j = "bar_stage_label",
    value = data.table::fifelse(out$bar_stage == 1L, "open",
      data.table::fifelse(out$bar_stage == 2L, "intra",
        data.table::fifelse(out$bar_stage == 3L, "close", NA_character_)
      )
    )
  )
  data.table::set(
    out,
    j = "status_label",
    value = data.table::fifelse(out$status == 1L, "filled",
      data.table::fifelse(out$status == 0L, "pending",
        data.table::fifelse(out$status == -1L, "failed", NA_character_)
      )
    )
  )
  data.table::set(
    out,
    j = "action_label",
    value = data.table::fifelse(out$action == 1L, "open",
      data.table::fifelse(out$action == 2L, "increase",
        data.table::fifelse(out$action == -1L, "close",
          data.table::fifelse(out$action == -2L, "reduce",
            data.table::fifelse(out$action == 0L, "none", NA_character_)
          )
        )
      )
    )
  )
  data.table::set(
    out,
    j = "dir_label",
    value = data.table::fifelse(out$dir == 1L, "long",
      data.table::fifelse(out$dir == -1L, "short",
        data.table::fifelse(out$dir == 0L, "flat", NA_character_)
      )
    )
  )
  data.table::set(
    out,
    j = "state_dir_label",
    value = data.table::fifelse(out$state_dir == 1L, "long",
      data.table::fifelse(out$state_dir == -1L, "short",
        data.table::fifelse(out$state_dir == 0L, "flat", NA_character_)
      )
    )
  )
  out[]
}

#' Convert a simulation recorder into an order/event table
#'
#' @param x A simulation result returned by `sim_backtest()` or a raw recorder
#'   list from the C++ engine.
#' @return A data.table of recorded execution events.
#' @export
sim_orders <- function(x) {
  out <- sim_events(x)
  if (nrow(out) == 0L) return(out)
  out[out$event_type_label == "trade"]
}

#' Calculate core performance metrics from a simulation result
#'
#' @param sim A simulation result from `sim_backtest()`.
#' @return A one-row data.table with return, drawdown, and event counts.
#' @export
sim_metrics <- function(sim) {
  DT <- data.table::as.data.table(sim)
  if (!all(c("timestamp", "equity") %in% names(DT))) {
    stop("`sim` must contain `timestamp` and `equity` columns.", call. = FALSE)
  }
  if (nrow(DT) == 0L) {
    return(data.table::data.table(
      start_equity = numeric(),
      end_equity = numeric(),
      total_return = numeric(),
      annualized_return = numeric(),
      max_drawdown = numeric(),
      n_events = integer(),
      n_fills = integer(),
      liquidated = logical()
    ))
  }

  start_equity <- DT$equity[1L]
  end_equity <- DT$equity[nrow(DT)]
  total_return <- end_equity / start_equity - 1
  peak <- cummax(DT$equity)
  max_drawdown <- max(1 - DT$equity / peak, na.rm = TRUE)

  timestamp <- DT$timestamp
  years <- NA_real_
  if (inherits(timestamp, "POSIXt") || inherits(timestamp, "Date")) {
    years <- as.numeric(difftime(max(timestamp), min(timestamp), units = "days")) / 365.25
  } else {
    span_seconds <- max(as.numeric(timestamp), na.rm = TRUE) - min(as.numeric(timestamp), na.rm = TRUE)
    years <- span_seconds / (365.25 * 24 * 60 * 60)
  }
  annualized_return <- if (is.finite(years) && years > 0 && start_equity > 0 && end_equity >= 0) {
    (end_equity / start_equity)^(1 / years) - 1
  } else {
    NA_real_
  }

  events <- sim_events(sim)
  fills <- sim_fills(sim)
  data.table::data.table(
    start_equity = start_equity,
    end_equity = end_equity,
    total_return = total_return,
    annualized_return = annualized_return,
    max_drawdown = max_drawdown,
    n_events = nrow(events),
    n_fills = nrow(fills),
    liquidated = any(DT$equity <= 0, na.rm = TRUE) ||
      (nrow(events) > 0L && any(events$liquidation, na.rm = TRUE))
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}
