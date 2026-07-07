#' Simulation table schemas
#'
#' @return A named list of empty data.tables representing durable simulation
#'   table schemas.
#' @export
sim_schemas <- function() {
  list(
    market_events = data.table::data.table(
      timestamp = as.POSIXct(character()),
      open = numeric(),
      high = numeric(),
      low = numeric(),
      close = numeric()
    ),
    intents = data.table::data.table(
      timestamp = as.POSIXct(character()),
      strat_id = integer(),
      tgt_pos = numeric(),
      tol_pos = numeric()
    ),
    events = data.table::data.table(
      timestamp = as.POSIXct(character()),
      event_id = integer(),
      event_type = integer(),
      event_type_label = character(),
      action_id = integer(),
      status_label = character(),
      action_label = character(),
      dir_label = character(),
      ctr_qty = numeric(),
      price = numeric(),
      cash = numeric(),
      equity = numeric()
    ),
    fills = data.table::data.table(
      timestamp = as.POSIXct(character()),
      event_id = integer(),
      action_id = integer(),
      action_label = character(),
      dir_label = character(),
      ctr_qty = numeric(),
      price = numeric(),
      fee = numeric(),
      realized_pnl = numeric()
    ),
    positions = data.table::data.table(
      timestamp = as.POSIXct(character()),
      pos_dir = integer(),
      pos_label = character(),
      ctr_unit = numeric(),
      avg_price = numeric(),
      last_px = numeric(),
      notional = numeric(),
      unrealized_pnl = numeric()
    ),
    cash_ledger = data.table::data.table(
      timestamp = as.POSIXct(character()),
      event_id = integer(),
      event_type_label = character(),
      action_label = character(),
      cash = numeric(),
      fee = numeric(),
      funding_fee = numeric(),
      realized_pnl = numeric()
    ),
    account_snapshots = data.table::data.table(
      timestamp = as.POSIXct(character()),
      equity = numeric(),
      cash = numeric(),
      notional = numeric(),
      abs_notional = numeric(),
      unrealized_pnl = numeric()
    ),
    risk_snapshots = data.table::data.table(
      timestamp = as.POSIXct(character()),
      equity = numeric(),
      abs_notional = numeric(),
      leverage = numeric(),
      maintenance_margin = numeric(),
      margin_buffer = numeric()
    )
  )
}

#' Validate core market-bar columns
#'
#' @param data A table-like object.
#' @param timestamp_col,open_col,high_col,low_col,close_col Column names.
#' @return Invisibly returns `TRUE` on success.
#' @export
validate_market_data <- function(data,
                                 timestamp_col = "timestamp",
                                 open_col = "open",
                                 high_col = "high",
                                 low_col = "low",
                                 close_col = "close") {
  DT <- data.table::as.data.table(data)
  required <- c(timestamp_col, open_col, high_col, low_col, close_col)
  missing <- setdiff(required, names(DT))
  if (length(missing) > 0L) {
    stop("Missing market data column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  price_cols <- c(open_col, high_col, low_col, close_col)
  if (any(!vapply(price_cols, function(col) is.numeric(DT[[col]]), logical(1)))) {
    stop("OHLC columns must be numeric.", call. = FALSE)
  }
  if (anyNA(DT[[timestamp_col]])) {
    stop("Timestamp column cannot contain NA values.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate target-position intent columns
#'
#' @param data A table-like object.
#' @param tgt_pos_col Target-position column name.
#' @param tol_pos_col Optional tolerance column name.
#' @return Invisibly returns `TRUE` on success.
#' @export
validate_intents <- function(data, tgt_pos_col = "tgt_pos", tol_pos_col = NULL) {
  DT <- data.table::as.data.table(data)
  required <- c(tgt_pos_col, tol_pos_col)
  missing <- setdiff(required[!is.null(required)], names(DT))
  if (length(missing) > 0L) {
    stop("Missing intent column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(DT[[tgt_pos_col]])) {
    stop("Target-position column must be numeric.", call. = FALSE)
  }
  if (!is.null(tol_pos_col) && !is.numeric(DT[[tol_pos_col]])) {
    stop("Tolerance column must be numeric.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Normalize market bars for tradesimr
#'
#' @inheritParams validate_market_data
#' @return A data.table with canonical `timestamp`, `open`, `high`, `low`,
#'   `close` columns.
#' @export
as_market_bars <- function(data,
                           timestamp_col = "timestamp",
                           open_col = "open",
                           high_col = "high",
                           low_col = "low",
                           close_col = "close") {
  validate_market_data(data, timestamp_col, open_col, high_col, low_col, close_col)
  DT <- data.table::as.data.table(data)
  out <- DT[, .SD, .SDcols = c(timestamp_col, open_col, high_col, low_col, close_col)]
  data.table::setnames(out, c("timestamp", "open", "high", "low", "close"))
  out[]
}

#' Normalize target-position intents for tradesimr
#'
#' @param data A table-like object.
#' @param timestamp_col Optional timestamp column name.
#' @param tgt_pos_col Target-position column name.
#' @param pos_strat_col Optional strategy-id column name.
#' @param tol_pos_col Optional tolerance column name.
#' @param strat Default strategy id.
#' @param tol_pos Default target-position tolerance.
#' @return A data.table with canonical intent columns.
#' @export
as_target_positions <- function(data,
                                timestamp_col = NULL,
                                tgt_pos_col = "tgt_pos",
                                pos_strat_col = NULL,
                                tol_pos_col = NULL,
                                strat = 0L,
                                tol_pos = 0) {
  validate_intents(data, tgt_pos_col, tol_pos_col)
  DT <- data.table::as.data.table(data)
  n <- nrow(DT)
  data.table::data.table(
    timestamp = if (!is.null(timestamp_col)) DT[[timestamp_col]] else seq_len(n),
    strat_id = if (!is.null(pos_strat_col)) as.integer(DT[[pos_strat_col]]) else rep.int(as.integer(strat), n),
    tgt_pos = as.numeric(DT[[tgt_pos_col]]),
    tol_pos = if (!is.null(tol_pos_col)) as.numeric(DT[[tol_pos_col]]) else rep.int(as.numeric(tol_pos), n)
  )
}
