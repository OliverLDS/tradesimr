#' tradesimr durable schema version
#'
#' @export
TRADESIMR_SCHEMA_VERSION <- "0.16.0"

#' Heterogeneous account schema version
#'
#' @export
TRADESIMR_ACCOUNT_SCHEMA_VERSION <- "2.0.0"

#' Simulation table schemas
#'
#' @return A named list of empty data.tables representing durable simulation
#'   table schemas.
#' @export
sim_schemas <- function() {
  list(
    cash_balances = data.table::data.table(
      agent_id = character(), currency = character(), settled = numeric(),
      unsettled = numeric(), timestamp = as.POSIXct(character())
    ),
    inventory_positions = data.table::data.table(
      agent_id = character(), asset_id = integer(), symbol = character(),
      currency = character(), units = numeric(), average_cost = numeric(),
      last_price = numeric(), contract_size = numeric(), accrued_interest = numeric(),
      timestamp = as.POSIXct(character())
    ),
    margin_positions = data.table::data.table(
      agent_id = character(), asset_id = integer(), symbol = character(),
      currency = character(), signed_units = numeric(), settlement_price = numeric(),
      last_price = numeric(), contract_size = numeric(), maintenance_rate = numeric(),
      timestamp = as.POSIXct(character())
    ),
    account_events = data.table::data.table(
      account_event_id = character(), timestamp = as.POSIXct(character()),
      agent_id = character(), event_type = character(), asset_id = integer(),
      symbol = character(), currency = character(), amount = numeric(),
      order_id = character(), fill_id = character(), atomic_group_id = character(),
      message = character()
    ),
    assets = data.table::data.table(
      asset_id = integer(),
      symbol = character(),
      status = character(),
      asset_class = character(),
      instrument_profile = character(),
      contract_size = numeric(),
      tick_size = numeric(),
      qty_step = numeric(),
      base_ccy = character(),
      quote_ccy = character(),
      calendar_id = character(),
      timezone = character(),
      settlement_lag_days = integer(),
      margin_model = character(),
      accounting_model = character(),
      bar_cadence_seconds = numeric(),
      metadata = character(),
      created_at = as.POSIXct(character())
    ),
    fx_rates = data.table::data.table(
      timestamp = as.POSIXct(character()),
      from_ccy = character(),
      to_ccy = character(),
      rate = numeric(),
      source = character()
    ),
    profile_cash_ledger = data.table::data.table(
      entry_id = character(),
      timestamp = as.POSIXct(character()),
      agent_id = character(),
      currency = character(),
      amount = numeric(),
      balance_after = numeric(),
      event_type = character(),
      asset_id = integer(),
      symbol = character(),
      order_id = character(),
      settlement_id = character(),
      message = character()
    ),
    settlement_ledger = data.table::data.table(
      settlement_id = character(),
      trade_timestamp = as.POSIXct(character()),
      due_timestamp = as.POSIXct(character()),
      settled_timestamp = as.POSIXct(character()),
      agent_id = character(),
      currency = character(),
      amount = numeric(),
      asset_id = integer(),
      symbol = character(),
      order_id = character(),
      status = character(),
      message = character()
    ),
    corporate_actions = data.table::data.table(
      action_id = character(),
      effective_timestamp = as.POSIXct(character()),
      asset_id = integer(),
      symbol = character(),
      action_type = character(),
      amount = numeric(),
      currency = character(),
      status = character(),
      message = character()
    ),
    bond_schedules = data.table::data.table(
      asset_id = integer(), symbol = character(), currency = character(),
      coupon_rate = numeric(), coupon_frequency = numeric(), face_value = numeric(),
      accrual_day_count = numeric(), issue_timestamp = as.POSIXct(character()),
      maturity_timestamp = as.POSIXct(character()), last_accrual_timestamp = as.POSIXct(character()),
      next_coupon_timestamp = as.POSIXct(character()), status = character()
    ),
    market_events = data.table::data.table(
      timestamp = as.POSIXct(character()),
      observation_timestamp = as.POSIXct(character()),
      bar_start = as.POSIXct(character()),
      bar_end = as.POSIXct(character()),
      valuation_timestamp = as.POSIXct(character()),
      market_timezone = character(),
      is_completed = logical(),
      is_tradable = logical(),
      symbol = character(),
      asset_id = integer(),
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
    agent_orders = data.table::data.table(
      order_id = character(),
      client_order_id = character(),
      agent_id = character(),
      symbol = character(),
      asset_id = integer(),
      timestamp = as.POSIXct(character()),
      eligible_after = as.POSIXct(character()),
      settlement_timestamp = as.POSIXct(character()),
      rebalance_id = character(),
      atomic_group_id = character(),
      target_derived = logical(),
      superseded_by_rebalance_id = character(),
      supersedes_rebalance_id = character(),
      target_weight = numeric(),
      decision_price = numeric(),
      order_type = character(),
      side = character(),
      intended_action = character(),
      intended_dir = character(),
      qty_type = character(),
      qty = numeric(),
      limit_price = numeric(),
      time_in_force = character(),
      tgt_pos = numeric(),
      tol_pos = numeric(),
      status = character(),
      price = numeric(),
      fee = numeric(),
      realized_pnl = numeric(),
      reason_code = character(),
      message = character()
    ),
    portfolio_targets = data.table::data.table(
      rebalance_id = character(),
      superseded_by_rebalance_id = character(),
      supersedes_rebalance_id = character(),
      timestamp = as.POSIXct(character()),
      eligible_after = as.POSIXct(character()),
      agent_id = character(),
      symbol = character(),
      asset_id = integer(),
      target_weight = numeric(),
      realized_weight_before = numeric(),
      decision_equity = numeric(),
      planned_signed_quantity = numeric(),
      decision_price = numeric(),
      status = character(),
      message = character()
    ),
    portfolio_rebalances = data.table::data.table(
      rebalance_id = character(),
      superseded_by_rebalance_id = character(),
      supersedes_rebalance_id = character(),
      timestamp = as.POSIXct(character()),
      agent_id = character(),
      status = character(),
      execution_timing = character(),
      fee_rt = numeric(),
      slippage = numeric(),
      spread = numeric(),
      message = character()
    ),
    portfolio_fills = data.table::data.table(
      fill_id = character(),
      timestamp = as.POSIXct(character()),
      agent_id = character(),
      order_id = character(),
      rebalance_id = character(),
      symbol = character(),
      asset_id = integer(),
      event_id = integer(),
      action_id = integer(),
      side = character(),
      action = character(),
      status = character(),
      action_label = character(),
      status_label = character(),
      dir_label = character(),
      qty = numeric(),
      ctr_qty = numeric(),
      price = numeric(),
      fee = numeric(),
      realized_pnl = numeric(),
      reason_code = character(),
      target_weight = numeric()
    ),
    portfolio_market_boundaries = data.table::data.table(
      timestamp = as.POSIXct(character()),
      symbol = character(),
      asset_id = integer()
    ),
    agent_commands = data.table::data.table(
      command_id = character(),
      timestamp = as.POSIXct(character()),
      agent_id = character(),
      command_type = character(),
      status = character(),
      ref_id = character(),
      message = character()
    ),
    order_requests = data.table::data.table(
      command_id = character(),
      client_order_id = character(),
      agent_id = character(),
      symbol = character(),
      asset_id = integer(),
      timestamp = as.POSIXct(character()),
      order_type = character(),
      side = character(),
      qty_type = character(),
      qty = numeric(),
      limit_price = numeric(),
      time_in_force = character(),
      tgt_pos = numeric(),
      tol_pos = numeric(),
      status = character(),
      order_id = character(),
      message = character()
    ),
    order_cancellations = data.table::data.table(
      command_id = character(),
      agent_id = character(),
      timestamp = as.POSIXct(character()),
      order_id = character(),
      client_order_id = character(),
      status = character(),
      message = character()
    ),
    agents = data.table::data.table(
      agent_id = character(),
      agent_type = character(),
      status = character(),
      config = character(),
      created_at = as.POSIXct(character())
    ),
    agent_decisions = data.table::data.table(
      timestamp = as.POSIXct(character()),
      agent_id = character(),
      agent_type = character(),
      symbol = character(),
      asset_id = integer(),
      decision_type = character(),
      side = character(),
      intended_action = character(),
      intended_dir = character(),
      qty_type = character(),
      qty = numeric(),
      order_type = character(),
      limit_price = numeric(),
      tgt_pos = numeric(),
      reason = character(),
      command_id = character(),
      status = character()
    ),
    agent_strategy_events = data.table::data.table(
      timestamp = as.POSIXct(character()),
      agent_id = character(),
      strategy_id = character(),
      strategy_fun = character(),
      stage = character(),
      output_type = character(),
      symbol = character(),
      asset_id = integer(),
      signal = character(),
      target = numeric(),
      status = character(),
      message = character(),
      raw = character()
    ),
    agent_rankings = data.table::data.table(
      timestamp = as.POSIXct(character()),
      agent_id = character(),
      agent_type = character(),
      status = character(),
      equity = numeric(),
      cash = numeric(),
      unrealized_pnl = numeric(),
      orders = integer(),
      filled_orders = integer(),
      net_qty = numeric(),
      last_side = character(),
      rank = integer()
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
      agent_id = character(),
      symbol = character(),
      asset_id = integer(),
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
      agent_id = character(),
      symbol = character(),
      asset_id = integer(),
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
      agent_id = character(),
      symbol = character(),
      asset_id = integer(),
      accounting_model = character(),
      equity = numeric(),
      cash = numeric(),
      notional = numeric(),
      abs_notional = numeric(),
      unrealized_pnl = numeric(),
      maintenance_margin = numeric()
    ),
    risk_snapshots = data.table::data.table(
      timestamp = as.POSIXct(character()),
      agent_id = character(),
      symbol = character(),
      asset_id = integer(),
      equity = numeric(),
      abs_notional = numeric(),
      leverage = numeric(),
      maintenance_margin = numeric(),
      margin_buffer = numeric()
    )
  )
}

#' Get the tradesimr schema version
#'
#' @return A scalar character schema version.
#' @export
sim_schema_version <- function() {
  TRADESIMR_SCHEMA_VERSION
}

#' Migrate durable tables to the current schema
#'
#' Missing columns are added with typed `NA` values and existing columns are
#' retained unchanged. This makes older CSV/fst exports readable without
#' silently discarding consumer-defined extension columns.
#'
#' @param tables A named list of durable data tables.
#' @return A named list upgraded to [sim_schema_version()].
#' @export
sim_schema_migrate <- function(tables) {
  if (!is.list(tables) || is.null(names(tables))) stop("`tables` must be a named list.", call. = FALSE)
  schemas <- sim_schemas()
  out <- lapply(names(tables), function(name) {
    table <- data.table::as.data.table(tables[[name]])
    if (!name %in% names(schemas)) return(table)
    schema <- schemas[[name]]
    for (column in setdiff(names(schema), names(table))) {
      data.table::set(table, j = column, value = .schema_typed_na(schema[[column]], nrow(table)))
    }
    for (column in intersect(names(schema), names(table))) {
      data.table::set(table, j = column, value = .schema_cast_column(table[[column]], schema[[column]]))
    }
    table
  })
  names(out) <- names(tables)
  attr(out, "schema_version") <- TRADESIMR_SCHEMA_VERSION
  out
}

#' @keywords internal
.schema_typed_na <- function(prototype, n) {
  if (inherits(prototype, "POSIXt")) return(as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC"))
  if (is.integer(prototype)) return(rep.int(NA_integer_, n))
  if (is.logical(prototype)) return(rep.int(NA, n))
  if (is.numeric(prototype)) return(rep.int(NA_real_, n))
  rep.int(NA_character_, n)
}

#' @keywords internal
.schema_cast_column <- function(value, prototype) {
  if (inherits(prototype, "POSIXt")) return(as.POSIXct(value, tz = "UTC"))
  if (is.integer(prototype)) return(as.integer(value))
  if (is.logical(prototype)) return(as.logical(value))
  if (is.numeric(prototype)) return(as.numeric(value))
  as.character(value)
}

#' Build an export manifest
#'
#' @param paths Named character vector of exported files.
#' @param tables Named list of exported tables.
#' @param format Export file format.
#' @param config Optional simulation configuration.
#' @return A data.table manifest.
#' @export
sim_manifest <- function(paths, tables, format, config = list()) {
  data.table::data.table(
    schema_version = TRADESIMR_SCHEMA_VERSION,
    package_version = as.character(utils::packageVersion("tradesimr")),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    format = format,
    table = names(paths),
    file = basename(unname(paths)),
    rows = vapply(tables[names(paths)], nrow, integer(1)),
    config = paste(names(config), unlist(config, use.names = FALSE), sep = "=", collapse = ";")
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
#' @param symbol_col Optional input symbol column name.
#' @param asset_id_col Optional input asset identifier column name.
#' @param symbol Optional scalar symbol when the input has no symbol column.
#' @param asset_id Optional scalar asset identifier when the input has no asset
#'   identifier column.
#' @param observation_timestamp_col,bar_start_col,bar_end_col Optional source
#'   columns for explicit market-time metadata. `timestamp` remains the
#'   completed-bar decision boundary for compatibility.
#' @param market_timezone Time zone label when the input has no timezone column.
#' @return A data.table with canonical `timestamp`, `open`, `high`, `low`,
#'   `close` columns.
#' @export
as_market_bars <- function(data,
                           timestamp_col = "timestamp",
                           symbol_col = NULL,
                           asset_id_col = NULL,
                           symbol = NULL,
                           asset_id = NULL,
                           open_col = "open",
                           high_col = "high",
                           low_col = "low",
                           close_col = "close",
                           observation_timestamp_col = NULL,
                           bar_start_col = NULL,
                           bar_end_col = NULL,
                           market_timezone = "UTC") {
  validate_market_data(data, timestamp_col, open_col, high_col, low_col, close_col)
  DT <- data.table::as.data.table(data)
  if (is.null(symbol_col) && "symbol" %in% names(DT)) symbol_col <- "symbol"
  if (is.null(asset_id_col) && "asset_id" %in% names(DT)) asset_id_col <- "asset_id"
  if (is.null(symbol) && is.null(symbol_col) && !is.null(asset_id)) symbol <- paste0("asset-", asset_id)
  out <- DT[, .SD, .SDcols = c(timestamp_col, open_col, high_col, low_col, close_col)]
  data.table::setnames(out, c("timestamp", "open", "high", "low", "close"))
  symbol_values <- if (!is.null(symbol_col)) as.character(DT[[symbol_col]]) else rep.int(as.character(symbol %||% "default"), nrow(out))
  asset_values <- if (!is.null(asset_id_col)) {
    as.integer(DT[[asset_id_col]])
  } else if (!is.null(asset_id)) {
    rep.int(as.integer(asset_id), nrow(out))
  } else {
    vapply(symbol_values, .asset_id_from_symbol, integer(1))
  }
  data.table::set(out, j = "symbol", value = symbol_values)
  data.table::set(out, j = "asset_id", value = asset_values)
  observation_timestamp_col <- observation_timestamp_col %||% if ("observation_timestamp" %in% names(DT)) "observation_timestamp" else NULL
  bar_start_col <- bar_start_col %||% if ("bar_start" %in% names(DT)) "bar_start" else NULL
  bar_end_col <- bar_end_col %||% if ("bar_end" %in% names(DT)) "bar_end" else NULL
  time_value <- function(column, default) if (!is.null(column) && column %in% names(DT)) as.POSIXct(DT[[column]], tz = "UTC") else default
  data.table::set(out, j = "observation_timestamp", value = time_value(observation_timestamp_col, as.POSIXct(out$timestamp, tz = "UTC")))
  data.table::set(out, j = "bar_start", value = time_value(bar_start_col, as.POSIXct(rep(NA_real_, nrow(out)), origin = "1970-01-01", tz = "UTC")))
  data.table::set(out, j = "bar_end", value = time_value(bar_end_col, as.POSIXct(out$timestamp, tz = "UTC")))
  data.table::set(out, j = "valuation_timestamp", value = as.POSIXct(out$timestamp, tz = "UTC"))
  data.table::set(out, j = "market_timezone", value = if ("market_timezone" %in% names(DT)) as.character(DT$market_timezone) else rep.int(as.character(market_timezone), nrow(out)))
  data.table::set(out, j = "is_completed", value = if ("is_completed" %in% names(DT)) as.logical(DT$is_completed) else rep.int(TRUE, nrow(out)))
  data.table::set(out, j = "is_tradable", value = if ("is_tradable" %in% names(DT)) as.logical(DT$is_tradable) else rep.int(TRUE, nrow(out)))
  data.table::setcolorder(out, c("timestamp", "observation_timestamp", "bar_start", "bar_end", "valuation_timestamp", "market_timezone", "is_completed", "is_tradable", "symbol", "asset_id", "open", "high", "low", "close"))
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
