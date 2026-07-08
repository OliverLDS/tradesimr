#' Extract fill events from a simulation
#'
#' @param sim A simulation result returned by `sim_backtest()`.
#' @return A data.table of filled trade events.
#' @export
sim_fills <- function(sim) {
  events <- sim_events(sim)
  if (nrow(events) == 0L) return(events)
  events[events$event_type_label == "trade" & events$status_label == "filled"]
}

#' Extract position snapshots from a simulation
#'
#' @param sim A simulation result returned by `sim_backtest()`.
#' @return A data.table of bar-level position snapshots.
#' @export
sim_positions <- function(sim) {
  DT <- data.table::as.data.table(sim)
  cols <- c("timestamp", "agent_id", "symbol", "asset_id", "pos_dir", "ctr_unit", "avg_price", "last_px", "notional", "unrealized_pnl")
  out <- DT[, .SD, .SDcols = intersect(cols, names(DT))]
  if (!"pos_dir" %in% names(out)) return(data.table::data.table())
  pos_label <- data.table::fifelse(out$pos_dir == 1L, "long",
    data.table::fifelse(out$pos_dir == -1L, "short",
      data.table::fifelse(out$pos_dir == 0L, "flat", NA_character_)
    )
  )
  data.table::set(out, j = "pos_label", value = pos_label)
  data.table::setcolorder(out, intersect(c("timestamp", "agent_id", "symbol", "asset_id", "pos_dir", "pos_label", "ctr_unit", "avg_price", "last_px", "notional", "unrealized_pnl"), names(out)))
  out[]
}

#' Extract cash ledger entries from a simulation
#'
#' @param sim A simulation result returned by `sim_backtest()`.
#' @return A data.table of event-level cash changes.
#' @export
sim_cash_ledger <- function(sim) {
  events <- sim_events(sim)
  if (nrow(events) == 0L) return(events)
  cols <- c("timestamp", "event_id", "event_type_label", "action_label", "cash", "fee", "funding_fee", "realized_pnl")
  events[, .SD, .SDcols = intersect(cols, names(events))]
}

#' Extract account snapshots from a simulation
#'
#' @param sim A simulation result returned by `sim_backtest()`.
#' @return A data.table of bar-level account snapshots.
#' @export
sim_account <- function(sim) {
  DT <- data.table::as.data.table(sim)
  cols <- c("timestamp", "agent_id", "symbol", "asset_id", "equity", "cash", "notional", "abs_notional", "unrealized_pnl")
  DT[, .SD, .SDcols = intersect(cols, names(DT))]
}

#' Extract market bars from a simulation
#'
#' @param sim A simulation result returned by `sim_backtest()` or
#'   `sim_exchange_step()`.
#' @return A data.table with `timestamp`, `open`, `high`, `low`, and `close`.
#' @export
sim_market_events <- function(sim) {
  attr_bars <- attr(sim, "market_events", exact = TRUE)
  if (!is.null(attr_bars)) return(as_market_bars(attr_bars))
  DT <- data.table::as.data.table(sim)
  required <- c("timestamp", "open", "high", "low", "close")
  if (!all(required %in% names(DT))) return(sim_schemas()$market_events[0])
  cols <- intersect(c("timestamp", "symbol", "asset_id", "open", "high", "low", "close"), names(DT))
  as_market_bars(DT[, .SD, .SDcols = cols])
}

#' Extract risk snapshots from a simulation
#'
#' @param sim A simulation result returned by `sim_backtest()`.
#' @return A data.table of bar-level risk snapshots.
#' @export
sim_risk <- function(sim) {
  DT <- data.table::as.data.table(sim)
  if (nrow(DT) == 0L) return(data.table::data.table())
  out <- data.table::data.table(
    timestamp = DT$timestamp,
    agent_id = if ("agent_id" %in% names(DT)) DT$agent_id else NA_character_,
    symbol = if ("symbol" %in% names(DT)) DT$symbol else NA_character_,
    asset_id = if ("asset_id" %in% names(DT)) DT$asset_id else NA_integer_,
    equity = DT$equity,
    abs_notional = DT$abs_notional,
    leverage = data.table::fifelse(DT$equity > 0, DT$abs_notional / DT$equity, Inf),
    maintenance_margin = DT$maintenance_margin,
    margin_buffer = DT$equity - DT$maintenance_margin
  )
  out[]
}

#' Export simulation tables to durable files
#'
#' @param sim A simulation result returned by `sim_backtest()`.
#' @param path Output directory.
#' @param format File format, either `csv` or `fst`.
#' @param tables Names of tables to export.
#' @return Invisibly returns a named character vector of written file paths.
#' @export
sim_export <- function(sim,
                       path,
                       format = c("csv", "fst"),
                       tables = c("simulation", "market_events", "events", "orders", "fills", "positions", "cash_ledger", "account", "risk")) {
  format <- match.arg(format)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  if (format == "fst" && !requireNamespace("fst", quietly = TRUE)) {
    stop("Package `fst` is required for fst export. Install it or use format = 'csv'.", call. = FALSE)
  }

  table_map <- list(
    simulation = data.table::as.data.table(sim),
    market_events = sim_market_events(sim),
    events = sim_events(sim),
    orders = sim_orders(sim),
    fills = sim_fills(sim),
    positions = sim_positions(sim),
    cash_ledger = sim_cash_ledger(sim),
    account = sim_account(sim),
    risk = sim_risk(sim)
  )
  table_map <- table_map[intersect(tables, names(table_map))]

  paths <- character(length(table_map))
  names(paths) <- names(table_map)
  for (nm in names(table_map)) {
    file <- file.path(path, paste0(nm, ".", format))
    if (format == "csv") {
      data.table::fwrite(table_map[[nm]], file)
    } else {
      fst::write_fst(as.data.frame(table_map[[nm]]), file)
    }
    paths[[nm]] <- file
  }

  manifest <- sim_manifest(
    paths = paths,
    tables = table_map,
    format = format,
    config = attr(sim, "sim_config", exact = TRUE) %||% list()
  )
  manifest_file <- file.path(path, "manifest.csv")
  data.table::fwrite(manifest, manifest_file)
  paths <- c(paths, manifest = manifest_file)
  invisible(paths)
}

#' Read an exported simulation table
#'
#' @param path Export directory or table file path.
#' @param table Table name when `path` is a directory.
#' @param format Optional format. Inferred from manifest or file extension when
#'   absent.
#' @return A data.table.
#' @export
sim_read_table <- function(path, table, format = NULL) {
  file <- path
  if (dir.exists(path)) {
    manifest <- sim_read_manifest(path)
    table_name <- table
    row <- manifest[manifest[["table"]] == table_name, ]
    if (nrow(row) == 0L) stop("Table not found in manifest: ", table, call. = FALSE)
    file <- file.path(path, row$file[1L])
    format <- row$format[1L]
  }
  if (is.null(format)) {
    format <- sub("^.*\\.", "", basename(file))
  }
  if (format == "csv") {
    data.table::fread(file)
  } else if (format == "fst") {
    if (!requireNamespace("fst", quietly = TRUE)) {
      stop("Package `fst` is required to read fst exports.", call. = FALSE)
    }
    data.table::as.data.table(fst::read_fst(file))
  } else {
    stop("Unsupported table format: ", format, call. = FALSE)
  }
}

#' Read an export manifest
#'
#' @param path Export directory or manifest file path.
#' @return A data.table manifest.
#' @export
sim_read_manifest <- function(path) {
  file <- if (dir.exists(path)) file.path(path, "manifest.csv") else path
  if (!file.exists(file)) stop("Manifest file does not exist: ", file, call. = FALSE)
  data.table::fread(file)
}

#' Import exported simulation tables
#'
#' @param path Export directory created by `sim_export()`.
#' @return A named list containing `manifest`, `simulation`, and available
#'   durable tables.
#' @export
sim_import <- function(path) {
  manifest <- sim_read_manifest(path)
  out <- list(manifest = manifest)
  for (table in manifest$table) {
    out[[table]] <- sim_read_table(path, table)
  }
  if (!is.null(out$simulation)) {
    sim <- data.table::as.data.table(out$simulation)
    if (!is.null(out$events)) data.table::setattr(sim, "events", sim_events(out$events))
    if (!is.null(out$orders)) data.table::setattr(sim, "orders", out$orders)
    data.table::setattr(sim, "import_manifest", manifest)
    out$simulation <- sim
  }
  out
}

#' Read exported simulation events
#'
#' @param path Export directory created by `sim_export()`.
#' @return A data.table of events.
#' @export
sim_read_events <- function(path) {
  sim_events(sim_read_table(path, "events"))
}

#' Read exported account snapshots
#'
#' @param path Export directory created by `sim_export()`.
#' @return A data.table of account snapshots.
#' @export
sim_read_account <- function(path) {
  sim_read_table(path, "account")
}

#' Reconstruct simulation views from exported events
#'
#' @param path Export directory created by `sim_export()`.
#' @return A data.table simulation result when a `simulation` table exists,
#'   otherwise event-level account state reconstructed from events.
#' @export
sim_run_from_events <- function(path) {
  imported <- sim_import(path)
  if (!is.null(imported$simulation)) return(imported$simulation)
  events <- sim_events(imported$events)
  out <- events[, .SD, .SDcols = intersect(
    c("timestamp", "equity", "cash", "state_dir", "state_ctr_unit", "avg_price", "last_px", "notional", "abs_notional", "unrealized_pnl", "maintenance_margin"),
    names(events)
  )]
  if ("state_dir" %in% names(out)) data.table::setnames(out, "state_dir", "pos_dir")
  if ("state_ctr_unit" %in% names(out)) data.table::setnames(out, "state_ctr_unit", "ctr_unit")
  data.table::setattr(out, "events", events)
  data.table::setattr(out, "orders", sim_orders(events))
  out[]
}
