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
  cols <- c("timestamp", "pos_dir", "ctr_unit", "avg_price", "last_px", "notional", "unrealized_pnl")
  out <- DT[, .SD, .SDcols = intersect(cols, names(DT))]
  if (!"pos_dir" %in% names(out)) return(data.table::data.table())
  pos_label <- data.table::fifelse(out$pos_dir == 1L, "long",
    data.table::fifelse(out$pos_dir == -1L, "short",
      data.table::fifelse(out$pos_dir == 0L, "flat", NA_character_)
    )
  )
  data.table::set(out, j = "pos_label", value = pos_label)
  data.table::setcolorder(out, intersect(c("timestamp", "pos_dir", "pos_label", "ctr_unit", "avg_price", "last_px", "notional", "unrealized_pnl"), names(out)))
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
  cols <- c("timestamp", "equity", "cash", "notional", "abs_notional", "unrealized_pnl")
  DT[, .SD, .SDcols = intersect(cols, names(DT))]
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
                       tables = c("events", "orders", "fills", "positions", "cash_ledger", "account", "risk")) {
  format <- match.arg(format)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  if (format == "fst" && !requireNamespace("fst", quietly = TRUE)) {
    stop("Package `fst` is required for fst export. Install it or use format = 'csv'.", call. = FALSE)
  }

  table_map <- list(
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
  invisible(paths)
}
