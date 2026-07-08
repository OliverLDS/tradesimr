#' Export a static replay dashboard
#'
#' Writes dashboard-ready CSV tables, a manifest, and static HTML/CSS/JS assets.
#' The dashboard is a read-only consumer of durable market, strategy, event,
#' account, risk, order, and fill tables; it does not call C++, R6, or live
#' exchange internals.
#'
#' @param sim A simulation result returned by `sim_backtest()` or
#'   `sim_exchange_step()`.
#' @param path Output directory.
#' @return Invisibly returns a named character vector of written files.
#' @export
sim_replay_dashboard_export <- function(sim, path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  tables <- list(
    market_events = sim_market_events(sim),
    strategy_snapshots = .dashboard_strategy_snapshots(sim),
    events = sim_events(sim),
    account_snapshots = sim_account(sim),
    risk_snapshots = sim_risk(sim),
    orders = sim_orders(sim),
    fills = sim_fills(sim)
  )

  paths <- .dashboard_write_tables(tables, path, config = attr(sim, "sim_config", exact = TRUE) %||% list())
  paths <- c(paths, .dashboard_copy_assets(path, "replay"))
  invisible(paths)
}

#' Export a static simulation dashboard
#'
#' Compatibility alias for `sim_replay_dashboard_export()`.
#'
#' @inheritParams sim_replay_dashboard_export
#' @return Invisibly returns a named character vector of written files.
#' @export
sim_dashboard_export <- function(sim, path) {
  sim_replay_dashboard_export(sim, path)
}

#' @keywords internal
.dashboard_strategy_snapshots <- function(sim) {
  DT <- data.table::as.data.table(sim)
  cols <- c("timestamp", "tgt_pos", "pos_strat", "tol_pos", "pos_dir", "ctr_unit", "avg_price", "last_px")
  out <- DT[, .SD, .SDcols = intersect(cols, names(DT))]
  if (!"tgt_pos" %in% names(out)) return(.dashboard_empty_strategy_snapshots())
  out[]
}

#' Open an exported static dashboard
#'
#' @param path Directory created by a dashboard export helper.
#' @return Invisibly returns the dashboard index path.
#' @export
sim_dashboard_open <- function(path) {
  index <- file.path(path, "index.html")
  if (!file.exists(index)) {
    stop("Dashboard index does not exist. Run a dashboard export helper first.", call. = FALSE)
  }
  utils::browseURL(normalizePath(index, winslash = "/", mustWork = TRUE))
  invisible(index)
}

#' Export and open a simulated exchange dashboard
#'
#' @param exchange A `tradesimr_exchange` with a simulation result.
#' @param path Output directory.
#' @return Invisibly returns written dashboard files.
#' @export
sim_exchange_dashboard <- function(exchange, path) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$result)) {
    stop("Exchange has no simulation result. Run `sim_exchange_run()` or `sim_exchange_step()` first.", call. = FALSE)
  }
  paths <- sim_replay_dashboard_export(exchange$result, path)
  command_tables <- list(
    agent_commands = exchange$agent_commands,
    order_requests = exchange$order_requests,
    order_cancellations = exchange$order_cancellations,
    agent_orders = exchange$agent_orders
  )
  for (nm in names(command_tables)) {
    file <- file.path(path, paste0(nm, ".csv"))
    data.table::fwrite(command_tables[[nm]], file)
    paths[[nm]] <- file
  }
  sim_dashboard_open(path)
  invisible(paths)
}

#' Export a live-state dashboard
#'
#' Writes the current exchange state using the god-facing live-state dashboard
#' shell. This dashboard can configure and step the feed but cannot place
#' orders.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param path Output directory.
#' @return Invisibly returns a named character vector of written files.
#' @export
sim_state_dashboard_export <- function(exchange, path) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  tables <- .dashboard_exchange_tables(exchange)
  paths <- .dashboard_write_tables(tables, path, config = exchange$config %||% list())
  paths <- c(paths, .dashboard_copy_assets(path, "live_state"))
  invisible(paths)
}

#' Open a live-state dashboard
#'
#' @inheritParams sim_state_dashboard_export
#' @return Invisibly returns the dashboard index path.
#' @export
sim_live_state_dashboard_open <- function(exchange = sim_exchange_new(), path = tempfile("tradesimr-live-state-")) {
  sim_state_dashboard_export(exchange, path)
  sim_dashboard_open(path)
}

#' Export an agent-facing live dashboard
#'
#' @param exchange A `tradesimr_exchange`.
#' @param path Output directory.
#' @return Invisibly returns a named character vector of written files.
#' @export
sim_agent_dashboard_export <- function(exchange, path) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  tables <- .dashboard_exchange_tables(exchange)
  paths <- .dashboard_write_tables(tables, path, config = exchange$config %||% list())
  paths <- c(paths, .dashboard_copy_assets(path, "live_agent"))
  invisible(paths)
}

#' Open an agent-facing live dashboard
#'
#' @inheritParams sim_agent_dashboard_export
#' @return Invisibly returns the dashboard index path.
#' @export
sim_agent_dashboard_open <- function(exchange = sim_exchange_new(), path = tempfile("tradesimr-live-agent-")) {
  sim_agent_dashboard_export(exchange, path)
  sim_dashboard_open(path)
}

#' @keywords internal
.dashboard_exchange_tables <- function(exchange) {
  result <- exchange$result
  events <- if (is.null(result)) sim_schemas()$events[0] else sim_events(result)
  agent_orders <- data.table::copy(exchange$agent_orders)
  orders <- if (nrow(agent_orders) > 0L) agent_orders else if (is.null(result)) sim_orders(events) else sim_orders(result)
  list(
    market_events = data.table::copy(exchange$market_events),
    strategy_snapshots = if (is.null(result)) .dashboard_empty_strategy_snapshots() else .dashboard_strategy_snapshots(result),
    events = events,
    account_snapshots = if (is.null(result)) .dashboard_empty_account_snapshots() else sim_account(result),
    risk_snapshots = if (is.null(result)) .dashboard_empty_risk_snapshots() else sim_risk(result),
    orders = orders,
    fills = if (is.null(result)) sim_schemas()$events[0] else sim_fills(result),
    agent_commands = data.table::copy(exchange$agent_commands),
    order_requests = data.table::copy(exchange$order_requests),
    order_cancellations = data.table::copy(exchange$order_cancellations),
    agent_orders = agent_orders,
    feed_status = data.table::as.data.table(sim_feed_status(exchange))
  )
}

#' @keywords internal
.dashboard_empty_strategy_snapshots <- function() {
  data.table::data.table(
    timestamp = as.POSIXct(character()),
    tgt_pos = numeric(),
    pos_strat = integer(),
    tol_pos = numeric(),
    pos_dir = integer(),
    ctr_unit = numeric(),
    avg_price = numeric(),
    last_px = numeric()
  )
}

#' @keywords internal
.dashboard_empty_account_snapshots <- function() {
  data.table::data.table(
    timestamp = as.POSIXct(character()),
    equity = numeric(),
    cash = numeric(),
    notional = numeric(),
    abs_notional = numeric(),
    unrealized_pnl = numeric()
  )
}

#' @keywords internal
.dashboard_empty_risk_snapshots <- function() {
  data.table::data.table(
    timestamp = as.POSIXct(character()),
    equity = numeric(),
    abs_notional = numeric(),
    leverage = numeric(),
    maintenance_margin = numeric(),
    margin_buffer = numeric()
  )
}

#' @keywords internal
.dashboard_write_tables <- function(tables, path, config = list()) {
  paths <- character(length(tables))
  names(paths) <- names(tables)
  for (nm in names(tables)) {
    file <- file.path(path, paste0(nm, ".csv"))
    data.table::fwrite(tables[[nm]], file)
    paths[[nm]] <- file
  }
  manifest <- sim_manifest(paths = paths, tables = tables, format = "csv", config = config)
  manifest_file <- file.path(path, "manifest.csv")
  data.table::fwrite(manifest, manifest_file)
  c(paths, manifest = manifest_file)
}

#' @keywords internal
.dashboard_copy_assets <- function(path, app) {
  app_dir <- .dashboard_asset_dir(app)
  shared_dir <- .dashboard_asset_dir("shared")
  shared_out <- file.path(path, "shared")
  if (!dir.exists(shared_out)) dir.create(shared_out, recursive = TRUE)
  asset_files <- c(
    index = file.path(app_dir, "index.html"),
    dashboard = file.path(shared_dir, "dashboard.js"),
    style = file.path(shared_dir, "style.css")
  )
  ok <- c(
    file.copy(asset_files[["index"]], file.path(path, "index.html"), overwrite = TRUE),
    file.copy(asset_files[["dashboard"]], file.path(shared_out, "dashboard.js"), overwrite = TRUE),
    file.copy(asset_files[["style"]], file.path(shared_out, "style.css"), overwrite = TRUE)
  )
  if (!all(ok)) stop("Failed to copy dashboard assets.", call. = FALSE)
  c(
    index = file.path(path, "index.html"),
    dashboard = file.path(shared_out, "dashboard.js"),
    style = file.path(shared_out, "style.css")
  )
}

#' @keywords internal
.dashboard_asset_dir <- function(app = NULL) {
  asset_dir <- system.file("dashboard", package = "tradesimr")
  if (!nzchar(asset_dir)) {
    asset_dir <- file.path(getwd(), "inst", "dashboard")
  }
  if (!is.null(app)) asset_dir <- file.path(asset_dir, app)
  if (!dir.exists(asset_dir)) stop("Dashboard assets were not found.", call. = FALSE)
  asset_dir
}
