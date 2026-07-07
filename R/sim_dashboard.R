#' Export a static simulation dashboard
#'
#' Writes dashboard-ready CSV tables, a manifest, and static HTML/CSS/JS assets.
#' The dashboard is a read-only consumer of durable event/account/risk tables; it
#' does not call C++, R6, or live exchange internals.
#'
#' @param sim A simulation result returned by `sim_backtest()` or
#'   `sim_exchange_step()`.
#' @param path Output directory.
#' @return Invisibly returns a named character vector of written files.
#' @export
sim_dashboard_export <- function(sim, path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  tables <- list(
    events = sim_events(sim),
    account_snapshots = sim_account(sim),
    risk_snapshots = sim_risk(sim),
    orders = sim_orders(sim),
    fills = sim_fills(sim)
  )

  paths <- character(length(tables))
  names(paths) <- names(tables)
  for (nm in names(tables)) {
    file <- file.path(path, paste0(nm, ".csv"))
    data.table::fwrite(tables[[nm]], file)
    paths[[nm]] <- file
  }

  manifest <- sim_manifest(
    paths = paths,
    tables = tables,
    format = "csv",
    config = attr(sim, "sim_config", exact = TRUE) %||% list()
  )
  manifest_file <- file.path(path, "manifest.csv")
  data.table::fwrite(manifest, manifest_file)
  paths <- c(paths, manifest = manifest_file)

  asset_dir <- .dashboard_asset_dir()
  asset_files <- file.path(asset_dir, c("index.html", "dashboard.js", "style.css"))
  ok <- file.copy(asset_files, path, overwrite = TRUE)
  if (!all(ok)) stop("Failed to copy dashboard assets.", call. = FALSE)
  paths <- c(paths, stats::setNames(file.path(path, basename(asset_files)), tools::file_path_sans_ext(basename(asset_files))))

  invisible(paths)
}

#' Open a static simulation dashboard
#'
#' @param path Directory created by `sim_dashboard_export()`.
#' @return Invisibly returns the dashboard index path.
#' @export
sim_dashboard_open <- function(path) {
  index <- file.path(path, "index.html")
  if (!file.exists(index)) {
    stop("Dashboard index does not exist. Run `sim_dashboard_export()` first.", call. = FALSE)
  }
  utils::browseURL(normalizePath(index, winslash = "/", mustWork = TRUE))
  invisible(index)
}

#' Export and open a simulated exchange dashboard
#'
#' @param exchange A `tradesimr_exchange` with a simulation result.
#' @param path Output directory.
#' @return Invisibly returns written files from `sim_dashboard_export()`.
#' @export
sim_exchange_dashboard <- function(exchange, path) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$result)) {
    stop("Exchange has no simulation result. Run `sim_exchange_run()` or `sim_exchange_step()` first.", call. = FALSE)
  }
  paths <- sim_dashboard_export(exchange$result, path)
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

#' @keywords internal
.dashboard_asset_dir <- function() {
  asset_dir <- system.file("dashboard", package = "tradesimr")
  if (!nzchar(asset_dir)) {
    asset_dir <- file.path(getwd(), "inst", "dashboard")
  }
  if (!dir.exists(asset_dir)) stop("Dashboard assets were not found.", call. = FALSE)
  asset_dir
}
