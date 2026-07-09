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
  cols <- c("timestamp", "agent_id", "symbol", "asset_id", "equity", "cash", "notional", "abs_notional", "unrealized_pnl", "maintenance_margin")
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

#' Compute cross-asset risk for live exchange agents
#'
#' @param exchange A `tradesimr_exchange`.
#' @param stress_sigma Multiplier applied to portfolio return volatility for
#'   the stress-loss estimate.
#' @return A data.table with one row per agent and exposed asset.
#' @export
sim_cross_asset_risk <- function(exchange, stress_sigma = 2) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  positions <- sim_exchange_positions(exchange)
  if (nrow(positions) == 0L) {
    return(data.table::data.table(
      timestamp = as.POSIXct(character()),
      agent_id = character(),
      symbol = character(),
      asset_id = integer(),
      asset_class = character(),
      quantity = numeric(),
      direction = character(),
      notional = numeric(),
      abs_notional = numeric(),
      allocation = numeric(),
      asset_class_allocation = numeric(),
      unrealized_pnl = numeric(),
      equity = numeric(),
      leverage = numeric(),
      concentration_hhi = numeric(),
      factor_exposure = numeric(),
      max_drawdown = numeric(),
      risk_contribution = numeric(),
      portfolio_vol = numeric(),
      stress_loss = numeric()
    ))
  }
  positions <- data.table::copy(positions)
  assets <- sim_assets(exchange)
  if (!"asset_id" %in% names(positions)) positions[, asset_id := NA_integer_]
  if (nrow(assets) > 0L && "asset_id" %in% names(assets)) {
    positions <- merge(positions, assets[, .(asset_id, asset_class)], by = "asset_id", all.x = TRUE)
  } else {
    positions[, asset_class := NA_character_]
  }
  positions <- positions[abs(as.numeric(ctr_unit)) > 0 | abs(as.numeric(notional)) > 0]
  if (nrow(positions) == 0L) return(sim_cross_asset_risk_empty())
  account <- sim_exchange_account(exchange)
  latest_account <- if (nrow(account) > 0L && "agent_id" %in% names(account)) {
    account[, .SD[.N], by = agent_id]
  } else {
    data.table::data.table(agent_id = unique(positions$agent_id), equity = NA_real_)
  }
  positions[, abs_notional := abs(as.numeric(notional))]
  positions[, direction := data.table::fcase(
    pos_dir > 0, "long",
    pos_dir < 0, "short",
    default = "flat"
  )]
  out <- merge(
    positions[, .(timestamp, agent_id, symbol, asset_id, asset_class, quantity = ctr_unit, direction, notional, abs_notional, unrealized_pnl)],
    latest_account[, .(agent_id, equity)],
    by = "agent_id",
    all.x = TRUE
  )
  out[, total_abs_notional := sum(abs_notional, na.rm = TRUE), by = agent_id]
  out[, allocation := data.table::fifelse(total_abs_notional > 0, abs_notional / total_abs_notional, 0)]
  out[, asset_class_allocation := sum(allocation, na.rm = TRUE), by = .(agent_id, asset_class)]
  out[, leverage := data.table::fifelse(equity > 0, total_abs_notional / equity, Inf)]
  out[, concentration_hhi := sum(allocation * allocation, na.rm = TRUE), by = agent_id]
  factor <- .cross_asset_factor_exposure(exchange, positions)
  drawdown <- .cross_asset_drawdown(exchange)
  stress <- .cross_asset_stress(exchange, positions, stress_sigma = stress_sigma)
  out <- merge(out, stress, by = "agent_id", all.x = TRUE)
  out <- merge(out, factor, by = c("agent_id", "asset_id"), all.x = TRUE)
  out <- merge(out, drawdown, by = "agent_id", all.x = TRUE)
  out[, risk_contribution := data.table::fifelse(portfolio_vol > 0, abs_notional * abs(factor_exposure %||% 0) / portfolio_vol, 0)]
  out[, total_abs_notional := NULL]
  data.table::setcolorder(out, c("timestamp", "agent_id", "symbol", "asset_id", "asset_class", "quantity", "direction", "notional", "abs_notional", "allocation", "asset_class_allocation", "unrealized_pnl", "equity", "leverage", "concentration_hhi", "factor_exposure", "max_drawdown", "risk_contribution", "portfolio_vol", "stress_loss"))
  out[]
}

#' @keywords internal
sim_cross_asset_risk_empty <- function() {
  data.table::data.table(
    timestamp = as.POSIXct(character()),
    agent_id = character(),
    symbol = character(),
    asset_id = integer(),
    asset_class = character(),
    quantity = numeric(),
    direction = character(),
    notional = numeric(),
    abs_notional = numeric(),
    allocation = numeric(),
    asset_class_allocation = numeric(),
    unrealized_pnl = numeric(),
    equity = numeric(),
    leverage = numeric(),
    concentration_hhi = numeric(),
    factor_exposure = numeric(),
    max_drawdown = numeric(),
    risk_contribution = numeric(),
    portfolio_vol = numeric(),
    stress_loss = numeric()
  )
}

#' @keywords internal
.cross_asset_stress <- function(exchange, positions, stress_sigma = 2) {
  assets <- sort(unique(as.integer(positions$asset_id)))
  cov <- .cross_asset_covariance(exchange, assets)
  rows <- lapply(unique(positions$agent_id), function(aid) {
    pos <- positions[positions[["agent_id"]] == aid]
    exposure <- numeric(length(assets))
    idx <- match(as.integer(pos$asset_id), assets)
    exposure[idx] <- as.numeric(pos$notional)
    variance <- as.numeric(t(exposure) %*% cov %*% exposure)
    portfolio_vol <- sqrt(max(0, variance))
    data.table::data.table(
      agent_id = as.character(aid),
      portfolio_vol = portfolio_vol,
      stress_loss = as.numeric(stress_sigma) * portfolio_vol
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

#' @keywords internal
.cross_asset_factor_exposure <- function(exchange, positions) {
  assets <- sort(unique(as.integer(positions$asset_id)))
  model <- exchange$market_model %||% sim_market_model_config()
  loadings <- NULL
  if (identical(model$model, "factor_random_walk") && !is.null(model$factors)) {
    feeds <- data.table::data.table(asset_id = assets, symbol = positions$symbol[match(assets, positions$asset_id)])
    loadings <- .market_model_loadings(model$factors$loadings %||% matrix(rep(0.7, length(assets)), nrow = length(assets)), length(assets))
  } else if (!is.null(model$calibration$factors$loadings)) {
    loadings <- .market_model_loadings(model$calibration$factors$loadings, length(assets))
  }
  if (is.null(loadings)) {
    return(data.table::data.table(agent_id = positions$agent_id, asset_id = positions$asset_id, factor_exposure = 0)[0])
  }
  rows <- lapply(unique(positions$agent_id), function(aid) {
    pos <- positions[positions[["agent_id"]] == aid]
    idx <- match(as.integer(pos$asset_id), assets)
    exposure <- as.numeric(pos$notional) * loadings[idx, 1L]
    data.table::data.table(agent_id = as.character(aid), asset_id = as.integer(pos$asset_id), factor_exposure = exposure)
  })
  data.table::rbindlist(rows, fill = TRUE)
}

#' @keywords internal
.cross_asset_drawdown <- function(exchange) {
  history <- if (!is.null(exchange$result) && nrow(exchange$result) > 0L) {
    sim_account(exchange$result)
  } else if (!is.null(exchange$step_snapshots) && nrow(exchange$step_snapshots) > 0L) {
    .aggregate_account_snapshots(exchange$step_snapshots)
  } else {
    sim_exchange_account(exchange)
  }
  if (nrow(history) == 0L || !"agent_id" %in% names(history)) {
    return(data.table::data.table(agent_id = character(), max_drawdown = numeric()))
  }
  history <- data.table::copy(history)
  history <- history[is.finite(equity)]
  if (nrow(history) == 0L) return(data.table::data.table(agent_id = character(), max_drawdown = numeric()))
  data.table::setorderv(history, intersect(c("agent_id", "timestamp"), names(history)))
  history[, peak := cummax(equity), by = agent_id]
  history[, drawdown := data.table::fifelse(peak > 0, 1 - equity / peak, 0)]
  history[, .(max_drawdown = max(drawdown, na.rm = TRUE)), by = agent_id]
}

#' @keywords internal
.cross_asset_covariance <- function(exchange, asset_ids) {
  model <- exchange$market_model %||% sim_market_model_config()
  requested_ids <- as.integer(asset_ids)
  full_ids <- sort(unique(as.integer(names(exchange$feeds))))
  if (!length(full_ids)) full_ids <- requested_ids
  feeds <- data.table::rbindlist(lapply(full_ids, function(asset_id) {
    selected <- .feed_select(exchange, asset_id = asset_id)
    feed <- selected$feed
    data.table::data.table(
      asset_id = as.integer(asset_id),
      vol = abs(as.numeric(feed$random_walk$vol %||% 0.02))
    )
  }), fill = TRUE)
  subset_cov <- function(cov) {
    idx <- match(requested_ids, feeds$asset_id)
    cov[idx, idx, drop = FALSE]
  }
  if (identical(model$model, "factor_random_walk") && !is.null(model$factors)) {
    factors <- model$factors
    loadings <- .market_model_loadings(factors$loadings %||% matrix(rep(0.7, nrow(feeds)), nrow = nrow(feeds)), nrow(feeds))
    factor_vol <- rep_len(as.numeric(factors$factor_vol %||% 0.01), ncol(loadings))
    idio_vol <- rep_len(as.numeric(factors$idio_vol %||% feeds$vol), nrow(feeds))
    return(subset_cov(loadings %*% diag(factor_vol^2, nrow = length(factor_vol)) %*% t(loadings) + diag(idio_vol^2, nrow = nrow(feeds))))
  }
  if (identical(model$model, "regime_random_walk")) {
    regimes <- .market_model_regimes(model$regimes, nrow(feeds))
    current <- as.integer(model$state$current_regime %||% regimes$initial_state)
    state <- regimes$states[[current]]
    feeds[, vol := vol * rep_len(as.numeric(state$vol_multiplier %||% 1), .N)]
    model$corr <- state$corr %||% model$corr
    model$cov <- state$cov %||% NULL
  }
  subset_cov(.market_model_covariance(model, feeds))
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
