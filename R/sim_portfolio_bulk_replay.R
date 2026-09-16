#' Replay a historical multi-asset target-weight panel
#'
#' This bulk API is intended for historical reconstruction. It preserves the
#' public market-boundary and target-submission semantics while accumulating
#' durable snapshots and events in batches rather than repeatedly growing their
#' history tables. Live callers should continue to use
#' [sim_portfolio_market_step()] and [sim_portfolio_target_submit_batch()].
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars Completed OHLC bars with `timestamp`, `symbol`, `asset_id`,
#'   `open`, `high`, `low`, and `close`. Each timestamp is one market boundary.
#' @param target_weights A data frame with `timestamp`, `agent_id`, `symbol`,
#'   and `target_weight`. Each agent/timestamp group is submitted atomically.
#' @param allowed_symbols Optional named list of allowed-symbol vectors by
#'   agent. When omitted, each agent's universe is inferred from all symbols in
#'   its panel rows. Multi-asset target groups are submitted only at boundaries
#'   containing one completed bar for every allowed symbol; incomplete groups
#'   are treated as absent decisions.
#' @param execution Execution assumptions from [sim_portfolio_execution()].
#' @param decision_policy Market-observation policy from
#'   [sim_portfolio_decision_policy()]. Applied independently to each agent
#'   decision after the market boundary has been accepted.
#' @param rebalance_policy Optional policy for sparse deterministic target
#'   panels. `NULL` (the default) preserves historical behavior and submits
#'   every agent/timestamp target group. When supplied, it must be a list with
#'   `rebalance_due_column` (default `"rebalance_due"`) and a non-negative
#'   `drift_tolerance`. A group is submitted only when its due flag is `TRUE`
#'   and either its target vector differs from the last submitted vector or a
#'   supplied symbol's post-market realized-weight drift exceeds the tolerance.
#'   A skipped group is an absent decision: it creates no target, rebalance, or
#'   order record.
#' @param export_path Optional directory for public-safe per-agent exports.
#' @param profile Whether to return wall-time categories.
#' @return A list with the exchange, durable orders/fills/positions/accounts,
#'   targets/rebalances, execution quality, optional export paths, and timings.
#' @export
sim_portfolio_target_replay <- function(exchange,
                                        bars,
                                        target_weights,
                                        allowed_symbols = NULL,
                                        execution = sim_portfolio_execution(),
                                        decision_policy = sim_portfolio_decision_policy(),
                                        rebalance_policy = NULL,
                                        export_path = NULL,
                                        profile = FALSE) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  execution <- .portfolio_validate_execution(execution)
  decision_policy <- .portfolio_validate_decision_policy(decision_policy)
  bars <- .portfolio_validate_decision_bars(exchange, bars)
  panel <- data.table::as.data.table(target_weights)
  required <- c("timestamp", "agent_id", "symbol", "target_weight")
  if (!all(required %in% names(panel))) {
    stop("`target_weights` must contain: ", paste(required, collapse = ", "), call. = FALSE)
  }
  panel[, `:=`(
    timestamp = as.POSIXct(timestamp, tz = "UTC"),
    agent_id = as.character(agent_id),
    symbol = as.character(symbol),
    target_weight = as.numeric(target_weight)
  )]
  if (anyNA(panel$timestamp) || any(!nzchar(panel$agent_id)) || any(!nzchar(panel$symbol)) || any(!is.finite(panel$target_weight))) {
    stop("`target_weights` contains an invalid timestamp, agent id, symbol, or target weight.", call. = FALSE)
  }
  if (anyDuplicated(panel[, .(timestamp, agent_id, symbol)])) {
    stop("`target_weights` must contain at most one row per timestamp, agent, and symbol.", call. = FALSE)
  }
  data.table::setorderv(bars, c("timestamp", "asset_id"))
  data.table::setorderv(panel, c("timestamp", "agent_id", "symbol"))
  if (!all(panel$timestamp %in% bars$timestamp)) {
    stop("Every target timestamp must have a completed market boundary in `bars`.", call. = FALSE)
  }
  agent_ids <- unique(panel$agent_id)
  if (is.null(allowed_symbols)) {
    allowed_symbols <- stats::setNames(lapply(agent_ids, function(current_agent_id) {
      unique(panel$symbol[panel$agent_id == current_agent_id])
    }), agent_ids)
  }
  if (!is.list(allowed_symbols) || is.null(names(allowed_symbols)) || !all(agent_ids %in% names(allowed_symbols))) {
    stop("`allowed_symbols` must be a named list covering every panel agent.", call. = FALSE)
  }
  rebalance_policy <- .portfolio_validate_rebalance_policy(rebalance_policy, names(panel))
  # Asset registration is immutable during a replay. Resolve every requested
  # universe once instead of decoding the same agent configuration at each
  # boundary.
  allowed_assets_by_agent <- stats::setNames(lapply(agent_ids, function(current_agent_id) {
    .portfolio_resolve_allowed_assets(
      exchange, current_agent_id, allowed_symbols = allowed_symbols[[current_agent_id]]
    )
  }), agent_ids)

  accumulator <- new.env(parent = emptyenv())
  accumulator$step_snapshots <- list(data.table::copy(exchange$step_snapshots))
  accumulator$step_events <- list(data.table::copy(exchange$step_events))
  timings <- new.env(parent = emptyenv())
  timings$orchestration <- 0
  timings$portfolio_step_rcpp <- 0
  timings$ledger <- 0
  timings$boundary_normalization <- 0
  timings$boundary_bar_slicing <- 0
  timings$target_panel_slicing <- 0
  timings$account_position_lookup <- 0
  timings$target_planning <- 0
  timings$exchange_state_updates <- 0
  timings$durable_append_bind <- 0
  timings$supersession_checks <- 0
  timings$boundary_snapshot_bookkeeping <- 0
  timings$execution_quality <- 0
  timings$serialization_export <- 0
  timings$order_fill_event_ledger_writes <- 0
  timings$snapshot_construction <- 0
  timings$execution_quality_joins <- 0
  timings$boundary_latency_seconds <- numeric()
  exchange$.bulk_accumulator <- accumulator
  previous_covariance_cache <- exchange$.portfolio_covariance_cache %||% NULL
  exchange$.portfolio_covariance_cache <- new.env(parent = emptyenv())
  exchange$.profile_timings <- if (isTRUE(profile)) timings else NULL
  on.exit({
    exchange$.bulk_accumulator <- NULL
    exchange$.portfolio_covariance_cache <- previous_covariance_cache
    exchange$.profile_timings <- NULL
  }, add = TRUE)

  started <- proc.time()[["elapsed"]]
  boundary_values <- unique(as.numeric(bars$timestamp))
  # Split once. Repeated logical filtering of the full bar/target panels was
  # measurable on long Arena reconstructions.
  bars_by_boundary <- split(bars, factor(as.numeric(bars$timestamp), levels = boundary_values))
  panel_by_boundary <- split(panel, factor(as.numeric(panel$timestamp), levels = boundary_values))
  for (boundary_index in seq_along(boundary_values)) {
    boundary_started <- proc.time()[["elapsed"]]
    slicing_started <- .sim_profile_start(exchange)
    boundary_bars <- data.table::as.data.table(bars_by_boundary[[boundary_index]])
    .sim_profile_add(exchange, "boundary_bar_slicing", slicing_started)
    .portfolio_market_step_compact(exchange, boundary_bars, execution)
    slicing_started <- .sim_profile_start(exchange)
    decision_rows <- data.table::as.data.table(panel_by_boundary[[boundary_index]])
    .sim_profile_add(exchange, "target_panel_slicing", slicing_started)
    normalization_started <- .sim_profile_start(exchange)
    if (nrow(decision_rows)) {
      policy_context <- if (is.null(rebalance_policy)) NULL else .portfolio_submission_context(
        exchange,
        decision_bars = boundary_bars,
        agent_ids = unique(decision_rows$agent_id),
        allowed_assets_by_agent = allowed_assets_by_agent
      )
      decisions <- lapply(split(decision_rows, decision_rows$agent_id), function(rows) {
        agent_id <- as.character(rows$agent_id[1L])
        list(
          target_weights = stats::setNames(rows$target_weight, rows$symbol),
          allowed_symbols = as.character(allowed_symbols[[agent_id]]),
          .allowed_assets = allowed_assets_by_agent[[agent_id]],
          decision_label = if ("decision_label" %in% names(rows)) as.character(rows$decision_label[1L]) else "target_weight"
        )
      })
      # Historical feeds can have partial market calendars. Do not create a
      # multi-asset decision from an incomplete information boundary.
      eligible <- vapply(names(decisions), function(current_agent_id) {
        allowed_assets <- allowed_assets_by_agent[[current_agent_id]]
        tryCatch({
          .portfolio_require_decision_policy(
            exchange, boundary_bars, decisions[[current_agent_id]]$target_weights,
            allowed_assets, decision_policy
          )
          TRUE
        }, error = function(...) FALSE)
      }, logical(1L))
      decisions <- decisions[eligible]
      if (!is.null(rebalance_policy)) {
        keep <- vapply(names(decisions), function(current_agent_id) {
          rows <- decision_rows[decision_rows$agent_id == current_agent_id]
          .portfolio_rebalance_due(
            exchange = exchange,
            agent_id = current_agent_id,
            rows = rows,
            allowed_symbols = allowed_symbols[[current_agent_id]],
            policy = rebalance_policy,
            context = policy_context
          )
        }, logical(1L))
        decisions <- decisions[keep]
      }
      .sim_profile_add(exchange, "boundary_normalization", normalization_started)
      if (length(decisions)) {
        .portfolio_target_submit_batch_compact(exchange, boundary_bars, decisions, execution, decision_policy)
      }
    } else {
      .sim_profile_add(exchange, "boundary_normalization", normalization_started)
    }
    if (isTRUE(profile)) {
      boundary_elapsed <- proc.time()[["elapsed"]] - boundary_started
      timings$orchestration <- timings$orchestration + boundary_elapsed
      timings$boundary_latency_seconds <- c(timings$boundary_latency_seconds, boundary_elapsed)
    }
  }
  # Preserve the exact append ordering of incremental stepping while avoiding
  # repeated historical rbinds during the replay loop.
  exchange$step_snapshots <- data.table::rbindlist(accumulator$step_snapshots, fill = TRUE)
  exchange$step_events <- data.table::rbindlist(accumulator$step_events, fill = TRUE)
  exchange$last_events <- exchange$step_events
  exchange$result <- exchange$step_snapshots
  data.table::setattr(exchange$result, "market_events", exchange$market_events)
  data.table::setattr(exchange$result, "events", exchange$step_events)
  data.table::setattr(exchange$result, "orders", sim_orders(exchange$step_events))

  quality_started <- proc.time()[["elapsed"]]
  quality <- sim_portfolio_execution_quality(exchange)
  timings$execution_quality <- proc.time()[["elapsed"]] - quality_started
  timings$execution_quality_joins <- timings$execution_quality
  exports <- list()
  if (!is.null(export_path)) {
    export_started <- proc.time()[["elapsed"]]
    for (agent_id in agent_ids) {
      exports[[agent_id]] <- sim_portfolio_export(exchange, agent_id, file.path(export_path, agent_id))
    }
    timings$serialization_export <- proc.time()[["elapsed"]] - export_started
  }
  timings$wall_time <- proc.time()[["elapsed"]] - started
  if (isTRUE(profile)) {
    # The outer boundary timer includes the C++ and ledger portions; remove
    # them to report R-side batch construction/orchestration separately.
    timings$orchestration <- max(0, timings$orchestration - timings$portfolio_step_rcpp - timings$ledger)
  }
  list(
    exchange = exchange,
    orders = data.table::copy(exchange$agent_orders),
    fills = data.table::copy(exchange$portfolio_fills),
    positions = data.table::copy(sim_exchange_positions(exchange)),
    account = data.table::copy(sim_exchange_account(exchange)),
    targets = data.table::copy(exchange$portfolio_targets),
    rebalances = data.table::copy(exchange$portfolio_rebalances),
    quality = quality,
    exports = exports,
    timings = as.list(timings)
  )
}

#' @keywords internal
.portfolio_validate_rebalance_policy <- function(policy, panel_columns) {
  if (is.null(policy)) return(NULL)
  if (!is.list(policy)) stop("`rebalance_policy` must be NULL or a list.", call. = FALSE)
  unknown <- setdiff(names(policy), c("rebalance_due_column", "drift_tolerance", "target_tolerance"))
  if (length(unknown)) stop("Unknown `rebalance_policy` field(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  due_column <- as.character(policy$rebalance_due_column %||% "rebalance_due")
  if (length(due_column) != 1L || is.na(due_column) || !nzchar(due_column) || !due_column %in% panel_columns) {
    stop("`rebalance_policy$rebalance_due_column` must name a column in `target_weights`.", call. = FALSE)
  }
  drift_tolerance <- as.numeric(policy$drift_tolerance %||% 0)
  target_tolerance <- as.numeric(policy$target_tolerance %||% 1e-12)
  if (length(drift_tolerance) != 1L || !is.finite(drift_tolerance) || drift_tolerance < 0 ||
      length(target_tolerance) != 1L || !is.finite(target_tolerance) || target_tolerance < 0) {
    stop("`rebalance_policy` tolerances must be finite non-negative scalars.", call. = FALSE)
  }
  list(rebalance_due_column = due_column, drift_tolerance = drift_tolerance, target_tolerance = target_tolerance)
}

#' @keywords internal
.portfolio_rebalance_due <- function(exchange,
                                     agent_id,
                                     rows,
                                     allowed_symbols,
                                     policy,
                                     context) {
  requested_agent_id <- as.character(agent_id)
  due <- rows[[policy$rebalance_due_column]]
  if (is.logical(due)) {
    due <- as.logical(due)
  } else if (is.numeric(due)) {
    if (any(!is.finite(due) | !due %in% c(0, 1))) stop("`rebalance_due` must be logical or 0/1.", call. = FALSE)
    due <- as.logical(due)
  } else {
    stop("`rebalance_due` must be logical or 0/1.", call. = FALSE)
  }
  if (anyNA(due) || length(unique(due)) != 1L) {
    stop("Every agent/timestamp target group must have one non-missing `rebalance_due` value.", call. = FALSE)
  }
  if (!due[1L]) return(FALSE)

  symbols <- as.character(rows$symbol)
  desired <- as.numeric(rows$target_weight)
  prior <- exchange$portfolio_targets[
    agent_id == requested_agent_id & symbol %in% symbols
  ]
  changed <- TRUE
  if (nrow(prior)) {
    data.table::setorderv(prior, c("timestamp", "rebalance_id"))
    latest <- prior[, .SD[.N], by = symbol]
    index <- match(symbols, latest$symbol)
    changed <- any(is.na(index)) || any(abs(desired - latest$target_weight[index]) > policy$target_tolerance)
  }
  if (changed) return(TRUE)

  allowed_assets <- .portfolio_resolve_allowed_assets(exchange, agent_id, allowed_symbols = allowed_symbols)
  requested_asset_ids <- allowed_assets$asset_id[match(symbols, allowed_assets$symbol)]
  planning <- context$planning[agent_id == requested_agent_id & asset_id %in% requested_asset_ids]
  realized <- planning$realized_weight_before[match(requested_asset_ids, planning$asset_id)]
  any(!is.finite(realized) | abs(desired - realized) > policy$drift_tolerance)
}

#' @keywords internal
.sim_profile_start <- function(exchange) {
  if (!is.environment(exchange$.profile_timings %||% NULL)) return(NULL)
  proc.time()[["elapsed"]]
}

#' @keywords internal
.sim_profile_add <- function(exchange, category, started) {
  timings <- exchange$.profile_timings %||% NULL
  if (!is.environment(timings) || is.null(started)) return(invisible(NULL))
  timings[[category]] <- (timings[[category]] %||% 0) + (proc.time()[["elapsed"]] - started)
  invisible(NULL)
}
