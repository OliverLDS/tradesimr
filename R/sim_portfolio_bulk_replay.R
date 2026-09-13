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
#'   its panel rows.
#' @param execution Execution assumptions from [sim_portfolio_execution()].
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
                                        export_path = NULL,
                                        profile = FALSE) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  execution <- .portfolio_validate_execution(execution)
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

  accumulator <- new.env(parent = emptyenv())
  accumulator$step_snapshots <- list(data.table::copy(exchange$step_snapshots))
  accumulator$step_events <- list(data.table::copy(exchange$step_events))
  timings <- new.env(parent = emptyenv())
  timings$orchestration <- 0
  timings$portfolio_step_rcpp <- 0
  timings$ledger <- 0
  timings$execution_quality <- 0
  timings$serialization_export <- 0
  exchange$.bulk_accumulator <- accumulator
  exchange$.profile_timings <- if (isTRUE(profile)) timings else NULL
  on.exit({
    exchange$.bulk_accumulator <- NULL
    exchange$.profile_timings <- NULL
  }, add = TRUE)

  started <- proc.time()[["elapsed"]]
  for (boundary_timestamp in unique(bars$timestamp)) {
    boundary_started <- proc.time()[["elapsed"]]
    boundary_bars <- bars[as.numeric(timestamp) == as.numeric(boundary_timestamp)]
    sim_portfolio_market_step(exchange, boundary_bars, execution)
    decision_rows <- panel[as.numeric(timestamp) == as.numeric(boundary_timestamp)]
    if (nrow(decision_rows)) {
      decisions <- lapply(split(decision_rows, decision_rows$agent_id), function(rows) {
        list(
          target_weights = stats::setNames(rows$target_weight, rows$symbol),
          allowed_symbols = as.character(allowed_symbols[[as.character(rows$agent_id[1L])]]),
          decision_label = if ("decision_label" %in% names(rows)) as.character(rows$decision_label[1L]) else "target_weight"
        )
      })
      sim_portfolio_target_submit_batch(exchange, boundary_bars, decisions, execution)
    }
    if (isTRUE(profile)) {
      timings$orchestration <- timings$orchestration + (proc.time()[["elapsed"]] - boundary_started)
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
