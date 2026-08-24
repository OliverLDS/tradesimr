#' Build execution assumptions for a target-weight portfolio replay
#'
#' @param timing Execution timing. Phase 1 supports only
#'   `"next_eligible_open"`: a decision made from a completed bar can first fill
#'   on a strictly later bar for the same asset.
#' @param fee_rt Taker fee rate applied to filled notional.
#' @param maker_fee_rt Optional maker fee rate for future limit-order support.
#' @param slippage Absolute adverse price adjustment per unit.
#' @param spread Absolute bid/ask spread; half is applied adversely on fills.
#' @param lev Leverage used for initial-margin checks.
#' @param mmr Maintenance-margin rate.
#' @param max_gross_weight Maximum sum of absolute target weights.
#' @return A named execution configuration list.
#' @export
sim_portfolio_execution <- function(timing = "next_eligible_open",
                                    fee_rt = 0,
                                    maker_fee_rt = NA_real_,
                                    slippage = 0,
                                    spread = 0,
                                    lev = 1,
                                    mmr = 0.02,
                                    max_gross_weight = 1) {
  timing <- match.arg(timing, "next_eligible_open")
  values <- c(fee_rt = fee_rt, slippage = slippage, spread = spread, lev = lev, mmr = mmr, max_gross_weight = max_gross_weight)
  if (any(!is.finite(values)) || fee_rt < 0 || slippage < 0 || spread < 0 || lev <= 0 || mmr < 0 || max_gross_weight < 0) {
    stop("Execution assumptions must be finite; fees/costs must be non-negative and leverage positive.", call. = FALSE)
  }
  list(
    timing = timing,
    fee_rt = as.numeric(fee_rt),
    maker_fee_rt = as.numeric(maker_fee_rt),
    slippage = as.numeric(slippage),
    spread = as.numeric(spread),
    lev = as.numeric(lev),
    mmr = as.numeric(mmr),
    max_gross_weight = as.numeric(max_gross_weight)
  )
}

#' Advance an Arena exchange at one completed market boundary
#'
#' This API advances a timestamped batch of completed bars exactly once. It
#' executes only orders submitted at earlier boundaries; it never creates a
#' target decision or rebalance. Each supplied asset bar must be genuinely new,
#' so duplicate and stale batches fail rather than being silently reprocessed.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars One timestamped batch of registered, completed OHLC bars.
#' @param execution Execution assumptions from `sim_portfolio_execution()`.
#' @return A public-safe list with `bars`, `fills`, `events`, `positions`,
#'   `account`, and `outcomes`.
#' @export
sim_portfolio_market_step <- function(exchange,
                                      bars,
                                      execution = sim_portfolio_execution()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  execution <- .portfolio_validate_execution(execution)
  boundary_bars <- .portfolio_validate_decision_bars(exchange, bars)
  .portfolio_require_one_timestamp(boundary_bars)
  .portfolio_require_new_bars(exchange, boundary_bars)
  .portfolio_apply_execution_config(exchange, execution)

  sim_exchange_step(exchange, boundary_bars)
  exchange$portfolio_market_boundaries <- data.table::rbindlist(list(
    exchange$portfolio_market_boundaries,
    boundary_bars[, .(timestamp, symbol, asset_id)]
  ), fill = TRUE)
  fills <- .portfolio_fills_for_events(exchange, exchange$new_events)
  list(
    timestamp = boundary_bars$timestamp[1L],
    bars = data.table::copy(boundary_bars),
    fills = fills,
    events = data.table::copy(exchange$new_events),
    positions = data.table::copy(sim_exchange_positions(exchange)),
    account = data.table::copy(sim_exchange_account(exchange)),
    outcomes = data.table::data.table(
      timestamp = boundary_bars$timestamp[1L],
      symbol = boundary_bars$symbol,
      asset_id = boundary_bars$asset_id,
      status = "market_stepped",
      message = "Completed market bars accepted; only earlier eligible orders were executed."
    )
  )
}

#' Submit one Arena target-weight decision after a market boundary
#'
#' The supplied decision bars must already have been accepted by
#' `sim_portfolio_market_step()`. Targets are converted atomically using the
#' completed-bar close and the post-step account state. Submitted orders are
#' eligible only on a strictly later bar of the same asset. `NULL` records a
#' durable no-decision outcome and preserves current positions.
#' Target-derived opening and increasing orders are clipped down to the largest
#' executable contract-step quantity when fees or shared portfolio margin make
#' the exact target infeasible. Explicit contract orders remain all-or-nothing.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Account identifier. Each agent has an isolated account.
#' @param bars The already accepted timestamped completed-bar batch.
#' @param target_weights Optional named numeric target weights keyed by
#'   registered symbols.
#' @param execution Execution assumptions from `sim_portfolio_execution()`.
#' @param decision_label Optional durable label for the decision source.
#' @param allowed_symbols Optional registered symbols this agent may target,
#'   hold, or trade. Persisted on the agent after a successful submission.
#' @param allowed_asset_ids Optional registered asset ids this agent may target,
#'   hold, or trade. When supplied with `allowed_symbols`, both must identify
#'   the same allowed universe.
#' @return A list containing `orders`, `fills`, `positions`, `account`,
#'   `targets`, `realized_weights`, and `outcomes`.
#' @export
sim_portfolio_target_submit <- function(exchange,
                                        agent_id,
                                        bars,
                                        target_weights = NULL,
                                        execution = sim_portfolio_execution(),
                                        decision_label = "target_weight",
                                        allowed_symbols = NULL,
                                        allowed_asset_ids = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  agent_id <- as.character(agent_id)
  if (!nzchar(agent_id)) stop("`agent_id` is required.", call. = FALSE)
  execution <- .portfolio_validate_execution(execution)
  decision_bars <- .portfolio_validate_decision_bars(exchange, bars)
  .portfolio_require_one_timestamp(decision_bars)
  .portfolio_require_accepted_boundary(exchange, decision_bars)
  .portfolio_apply_execution_config(exchange, execution)
  first_asset <- .bar_asset_key(decision_bars[1L])
  .ensure_agent_account(exchange, agent_id, asset_id = first_asset$asset_id, symbol = first_asset$symbol, agent_type = "arena")
  allowed_assets <- .portfolio_resolve_allowed_assets(exchange, agent_id, allowed_symbols, allowed_asset_ids)
  .portfolio_set_agent_universe(exchange, agent_id, allowed_assets$asset_id)
  .portfolio_submit_target(
    exchange, agent_id, decision_bars, target_weights, execution, decision_label,
    fills = sim_schemas()$events[0], allowed_assets = allowed_assets,
    context = .portfolio_submission_context(exchange)
  )
}

#' Submit multiple Arena target-weight decisions after one market boundary
#'
#' This is the efficient replay interface for a common completed market
#' boundary. It does not step market data. Instead, it validates the accepted
#' boundary once and snapshots account, position, and carried-price state once
#' before translating each agent's decision atomically. Each `decisions`
#' element is a named list containing `target_weights` (or `NULL` for a
#' no-decision), and optionally `decision_label`, `allowed_symbols`, and
#' `allowed_asset_ids`.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars The already accepted timestamped completed-bar batch.
#' @param decisions A named list keyed by `agent_id`.
#' @param execution Execution assumptions from `sim_portfolio_execution()`.
#' @return A list with the common boundary `timestamp`, public account and
#'   position snapshots, and a named `submissions` list containing the same
#'   result contract as `sim_portfolio_target_submit()` for each agent.
#' @export
sim_portfolio_target_submit_batch <- function(exchange,
                                              bars,
                                              decisions,
                                              execution = sim_portfolio_execution()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (!is.list(decisions) || is.null(names(decisions)) || any(!nzchar(names(decisions))) || anyDuplicated(names(decisions))) {
    stop("`decisions` must be a named list keyed by unique agent ids.", call. = FALSE)
  }
  execution <- .portfolio_validate_execution(execution)
  decision_bars <- .portfolio_validate_decision_bars(exchange, bars)
  .portfolio_require_one_timestamp(decision_bars)
  .portfolio_require_accepted_boundary(exchange, decision_bars)
  .portfolio_apply_execution_config(exchange, execution)
  prepared <- vector("list", length(decisions))
  names(prepared) <- names(decisions)
  for (i in seq_along(decisions)) {
    agent_id <- names(decisions)[i]
    decision <- decisions[[i]]
    if (is.null(decision)) decision <- list(target_weights = NULL)
    if (!is.list(decision)) {
      stop("Each `decisions` element must be a list or NULL.", call. = FALSE)
    }
    unknown <- setdiff(names(decision), c("target_weights", "decision_label", "allowed_symbols", "allowed_asset_ids"))
    if (length(unknown)) stop("Unknown decision field(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    first_asset <- .bar_asset_key(decision_bars[1L])
    .ensure_agent_account(exchange, agent_id, asset_id = first_asset$asset_id, symbol = first_asset$symbol, agent_type = "arena")
    allowed_assets <- .portfolio_resolve_allowed_assets(
      exchange, agent_id, decision$allowed_symbols %||% NULL, decision$allowed_asset_ids %||% NULL
    )
    .portfolio_set_agent_universe(exchange, agent_id, allowed_assets$asset_id)
    prepared[[i]] <- list(agent_id = agent_id, decision = decision, allowed_assets = allowed_assets)
  }
  context <- .portfolio_submission_context(exchange)
  submissions <- vector("list", length(decisions))
  names(submissions) <- names(decisions)
  for (i in seq_along(prepared)) {
    agent_id <- prepared[[i]]$agent_id
    decision <- prepared[[i]]$decision
    submissions[[i]] <- .portfolio_submit_target(
      exchange = exchange,
      agent_id = agent_id,
      decision_bars = decision_bars,
      target_weights = decision$target_weights %||% NULL,
      execution = execution,
      decision_label = decision$decision_label %||% "target_weight",
      fills = sim_schemas()$events[0],
      allowed_assets = prepared[[i]]$allowed_assets,
      context = context
    )
  }
  list(
    timestamp = decision_bars$timestamp[1L],
    positions = data.table::copy(context$positions),
    account = data.table::copy(context$account),
    submissions = submissions
  )
}

#' Step one agent portfolio from target weights
#'
#' This is the stable Vox Arena integration point. It accepts a batch of
#' genuinely new completed OHLC bars and, optionally, an explicit target-weight
#' decision. Existing eligible orders are stepped first. A new target decision
#' is then translated atomically into contract orders using current account
#' equity and the decision-bar close (or the latest carried valuation for an
#' asset absent from the batch). New orders are eligible only on a strictly
#' later bar for their own asset. At that later fill boundary, target-derived
#' opening/increase orders are capped to the largest step-rounded quantity whose
#' fee and initial margin fit the account. Consequently a target weight of `1`
#' at `lev = 1` is near 100% notional after reserving the fee, rather than a
#' failed all-cash order. Explicit contract orders retain reject-on-insufficient
#' margin semantics.
#'
#' Missing target weights for active registered assets mean a target weight of
#' zero. A `NULL` target is a no-decision: positions are retained and no new
#' orders are submitted. Repeated/stale bars are ignored, so closed markets can
#' retain their last valuation without creating strategy reactions or fills.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Account identifier. Each agent has an isolated account.
#' @param bars One timestamped batch of registered, completed OHLC bars.
#' @param target_weights Optional named numeric target weights keyed by
#'   registered symbols.
#' @param execution Execution assumptions from `sim_portfolio_execution()`.
#' @param decision_label Optional durable label for the decision source.
#' @param allowed_symbols Optional registered symbols this agent may target,
#'   hold, or trade.
#' @param allowed_asset_ids Optional registered asset ids this agent may target,
#'   hold, or trade.
#' @return A list containing `orders`, `fills`, `positions`, `account`,
#'   `targets`, `realized_weights`, and `outcomes`.
#' @export
sim_portfolio_target_step <- function(exchange,
                                      agent_id,
                                      bars,
                                      target_weights = NULL,
                                      execution = sim_portfolio_execution(),
                                      decision_label = "target_weight",
                                      allowed_symbols = NULL,
                                      allowed_asset_ids = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  agent_id <- as.character(agent_id)
  if (!nzchar(agent_id)) stop("`agent_id` is required.", call. = FALSE)
  execution <- .portfolio_validate_execution(execution)
  decision_bars <- .portfolio_validate_decision_bars(exchange, bars)
  new_bars <- .portfolio_new_completed_bars(exchange, decision_bars)
  if (nrow(new_bars) == 0L && is.null(target_weights)) {
    return(.portfolio_step_result(exchange, agent_id, outcomes = .portfolio_outcome_row(
      rebalance_id = NA_character_, timestamp = as.POSIXct(NA), agent_id = agent_id,
      status = "no_new_bar", message = "No genuinely new completed bars were supplied."
    )))
  }
  if (length(unique(decision_bars$timestamp)) != 1L) {
    stop("`bars` must be one timestamped batch.", call. = FALSE)
  }
  .portfolio_require_one_timestamp(decision_bars)
  if (nrow(new_bars) > 0L) {
    market <- sim_portfolio_market_step(exchange, new_bars, execution)
    result <- sim_portfolio_target_submit(exchange, agent_id, decision_bars, target_weights, execution, decision_label, allowed_symbols, allowed_asset_ids)
    # Preserve the combined API's historical behavior: a no-decision step
    # reports fills caused by older orders at this market boundary.
    if (is.null(target_weights)) result$fills <- .portfolio_fills_for_events(exchange, market$events, agent_id)
    return(result)
  }
  if (is.null(target_weights)) {
    return(.portfolio_step_result(exchange, agent_id, outcomes = .portfolio_outcome_row(
      rebalance_id = NA_character_, timestamp = as.POSIXct(NA), agent_id = agent_id,
      status = "no_new_bar", message = "No genuinely new completed bars were supplied."
    )))
  }
  sim_portfolio_target_submit(exchange, agent_id, decision_bars, target_weights, execution, decision_label, allowed_symbols, allowed_asset_ids)
}

#' @keywords internal
.portfolio_submit_target <- function(exchange,
                                     agent_id,
                                     decision_bars,
                                     target_weights,
                                     execution,
                                     decision_label,
                                     fills,
                                     allowed_assets,
                                     context = NULL) {
  if (is.null(target_weights)) {
    return(.portfolio_step_result(exchange, agent_id, fills = fills, context = context, outcomes = .portfolio_outcome_row(
      rebalance_id = NA_character_, timestamp = decision_bars$timestamp[1L], agent_id = agent_id,
      status = "no_decision", message = "No target-weight decision; prior positions were retained."
    )))
  }

  timestamp <- decision_bars$timestamp[1L]
  rebalance_id <- paste0("RB", sprintf("%06d", exchange$next_rebalance_id %||% 1L))
  exchange$next_rebalance_id <- as.integer(exchange$next_rebalance_id %||% 1L) + 1L
  targets <- tryCatch(
    .portfolio_normalize_targets(exchange, target_weights, execution$max_gross_weight, allowed_assets),
    error = function(error) error
  )
  if (inherits(targets, "error")) {
    message <- conditionMessage(targets)
    exchange$portfolio_rebalances <- data.table::rbindlist(list(exchange$portfolio_rebalances, data.table::data.table(
      rebalance_id = rebalance_id, timestamp = timestamp, agent_id = agent_id,
      status = "rejected", execution_timing = execution$timing, fee_rt = execution$fee_rt,
      slippage = execution$slippage, spread = execution$spread, message = message
    )), fill = TRUE)
    return(.portfolio_step_result(exchange, agent_id, rebalance_id, fills = fills, context = context, outcomes = .portfolio_outcome_row(
      rebalance_id, timestamp, agent_id, "rejected", message
    )))
  }
  plan <- .portfolio_rebalance_plan(exchange, agent_id, targets, decision_bars, execution, allowed_assets, context = context)
  target_rows <- plan$targets[, .(
    rebalance_id,
    timestamp,
    eligible_after = timestamp,
    agent_id,
    symbol,
    asset_id,
    target_weight,
    realized_weight_before,
    decision_equity,
    planned_signed_quantity = desired_signed_qty,
    decision_price,
    status = outcome_status,
    message = outcome_message
  )]
  exchange$portfolio_targets <- data.table::rbindlist(list(exchange$portfolio_targets, target_rows), fill = TRUE)

  if (nrow(plan$orders) == 0L) {
    exchange$portfolio_rebalances <- data.table::rbindlist(list(exchange$portfolio_rebalances, data.table::data.table(
      rebalance_id = rebalance_id, timestamp = timestamp, agent_id = agent_id,
      status = "no_op", execution_timing = execution$timing, fee_rt = execution$fee_rt,
      slippage = execution$slippage, spread = execution$spread,
      message = "All target quantities already match the current portfolio."
    )), fill = TRUE)
    return(.portfolio_step_result(exchange, agent_id, rebalance_id, fills = fills, context = context, outcomes = .portfolio_outcome_row(
      rebalance_id, timestamp, agent_id, "no_op", "No rebalance was required."
    )))
  }

  # Plan validation happened before this append: accepted rows appear together or not at all.
  order_rows <- .portfolio_order_rows(exchange, agent_id, rebalance_id, timestamp, plan$orders, execution)
  exchange$agent_orders <- data.table::rbindlist(list(exchange$agent_orders, order_rows), fill = TRUE)
  exchange$portfolio_rebalances <- data.table::rbindlist(list(exchange$portfolio_rebalances, data.table::data.table(
    rebalance_id = rebalance_id, timestamp = timestamp, agent_id = agent_id,
    status = "accepted", execution_timing = execution$timing, fee_rt = execution$fee_rt,
    slippage = execution$slippage, spread = execution$spread,
    message = as.character(decision_label)
  )), fill = TRUE)
  exchange$event_log <- data.table::rbindlist(list(exchange$event_log, data.table::data.table(
    timestamp = timestamp, source = "portfolio_rebalance", event = "accepted", ref_id = rebalance_id
  )), fill = TRUE)
  .portfolio_step_result(exchange, agent_id, rebalance_id, fills = fills, context = context, outcomes = plan$targets[, .(
    rebalance_id, timestamp, agent_id, symbol, asset_id, status = outcome_status, message = outcome_message
  )])
}

#' Export a safe portfolio replay snapshot for an external consumer
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Agent identifier.
#' @param path Output directory.
#' @param format Export format. Phase 1 supports JSON.
#' @return A named vector of written paths. `fills.json` is sourced from the
#'   durable portfolio fill ledger and links every filled portfolio order to
#'   its `order_id` and `rebalance_id`; `rebalances.json` contains the linked
#'   accepted/rejected rebalance records.
#' @export
sim_portfolio_export <- function(exchange,
                                 agent_id,
                                 path,
                                 format = "json") {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  format <- match.arg(format, "json")
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required for JSON portfolio export.", call. = FALSE)
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  snapshot <- .portfolio_step_result(exchange, as.character(agent_id))
  tables <- list(
    orders = snapshot$orders,
    fills = snapshot$fills,
    positions = snapshot$positions,
    valuations = snapshot$valuations,
    account = snapshot$account,
    targets = snapshot$targets,
    rebalances = snapshot$rebalances,
    realized_weights = snapshot$realized_weights
  )
  paths <- vapply(names(tables), function(name) {
    file <- file.path(path, paste0(name, ".json"))
    jsonlite::write_json(tables[[name]], file, dataframe = "rows", na = "null", auto_unbox = TRUE, pretty = TRUE)
    file
  }, character(1L))
  paths
}

#' @keywords internal
.portfolio_validate_execution <- function(execution) {
  if (!is.list(execution)) stop("`execution` must be created by sim_portfolio_execution().", call. = FALSE)
  required <- names(formals(sim_portfolio_execution))
  missing <- setdiff(required, names(execution))
  if (length(missing)) stop("Missing execution setting(s): ", paste(missing, collapse = ", "), call. = FALSE)
  do.call(sim_portfolio_execution, execution[required])
}

#' @keywords internal
.portfolio_apply_execution_config <- function(exchange, execution) {
  existing <- exchange$config$portfolio_execution %||% NULL
  comparable <- c("timing", "fee_rt", "maker_fee_rt", "slippage", "spread", "lev", "mmr", "max_gross_weight")
  if (!is.null(existing) && !identical(unname(existing[comparable]), unname(execution[comparable]))) {
    stop("Portfolio execution assumptions are already locked on this exchange. Create a new exchange for different assumptions.", call. = FALSE)
  }
  exchange$config$portfolio_execution <- execution
  exchange$config$fee_rt <- execution$fee_rt
  exchange$config$maker_fee_rt <- execution$maker_fee_rt
  exchange$config$taker_fee_rt <- execution$fee_rt
  exchange$config$slippage <- execution$slippage
  exchange$config$spread <- execution$spread
  exchange$config$lev <- execution$lev
  exchange$config$mmr <- execution$mmr
  invisible(exchange)
}

#' @keywords internal
.portfolio_new_completed_bars <- function(exchange, bars) {
  bars <- .portfolio_validate_decision_bars(exchange, bars)
  if (nrow(bars) == 0L) return(bars)
  if (anyDuplicated(paste(bars$asset_id, bars$timestamp))) {
    stop("`bars` must contain at most one completed bar per asset/timestamp.", call. = FALSE)
  }
  prior <- exchange$market_events
  keep <- vapply(seq_len(nrow(bars)), function(i) {
    old <- prior[asset_id == bars$asset_id[i], timestamp]
    !length(old) || bars$timestamp[i] > max(old)
  }, logical(1L))
  bars[keep]
}

#' @keywords internal
.portfolio_require_one_timestamp <- function(bars) {
  if (nrow(bars) == 0L || length(unique(bars$timestamp)) != 1L) {
    stop("`bars` must be a non-empty, one timestamped batch.", call. = FALSE)
  }
  invisible(bars)
}

#' @keywords internal
.portfolio_require_new_bars <- function(exchange, bars) {
  prior <- exchange$market_events
  stale <- vapply(seq_len(nrow(bars)), function(i) {
    old <- prior[asset_id == bars$asset_id[i], timestamp]
    length(old) && bars$timestamp[i] <= max(old)
  }, logical(1L))
  if (any(stale)) {
    labels <- paste0(bars$symbol[stale], "@", format(bars$timestamp[stale], tz = "UTC"))
    stop("Market boundary contains duplicate or stale completed bar(s): ", paste(labels, collapse = ", "), call. = FALSE)
  }
  invisible(bars)
}

#' @keywords internal
.portfolio_require_accepted_boundary <- function(exchange, bars) {
  boundaries <- exchange$portfolio_market_boundaries %||% sim_schemas()$portfolio_market_boundaries[0]
  supplied <- paste(bars$asset_id, as.numeric(bars$timestamp), sep = "|")
  accepted <- paste(boundaries$asset_id, as.numeric(boundaries$timestamp), sep = "|")
  if (!all(supplied %in% accepted)) {
    stop("The decision bars have not been accepted by `sim_portfolio_market_step()`.", call. = FALSE)
  }
  invisible(bars)
}

#' @keywords internal
.portfolio_validate_decision_bars <- function(exchange, bars) {
  bars <- as_market_bars(bars)
  bars <- .validate_market_bar_assets(exchange, bars)
  if (nrow(bars) == 0L) return(bars)
  if (anyDuplicated(paste(bars$asset_id, bars$timestamp))) {
    stop("`bars` must contain at most one completed bar per asset/timestamp.", call. = FALSE)
  }
  bars
}

#' @keywords internal
.portfolio_normalize_targets <- function(exchange, target_weights, max_gross_weight, allowed_assets) {
  if (is.null(names(target_weights)) || any(!nzchar(names(target_weights)))) {
    stop("`target_weights` must be a named numeric vector keyed by registered symbols.", call. = FALSE)
  }
  weights <- as.numeric(target_weights)
  names(weights) <- names(target_weights)
  if (any(!is.finite(weights)) || anyDuplicated(names(weights))) {
    stop("`target_weights` must contain finite, uniquely named values.", call. = FALSE)
  }
  unknown <- setdiff(names(weights), allowed_assets$symbol)
  if (length(unknown)) stop("Target symbol(s) are outside the agent allowed universe: ", paste(unknown, collapse = ", "), call. = FALSE)
  out <- data.table::copy(allowed_assets[, .(symbol, asset_id, contract_size, qty_step)])
  out[, target_weight := weights[symbol]]
  out[is.na(target_weight), target_weight := 0]
  if (sum(abs(out$target_weight)) > max_gross_weight + 1e-10) {
    stop("Absolute target weights exceed `max_gross_weight`.", call. = FALSE)
  }
  out[]
}

#' @keywords internal
.portfolio_resolve_allowed_assets <- function(exchange,
                                              agent_id,
                                              allowed_symbols = NULL,
                                              allowed_asset_ids = NULL) {
  requested_agent_id <- as.character(agent_id)
  assets <- sim_assets(exchange)[status == "active"]
  if (!is.null(allowed_symbols)) {
    allowed_symbols <- unique(as.character(allowed_symbols))
    if (!length(allowed_symbols) || any(!nzchar(allowed_symbols))) stop("`allowed_symbols` must contain registered symbols.", call. = FALSE)
    unknown <- setdiff(allowed_symbols, assets$symbol)
    if (length(unknown)) stop("Unknown or inactive allowed symbol(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    by_symbol <- assets[assets$symbol %in% allowed_symbols]
  } else {
    by_symbol <- NULL
  }
  if (!is.null(allowed_asset_ids)) {
    allowed_asset_ids <- unique(as.integer(allowed_asset_ids))
    if (!length(allowed_asset_ids) || anyNA(allowed_asset_ids)) stop("`allowed_asset_ids` must contain registered asset ids.", call. = FALSE)
    unknown <- setdiff(allowed_asset_ids, assets$asset_id)
    if (length(unknown)) stop("Unknown or inactive allowed asset id(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    by_id <- assets[assets$asset_id %in% allowed_asset_ids]
  } else {
    by_id <- NULL
  }
  if (!is.null(by_symbol) && !is.null(by_id) && !setequal(by_symbol$asset_id, by_id$asset_id)) {
    stop("`allowed_symbols` and `allowed_asset_ids` must identify the same allowed universe.", call. = FALSE)
  }
  explicit <- by_symbol %||% by_id
  if (!is.null(explicit)) return(data.table::copy(explicit))

  agent <- exchange$agents[exchange$agents$agent_id == requested_agent_id]
  if (nrow(agent) && "config" %in% names(agent)) {
    config <- .agent_config_decode(agent$config[1L])
    persisted_value <- as.character(config$portfolio_allowed_asset_ids %||% "")
    persisted_ids <- if (nzchar(persisted_value)) as.integer(strsplit(persisted_value, ",", fixed = TRUE)[[1L]]) else integer()
    if (length(persisted_ids)) return(data.table::copy(assets[assets$asset_id %in% persisted_ids]))
  }
  data.table::copy(assets)
}

#' @keywords internal
.portfolio_set_agent_universe <- function(exchange, agent_id, asset_ids) {
  index <- match(as.character(agent_id), exchange$agents$agent_id)
  if (is.na(index)) stop("Cannot persist an allowed universe for an unknown agent.", call. = FALSE)
  config <- .agent_config_decode(exchange$agents$config[index])
  keys <- names(exchange$agent_states)
  for (key in keys) {
    parsed <- .parse_agent_state_key(key)
    if (!identical(parsed$agent_id, as.character(agent_id)) || parsed$asset_id %in% asset_ids) next
    state <- exchange$agent_states[[key]]
    if (abs(as.numeric(state$ctr_unit %||% 0)) > 0) {
      stop("Agent has a non-zero position outside its proposed allowed universe.", call. = FALSE)
    }
    exchange$agent_states[[key]] <- NULL
  }
  config$portfolio_allowed_asset_ids <- paste(sort(unique(as.integer(asset_ids))), collapse = ",")
  data.table::set(exchange$agents, i = index, j = "config", value = .agent_config_encode(config))
  if (!is.null(exchange$portfolio_allowed_asset_cache)) {
    exchange$portfolio_allowed_asset_cache[[as.character(agent_id)]] <- NULL
  }
  invisible(exchange)
}

#' @keywords internal
.portfolio_agent_asset_allowed <- function(exchange, agent_id, asset_id) {
  requested_agent_id <- as.character(agent_id)
  active_ids <- as.integer(exchange$assets$asset_id[exchange$assets$status == "active"])
  index <- match(requested_agent_id, exchange$agents$agent_id)
  config <- if (is.na(index)) "" else as.character(exchange$agents$config[index] %||% "")
  cache <- exchange$portfolio_allowed_asset_cache %||% list()
  cached <- cache[[requested_agent_id]]
  if (is.null(cached) || !identical(cached$config, config) || !identical(cached$active_ids, active_ids)) {
    decoded <- .agent_config_decode(config)
    value <- as.character(decoded$portfolio_allowed_asset_ids %||% "")
    allowed_ids <- if (nzchar(value)) as.integer(strsplit(value, ",", fixed = TRUE)[[1L]]) else active_ids
    cached <- list(config = config, active_ids = active_ids, allowed_ids = allowed_ids)
    cache[[requested_agent_id]] <- cached
    exchange$portfolio_allowed_asset_cache <- cache
  }
  as.integer(asset_id) %in% cached$allowed_ids
}

#' @keywords internal
.portfolio_rebalance_plan <- function(exchange, agent_id, targets, bars, execution, allowed_assets, context = NULL) {
  requested_agent_id <- as.character(agent_id)
  if (!all(targets$asset_id %in% allowed_assets$asset_id)) {
    stop("Portfolio rebalance plan contains an asset outside the agent allowed universe.", call. = FALSE)
  }
  account <- if (is.null(context)) sim_exchange_account(exchange) else context$account
  equity <- if (nrow(account) && "equity" %in% names(account)) account[account$agent_id == requested_agent_id, equity] else numeric()
  equity <- if (length(equity)) as.numeric(equity[1L]) else as.numeric(exchange$config$cash %||% exchange$config$init_cash %||% 100000)
  if (!is.finite(equity) || equity <= 0) stop("Agent account equity must be positive to translate target weights.", call. = FALSE)
  positions <- if (is.null(context)) sim_exchange_positions(exchange) else context$positions
  if (nrow(positions) > 0L && "agent_id" %in% names(positions)) {
    positions <- positions[positions$agent_id == requested_agent_id]
  }
  if (nrow(positions) && any(!positions$asset_id %in% allowed_assets$asset_id & abs(positions$ctr_unit) > 0)) {
    stop("Agent has a non-zero position outside its allowed universe.", call. = FALSE)
  }
  latest <- data.table::copy(targets)
  latest[, decision_price := vapply(seq_len(.N), function(i) {
    bar_price <- bars[asset_id == latest$asset_id[i], close]
    if (length(bar_price)) return(as.numeric(bar_price[1L]))
    pos_price <- if (nrow(positions) && "asset_id" %in% names(positions)) positions[positions$asset_id == latest$asset_id[i], last_px] else numeric()
    if (length(pos_price)) return(as.numeric(pos_price[1L]))
    market_events <- if (is.null(context)) exchange$market_events else context$market_events
    historical <- market_events[asset_id == latest$asset_id[i], close]
    if (length(historical)) return(as.numeric(tail(historical, 1L)))
    NA_real_
  }, numeric(1L))]
  if (any(!is.finite(latest$decision_price) | latest$decision_price <= 0)) {
    missing <- latest$symbol[!is.finite(latest$decision_price) | latest$decision_price <= 0]
    stop("Cannot value target symbol(s) without a completed or carried price: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  latest[, desired_signed_qty := round((target_weight * equity / (decision_price * contract_size)) / qty_step) * qty_step]
  latest[, decision_equity := equity]
  latest[, current_signed_qty := vapply(asset_id, function(requested_asset_id) {
    row <- if (nrow(positions) && "asset_id" %in% names(positions)) positions[positions$asset_id == requested_asset_id] else positions[0]
    if (!nrow(row)) return(0)
    as.numeric(row$pos_dir[1L] * row$ctr_unit[1L])
  }, numeric(1L))]
  latest[, realized_weight_before := vapply(seq_len(.N), function(i) {
    row <- if (nrow(positions) && "asset_id" %in% names(positions)) positions[positions$asset_id == latest$asset_id[i]] else positions[0]
    if (!nrow(row) || equity <= 0) return(0)
    as.numeric(row$notional[1L] / equity)
  }, numeric(1L))]
  latest[, delta_qty := desired_signed_qty - current_signed_qty]
  latest[, `:=`(outcome_status = data.table::fifelse(abs(delta_qty) <= qty_step / 2, "no_op", "accepted"), outcome_message = data.table::fifelse(abs(delta_qty) <= qty_step / 2, "Target already matches rounded contract quantity.", "Target translated to executable contract actions."))]
  orders <- data.table::rbindlist(lapply(seq_len(nrow(latest)), function(i) .portfolio_asset_actions(latest[i])), fill = TRUE)
  list(targets = latest, orders = orders)
}

#' @keywords internal
.portfolio_asset_actions <- function(row) {
  current <- as.numeric(row$current_signed_qty)
  desired <- as.numeric(row$desired_signed_qty)
  eps <- abs(as.numeric(row$qty_step)) / 2
  if (abs(desired - current) <= eps) return(data.table::data.table())
  direction <- function(x) if (x > 0) "long" else if (x < 0) "short" else "flat"
  make <- function(action, dir, qty) data.table::data.table(
    symbol = row$symbol, asset_id = row$asset_id, action = action, dir = dir,
    qty = abs(as.numeric(qty)), target_weight = row$target_weight,
    decision_price = row$decision_price
  )
  if (abs(current) <= eps) return(make("open", direction(desired), desired))
  if (abs(desired) <= eps) return(make("close", "flat", current))
  if (sign(current) == sign(desired)) {
    if (abs(desired) > abs(current)) return(make("increase", direction(desired), desired - current))
    return(make("reduce", direction(current), current - desired))
  }
  data.table::rbindlist(list(make("close", "flat", current), make("open", direction(desired), desired)), fill = TRUE)
}

#' @keywords internal
.portfolio_order_rows <- function(exchange, agent_id, rebalance_id, timestamp, orders, execution) {
  if (!nrow(orders)) return(exchange$agent_orders[0])
  ids <- paste0("ORD", sprintf("%06d", seq.int(exchange$next_order_id, length.out = nrow(orders))))
  exchange$next_order_id <- exchange$next_order_id + nrow(orders)
  data.table::data.table(
    order_id = ids,
    client_order_id = NA_character_,
    agent_id = agent_id,
    symbol = orders$symbol,
    asset_id = as.integer(orders$asset_id),
    timestamp = timestamp,
    eligible_after = timestamp,
    settlement_timestamp = as.POSIXct(NA, tz = "UTC"),
    rebalance_id = rebalance_id,
    target_weight = as.numeric(orders$target_weight),
    decision_price = as.numeric(orders$decision_price),
    order_type = "market",
    side = data.table::fifelse(orders$dir == "long", "buy", data.table::fifelse(orders$dir == "short", "sell", "flat")),
    intended_action = orders$action,
    intended_dir = orders$dir,
    qty_type = "contracts",
    qty = as.numeric(orders$qty),
    limit_price = NA_real_,
    time_in_force = "next_eligible_bar",
    tgt_pos = NA_real_,
    tol_pos = 0,
    status = "accepted",
    price = NA_real_,
    fee = NA_real_,
    realized_pnl = NA_real_,
    reason_code = NA_character_,
    message = NA_character_
  )
}

#' @keywords internal
.portfolio_outcome_row <- function(rebalance_id, timestamp, agent_id, status, message) {
  data.table::data.table(rebalance_id = rebalance_id, timestamp = timestamp, agent_id = agent_id, symbol = NA_character_, asset_id = NA_integer_, status = status, message = message)
}

#' @keywords internal
.portfolio_fills_for_events <- function(exchange, events, agent_id = NULL) {
  fills <- data.table::copy(exchange$portfolio_fills)
  requested_agent_id <- as.character(agent_id)
  if (!is.null(agent_id)) fills <- fills[fills$agent_id == requested_agent_id]
  if (is.null(events) || !nrow(events) || !"agent_id" %in% names(events)) return(fills[0])
  keys <- unique(paste(events$agent_id, events$asset_id, as.numeric(events$timestamp), events$action_id, sep = "|"))
  fill_keys <- paste(fills$agent_id, fills$asset_id, as.numeric(fills$timestamp), fills$action_id, sep = "|")
  fills[fill_keys %in% keys]
}

#' @keywords internal
.portfolio_step_result <- function(exchange, agent_id, rebalance_id = NULL, fills = NULL, outcomes = data.table::data.table(), context = NULL) {
  requested_agent_id <- as.character(agent_id)
  requested_rebalance_id <- as.character(rebalance_id)
  orders <- data.table::copy(exchange$agent_orders[exchange$agent_orders$agent_id == requested_agent_id])
  if (!is.null(rebalance_id)) orders <- orders[orders$rebalance_id == requested_rebalance_id]
  durable_fills <- data.table::copy(exchange$portfolio_fills[exchange$portfolio_fills$agent_id == requested_agent_id])
  if (!is.null(rebalance_id)) {
    selected_rebalance_id <- requested_rebalance_id
    durable_fills <- durable_fills[durable_fills$rebalance_id == selected_rebalance_id]
  }
  if (is.null(fills)) {
    fills <- durable_fills
  } else if (!"agent_id" %in% names(fills) || nrow(fills) == 0L) {
    fills <- durable_fills[0]
  } else {
    fills <- .portfolio_fills_for_events(exchange, fills, agent_id)
    if (!is.null(rebalance_id)) fills <- fills[fills$rebalance_id == selected_rebalance_id]
  }
  positions <- data.table::copy(if (is.null(context)) sim_exchange_positions(exchange) else context$positions)
  if (nrow(positions) && "agent_id" %in% names(positions)) positions <- positions[positions$agent_id == requested_agent_id]
  account <- data.table::copy(if (is.null(context)) sim_exchange_account(exchange) else context$account)
  if (nrow(account) && "agent_id" %in% names(account)) account <- account[account$agent_id == requested_agent_id]
  targets <- data.table::copy(exchange$portfolio_targets[exchange$portfolio_targets$agent_id == requested_agent_id])
  if (!is.null(rebalance_id)) targets <- targets[targets$rebalance_id == requested_rebalance_id]
  rebalances <- data.table::copy(exchange$portfolio_rebalances[exchange$portfolio_rebalances$agent_id == requested_agent_id])
  if (!is.null(rebalance_id)) rebalances <- rebalances[rebalances$rebalance_id == requested_rebalance_id]
  equity <- if (nrow(account) && "equity" %in% names(account)) as.numeric(account$equity[1L]) else NA_real_
  realized_weights <- data.table::copy(positions)
  if ("notional" %in% names(realized_weights)) {
    realized_weights[, realized_weight := if (is.finite(equity) && equity != 0) notional / equity else NA_real_]
  } else {
    realized_weights[, realized_weight := numeric()]
  }
  valuations <- if (is.null(context)) .portfolio_valuation_snapshot(exchange) else data.table::copy(context$valuations)
  if (nrow(positions) && nrow(valuations)) {
    idx <- match(valuations$asset_id, positions$asset_id)
    matched <- !is.na(idx)
    if (any(matched)) {
      valuations[matched, `:=`(
        timestamp = positions$timestamp[idx[matched]],
        last_px = as.numeric(positions$last_px[idx[matched]])
      )]
    }
  }
  valuations[, agent_id := agent_id]
  list(rebalance_id = rebalance_id, orders = orders, fills = fills, positions = positions, account = account, targets = targets, rebalances = rebalances, realized_weights = realized_weights, valuations = valuations, outcomes = data.table::as.data.table(outcomes))
}

#' @keywords internal
.portfolio_valuation_snapshot <- function(exchange) {
  assets <- sim_assets(exchange)[status == "active", .(symbol, asset_id)]
  if (!nrow(assets)) {
    return(data.table::data.table(symbol = character(), asset_id = integer(), timestamp = as.POSIXct(character(), tz = "UTC"), last_px = numeric()))
  }
  market_events <- exchange$market_events
  valuations <- data.table::copy(assets)
  valuations[, `:=`(timestamp = as.POSIXct(NA, tz = "UTC"), last_px = NA_real_)]
  if (!nrow(market_events)) return(valuations[])
  latest <- data.table::copy(market_events)
  data.table::setorderv(latest, c("asset_id", "timestamp"))
  latest <- latest[, .SD[.N], by = asset_id]
  idx <- match(valuations$asset_id, latest$asset_id)
  matched <- !is.na(idx)
  valuations[matched, `:=`(
    timestamp = latest$timestamp[idx[matched]],
    last_px = as.numeric(latest$close[idx[matched]])
  )]
  valuations[]
}

#' @keywords internal
.portfolio_submission_context <- function(exchange) {
  list(
    account = data.table::copy(sim_exchange_account(exchange)),
    positions = data.table::copy(sim_exchange_positions(exchange)),
    market_events = data.table::copy(exchange$market_events),
    valuations = .portfolio_valuation_snapshot(exchange)
  )
}
