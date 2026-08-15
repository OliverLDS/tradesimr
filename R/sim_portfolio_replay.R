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
#' @return A list containing `orders`, `fills`, `positions`, `account`,
#'   `targets`, `realized_weights`, and `outcomes`.
#' @export
sim_portfolio_target_step <- function(exchange,
                                      agent_id,
                                      bars,
                                      target_weights = NULL,
                                      execution = sim_portfolio_execution(),
                                      decision_label = "target_weight") {
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
  .portfolio_apply_execution_config(exchange, execution)
  first_asset <- .bar_asset_key(decision_bars[1L])
  .ensure_agent_account(exchange, agent_id, asset_id = first_asset$asset_id, symbol = first_asset$symbol, agent_type = "arena")

  # First execute only orders made on earlier decision boundaries.
  current_fills <- sim_schemas()$events[0]
  if (nrow(new_bars) > 0L) {
    sim_exchange_step(exchange, new_bars)
    current_fills <- sim_fills(exchange$new_events)
  }
  if (is.null(target_weights)) {
    return(.portfolio_step_result(exchange, agent_id, fills = current_fills, outcomes = .portfolio_outcome_row(
      rebalance_id = NA_character_, timestamp = decision_bars$timestamp[1L], agent_id = agent_id,
      status = "no_decision", message = "No target-weight decision; prior positions were retained."
    )))
  }

  timestamp <- decision_bars$timestamp[1L]
  rebalance_id <- paste0("RB", sprintf("%06d", exchange$next_rebalance_id %||% 1L))
  exchange$next_rebalance_id <- as.integer(exchange$next_rebalance_id %||% 1L) + 1L
  targets <- tryCatch(
    .portfolio_normalize_targets(exchange, target_weights, execution$max_gross_weight),
    error = function(error) error
  )
  if (inherits(targets, "error")) {
    message <- conditionMessage(targets)
    exchange$portfolio_rebalances <- data.table::rbindlist(list(exchange$portfolio_rebalances, data.table::data.table(
      rebalance_id = rebalance_id, timestamp = timestamp, agent_id = agent_id,
      status = "rejected", execution_timing = execution$timing, fee_rt = execution$fee_rt,
      slippage = execution$slippage, spread = execution$spread, message = message
    )), fill = TRUE)
    return(.portfolio_step_result(exchange, agent_id, rebalance_id, fills = current_fills, outcomes = .portfolio_outcome_row(
      rebalance_id, timestamp, agent_id, "rejected", message
    )))
  }
  plan <- .portfolio_rebalance_plan(exchange, agent_id, targets, decision_bars, execution)
  target_rows <- plan$targets[, .(
    rebalance_id,
    timestamp,
    eligible_after = timestamp,
    agent_id,
    symbol,
    asset_id,
    target_weight,
    realized_weight_before,
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
    return(.portfolio_step_result(exchange, agent_id, rebalance_id, fills = current_fills, outcomes = .portfolio_outcome_row(
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
  .portfolio_step_result(exchange, agent_id, rebalance_id, fills = current_fills, outcomes = plan$targets[, .(
    rebalance_id, timestamp, agent_id, symbol, asset_id, status = outcome_status, message = outcome_message
  )])
}

#' Export a safe portfolio replay snapshot for an external consumer
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Agent identifier.
#' @param path Output directory.
#' @param format Export format. Phase 1 supports JSON.
#' @return A named vector of written paths.
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
.portfolio_normalize_targets <- function(exchange, target_weights, max_gross_weight) {
  if (is.null(names(target_weights)) || any(!nzchar(names(target_weights)))) {
    stop("`target_weights` must be a named numeric vector keyed by registered symbols.", call. = FALSE)
  }
  weights <- as.numeric(target_weights)
  names(weights) <- names(target_weights)
  if (any(!is.finite(weights)) || anyDuplicated(names(weights))) {
    stop("`target_weights` must contain finite, uniquely named values.", call. = FALSE)
  }
  assets <- sim_assets(exchange)[status == "active"]
  unknown <- setdiff(names(weights), assets$symbol)
  if (length(unknown)) stop("Unknown or inactive target symbol(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  out <- data.table::copy(assets[, .(symbol, asset_id, contract_size, qty_step)])
  out[, target_weight := weights[symbol]]
  out[is.na(target_weight), target_weight := 0]
  if (sum(abs(out$target_weight)) > max_gross_weight + 1e-10) {
    stop("Absolute target weights exceed `max_gross_weight`.", call. = FALSE)
  }
  out[]
}

#' @keywords internal
.portfolio_rebalance_plan <- function(exchange, agent_id, targets, bars, execution) {
  account <- sim_exchange_account(exchange)
  equity <- account[account$agent_id == agent_id, equity]
  equity <- if (length(equity)) as.numeric(equity[1L]) else as.numeric(exchange$config$cash %||% exchange$config$init_cash %||% 100000)
  if (!is.finite(equity) || equity <= 0) stop("Agent account equity must be positive to translate target weights.", call. = FALSE)
  positions <- sim_exchange_positions(exchange)
  positions <- positions[positions$agent_id == agent_id]
  latest <- data.table::copy(targets)
  latest[, decision_price := vapply(seq_len(.N), function(i) {
    bar_price <- bars[asset_id == latest$asset_id[i], close]
    if (length(bar_price)) return(as.numeric(bar_price[1L]))
    pos_price <- positions[asset_id == latest$asset_id[i], last_px]
    if (length(pos_price)) return(as.numeric(pos_price[1L]))
    historical <- exchange$market_events[asset_id == latest$asset_id[i], close]
    if (length(historical)) return(as.numeric(tail(historical, 1L)))
    NA_real_
  }, numeric(1L))]
  if (any(!is.finite(latest$decision_price) | latest$decision_price <= 0)) {
    missing <- latest$symbol[!is.finite(latest$decision_price) | latest$decision_price <= 0]
    stop("Cannot value target symbol(s) without a completed or carried price: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  latest[, desired_signed_qty := round((target_weight * equity / (decision_price * contract_size)) / qty_step) * qty_step]
  latest[, current_signed_qty := vapply(asset_id, function(requested_asset_id) {
    row <- positions[positions$asset_id == requested_asset_id]
    if (!nrow(row)) return(0)
    as.numeric(row$pos_dir[1L] * row$ctr_unit[1L])
  }, numeric(1L))]
  latest[, realized_weight_before := vapply(seq_len(.N), function(i) {
    row <- positions[asset_id == latest$asset_id[i]]
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
    realized_pnl = NA_real_
  )
}

#' @keywords internal
.portfolio_outcome_row <- function(rebalance_id, timestamp, agent_id, status, message) {
  data.table::data.table(rebalance_id = rebalance_id, timestamp = timestamp, agent_id = agent_id, symbol = NA_character_, asset_id = NA_integer_, status = status, message = message)
}

#' @keywords internal
.portfolio_step_result <- function(exchange, agent_id, rebalance_id = NULL, fills = NULL, outcomes = data.table::data.table()) {
  orders <- data.table::copy(exchange$agent_orders[exchange$agent_orders$agent_id == agent_id])
  if (!is.null(rebalance_id)) orders <- orders[orders$rebalance_id == rebalance_id]
  if (is.null(fills) || !"agent_id" %in% names(fills)) fills <- sim_schemas()$fills[0]
  fills <- data.table::copy(fills[fills$agent_id == agent_id])
  positions <- data.table::copy(sim_exchange_positions(exchange)[sim_exchange_positions(exchange)$agent_id == agent_id])
  account <- data.table::copy(sim_exchange_account(exchange)[sim_exchange_account(exchange)$agent_id == agent_id])
  targets <- data.table::copy(exchange$portfolio_targets[exchange$portfolio_targets$agent_id == agent_id])
  if (!is.null(rebalance_id)) targets <- targets[targets$rebalance_id == rebalance_id]
  equity <- if (nrow(account)) as.numeric(account$equity[1L]) else NA_real_
  realized_weights <- data.table::copy(positions)
  realized_weights[, realized_weight := if (is.finite(equity) && equity != 0) notional / equity else NA_real_]
  valuations <- data.table::copy(sim_assets(exchange))[, .(symbol, asset_id)]
  valuation_rows <- lapply(valuations$asset_id, function(requested_asset_id) {
    row <- positions[positions$asset_id == requested_asset_id, last_px]
    row_timestamp <- positions[positions$asset_id == requested_asset_id, timestamp]
    if (length(row)) return(list(timestamp = row_timestamp[1L], last_px = as.numeric(row[1L])))
    bars <- exchange$market_events[exchange$market_events$asset_id == requested_asset_id]
    if (nrow(bars)) {
      latest <- bars[which.max(timestamp)]
      return(list(timestamp = latest$timestamp, last_px = as.numeric(latest$close)))
    }
    list(timestamp = as.POSIXct(NA, tz = "UTC"), last_px = NA_real_)
  })
  valuations[, timestamp := as.POSIXct(vapply(valuation_rows, function(row) as.numeric(row$timestamp), numeric(1L)), origin = "1970-01-01", tz = "UTC")]
  valuations[, last_px := vapply(valuation_rows, `[[`, numeric(1L), "last_px")]
  valuations[, agent_id := agent_id]
  list(rebalance_id = rebalance_id, orders = orders, fills = fills, positions = positions, account = account, targets = targets, realized_weights = realized_weights, valuations = valuations, outcomes = data.table::as.data.table(outcomes))
}
