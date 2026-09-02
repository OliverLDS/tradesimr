#' Project execution quality for durable portfolio target rebalances
#'
#' The projection evaluates each target using its decision-time target record,
#' canonical order lifecycle, durable fills, and the position/account snapshot
#' at the relevant settlement boundary. It never uses a later current mark to
#' classify an earlier rebalance. Filled fee-aware target orders are assessed
#' against their exact C++-recorded executable quantity.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Optional account identifier used to filter the projection.
#' @param summary If `FALSE` (the default), return one row per rebalance and
#'   symbol. If `TRUE`, return one rebalance-level row whose quality is the
#'   worst component quality.
#' @return A public-safe data.table. Symbol rows contain `rebalance_id`,
#'   `agent_id`, `symbol`, `asset_id`, `decision_timestamp`, `eligible_after`,
#'   `settlement_timestamp`, `target_weight`, `decision_equity`,
#'   `decision_price`, `qty_step`, `contract_size`,
#'   `current_signed_quantity`, `expected_signed_quantity`,
#'   `expected_notional`, `realized_signed_quantity`, `realized_notional`,
#'   `quantity_deviation`, `notional_deviation`, `weight_deviation`,
#'   `execution_quality`, and `message`.
#' @export
sim_portfolio_execution_quality <- function(exchange, agent_id = NULL, summary = FALSE) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  targets <- data.table::copy(exchange$portfolio_targets)
  if (!is.null(agent_id)) {
    requested_agent_id <- as.character(agent_id)
    targets <- targets[targets$agent_id == requested_agent_id]
  }
  if (!nrow(targets)) return(.portfolio_execution_quality_empty())
  data.table::setorderv(targets, c("timestamp", "rebalance_id", "asset_id"))
  out <- data.table::rbindlist(lapply(seq_len(nrow(targets)), function(i) {
    .portfolio_execution_quality_row(exchange, targets[i])
  }), fill = TRUE)
  if (isTRUE(summary)) return(.portfolio_execution_quality_summary(exchange, out))
  out[]
}

#' @keywords internal
.portfolio_execution_quality_empty <- function() {
  data.table::data.table(
    rebalance_id = character(), agent_id = character(), symbol = character(), asset_id = integer(),
    decision_timestamp = as.POSIXct(character(), tz = "UTC"), eligible_after = as.POSIXct(character(), tz = "UTC"),
    settlement_timestamp = as.POSIXct(character(), tz = "UTC"), target_weight = numeric(), decision_equity = numeric(),
    decision_price = numeric(), qty_step = numeric(), contract_size = numeric(), current_signed_quantity = numeric(),
    expected_signed_quantity = numeric(), expected_notional = numeric(), realized_signed_quantity = numeric(),
    realized_notional = numeric(), quantity_deviation = numeric(), notional_deviation = numeric(),
    weight_deviation = numeric(), execution_quality = character(), message = character()
  )
}

#' @keywords internal
.portfolio_execution_quality_row <- function(exchange, target) {
  agent <- as.character(target$agent_id[1L])
  requested_asset_id <- as.integer(target$asset_id[1L])
  requested_rebalance_id <- as.character(target$rebalance_id[1L])
  asset_id <- requested_asset_id
  rebalance_id <- requested_rebalance_id
  decision_timestamp <- .portfolio_quality_timestamp(target$timestamp[1L])
  eligible_after <- .portfolio_quality_timestamp(target$eligible_after[1L] %||% decision_timestamp)
  spec <- exchange$assets[exchange$assets$asset_id == requested_asset_id]
  qty_step <- if (nrow(spec)) as.numeric(spec$qty_step[1L]) else 1
  contract_size <- if (nrow(spec)) as.numeric(spec$contract_size[1L]) else 1
  if (!is.finite(qty_step) || qty_step <= 0) qty_step <- 1
  if (!is.finite(contract_size) || contract_size <= 0) contract_size <- 1
  decision_position <- .portfolio_quality_position(exchange, agent, asset_id, decision_timestamp)
  current_signed_quantity <- decision_position$signed_quantity
  decision_equity <- as.numeric(target$decision_equity[1L] %||% NA_real_)
  if (!is.finite(decision_equity)) decision_equity <- .portfolio_quality_equity(exchange, agent, decision_timestamp)
  decision_price <- as.numeric(target$decision_price[1L] %||% NA_real_)
  target_weight <- as.numeric(target$target_weight[1L])
  planned_quantity <- as.numeric(target$planned_signed_quantity[1L] %||% NA_real_)
  if (!is.finite(planned_quantity) && is.finite(decision_equity) && is.finite(decision_price) && decision_price > 0) {
    planned_quantity <- round((target_weight * decision_equity / (decision_price * contract_size)) / qty_step) * qty_step
  }
  orders <- exchange$agent_orders[
    exchange$agent_orders$agent_id == agent &
      exchange$agent_orders$rebalance_id == requested_rebalance_id &
      exchange$agent_orders$asset_id == requested_asset_id
  ]
  fills <- exchange$portfolio_fills[
    exchange$portfolio_fills$agent_id == agent &
      exchange$portfolio_fills$rebalance_id == requested_rebalance_id &
      exchange$portfolio_fills$asset_id == requested_asset_id
  ]
  target_status <- as.character(target$status[1L] %||% "accepted")
  order_status <- as.character(orders$status %||% character())
  superseded <- identical(target_status, "superseded") || any(order_status == "superseded")
  margin_clipped <- "reason_code" %in% names(orders) && any(orders$reason_code == "margin_clipped", na.rm = TRUE)
  terminal <- order_status %in% c("rejected", "cancelled", "failed", "no_op")
  all_filled <- nrow(orders) > 0L && all(order_status == "filled") &&
    all(orders$order_id %in% fills$order_id)
  any_filled <- nrow(fills) > 0L
  settlement_timestamp <- as.POSIXct(NA, tz = "UTC")
  if (any_filled) {
    settlement_timestamp <- .portfolio_quality_timestamp(max(fills$timestamp))
  } else if (any(terminal) && "settlement_timestamp" %in% names(orders)) {
    settled <- orders$settlement_timestamp[!is.na(orders$settlement_timestamp)]
    if (length(settled)) settlement_timestamp <- .portfolio_quality_timestamp(max(settled))
  } else if (superseded) {
    settlement_timestamp <- .portfolio_quality_timestamp(decision_timestamp)
  } else if (identical(target_status, "no_op")) {
    settlement_timestamp <- .portfolio_quality_timestamp(decision_timestamp)
  }
  settlement_position <- if (!is.na(settlement_timestamp)) {
    .portfolio_quality_position(exchange, agent, asset_id, settlement_timestamp)
  } else {
    list(signed_quantity = NA_real_, last_px = NA_real_)
  }
  realized_signed_quantity <- settlement_position$signed_quantity
  if (!is.finite(realized_signed_quantity) && any_filled) {
    realized_signed_quantity <- .portfolio_quality_apply_fills(current_signed_quantity, fills)
  }
  expected_signed_quantity <- planned_quantity
  if (identical(target_status, "no_op")) expected_signed_quantity <- current_signed_quantity
  if (all_filled && !margin_clipped) {
    # Exact fill quantities include C++ fee-aware clipping and are therefore
    # the authoritative executable target for quality assessment.
    expected_signed_quantity <- .portfolio_quality_apply_fills(current_signed_quantity, fills)
  }
  settlement_price <- settlement_position$last_px
  if (!is.finite(settlement_price) && any_filled) settlement_price <- as.numeric(fills$price[nrow(fills)])
  if (!is.finite(settlement_price)) settlement_price <- decision_price
  expected_notional <- expected_signed_quantity * settlement_price * contract_size
  realized_notional <- realized_signed_quantity * settlement_price * contract_size
  quantity_deviation <- realized_signed_quantity - expected_signed_quantity
  notional_deviation <- realized_notional - expected_notional
  settlement_equity <- if (!is.na(settlement_timestamp)) .portfolio_quality_equity(exchange, agent, settlement_timestamp) else NA_real_
  weight_deviation <- if (is.finite(settlement_equity) && settlement_equity != 0) realized_notional / settlement_equity - target_weight else NA_real_
  tolerance <- qty_step / 2
  if (superseded) {
    quality <- "superseded"
    reasons <- unique(orders$message[order_status == "superseded" & !is.na(orders$message) & nzchar(orders$message)])
    message <- if (length(reasons)) paste(reasons, collapse = " ") else "Superseded by a later target-weight decision before execution."
  } else if (identical(target_status, "no_op")) {
    quality <- "no_op"
    message <- as.character(target$message[1L] %||% "Target already matched the rounded contract quantity.")
  } else if (margin_clipped && any_filled) {
    quality <- "partial"
    message <- "Target-derived order was clipped to available portfolio-margin capacity."
  } else if (all_filled && is.finite(quantity_deviation) && abs(quantity_deviation) <= tolerance) {
    quality <- "fulfilled"
    message <- "All order actions filled at the eligible market boundary."
  } else if (any(terminal) && any_filled) {
    quality <- "partial"
    message <- "Some order actions filled, but a terminal order outcome prevented full target fulfillment."
  } else if (any(terminal)) {
    quality <- "terminal_rejected"
    reasons <- unique(orders$message[terminal & !is.na(orders$message) & nzchar(orders$message)])
    message <- if (length(reasons)) paste(reasons, collapse = " ") else "A terminal order rejection prevented fulfillment."
  } else if (any_filled) {
    quality <- "partial"
    message <- "Filled order actions did not produce the expected executable quantity."
  } else {
    quality <- "pending"
    message <- "Awaiting a strictly later eligible completed market bar."
  }
  data.table::data.table(
    rebalance_id = rebalance_id, agent_id = agent, symbol = as.character(target$symbol[1L]), asset_id = asset_id,
    decision_timestamp = decision_timestamp, eligible_after = eligible_after, settlement_timestamp = settlement_timestamp,
    target_weight = target_weight, decision_equity = decision_equity, decision_price = decision_price,
    qty_step = qty_step, contract_size = contract_size, current_signed_quantity = current_signed_quantity,
    expected_signed_quantity = expected_signed_quantity, expected_notional = expected_notional,
    realized_signed_quantity = realized_signed_quantity, realized_notional = realized_notional,
    quantity_deviation = quantity_deviation, notional_deviation = notional_deviation,
    weight_deviation = weight_deviation, execution_quality = quality, message = message
  )
}

#' @keywords internal
.portfolio_quality_timestamp <- function(value) {
  if (!length(value) || is.na(value[1L])) return(as.POSIXct(NA, tz = "UTC"))
  as.POSIXct(as.numeric(value[1L]), origin = "1970-01-01", tz = "UTC")
}

#' @keywords internal
.portfolio_quality_position <- function(exchange, agent_id, asset_id, timestamp) {
  requested_agent_id <- as.character(agent_id)
  requested_asset_id <- as.integer(asset_id)
  requested_timestamp <- timestamp
  snapshots <- exchange$step_snapshots
  if (is.null(snapshots) || !nrow(snapshots) || is.na(requested_timestamp)) {
    return(list(signed_quantity = 0, last_px = NA_real_))
  }
  rows <- snapshots[
    snapshots$agent_id == requested_agent_id & snapshots$asset_id == requested_asset_id & snapshots$timestamp <= requested_timestamp
  ]
  if (!nrow(rows)) return(list(signed_quantity = 0, last_px = NA_real_))
  row <- rows[which.max(timestamp)]
  list(signed_quantity = as.numeric(row$pos_dir[1L] * row$ctr_unit[1L]), last_px = as.numeric(row$last_px[1L]))
}

#' @keywords internal
.portfolio_quality_equity <- function(exchange, agent_id, timestamp) {
  requested_agent_id <- as.character(agent_id)
  requested_timestamp <- timestamp
  snapshots <- exchange$step_snapshots
  if (is.null(snapshots) || !nrow(snapshots) || is.na(requested_timestamp)) return(NA_real_)
  rows <- snapshots[snapshots$agent_id == requested_agent_id & snapshots$timestamp <= requested_timestamp]
  if (!nrow(rows)) return(NA_real_)
  latest_timestamp <- max(rows$timestamp)
  account <- .aggregate_account_snapshots(rows[rows$timestamp == latest_timestamp], latest = TRUE)
  value <- account$equity[account$agent_id == requested_agent_id]
  if (length(value)) as.numeric(value[1L]) else NA_real_
}

#' @keywords internal
.portfolio_quality_apply_fills <- function(start_quantity, fills) {
  quantity <- as.numeric(start_quantity)
  if (!nrow(fills)) return(quantity)
  data.table::setorderv(fills, c("timestamp", "fill_id"))
  for (i in seq_len(nrow(fills))) {
    action <- as.character(fills$action[i] %||% fills$action_label[i])
    direction <- as.character(fills$dir_label[i] %||% "flat")
    signed_qty <- as.numeric(fills$ctr_qty[i]) * if (direction == "short") -1 else 1
    if (action == "open") quantity <- signed_qty
    else if (action == "increase") quantity <- quantity + signed_qty
    else if (action == "close") quantity <- 0
    else if (action == "reduce") quantity <- quantity - signed_qty
  }
  quantity
}

#' @keywords internal
.portfolio_execution_quality_summary <- function(exchange, quality) {
  if (!nrow(quality)) return(quality)
  severity <- c(fulfilled = 0L, superseded = 0L, no_op = 1L, pending = 2L, partial = 3L, terminal_rejected = 4L)
  quality[, .severity := severity[execution_quality]]
  out <- quality[, {
    worst <- which.max(.severity)
    list(
      symbol = "ALL", asset_id = NA_integer_, decision_timestamp = min(decision_timestamp),
      eligible_after = min(eligible_after), settlement_timestamp = if (all(is.na(settlement_timestamp))) as.POSIXct(NA, tz = "UTC") else max(settlement_timestamp, na.rm = TRUE),
      target_weight = sum(target_weight), decision_equity = decision_equity[1L], decision_price = NA_real_,
      qty_step = NA_real_, contract_size = NA_real_, current_signed_quantity = sum(current_signed_quantity),
      expected_signed_quantity = sum(expected_signed_quantity), expected_notional = sum(expected_notional),
      realized_signed_quantity = sum(realized_signed_quantity), realized_notional = sum(realized_notional),
      quantity_deviation = sum(quantity_deviation), notional_deviation = sum(notional_deviation),
      weight_deviation = sum(weight_deviation), execution_quality = execution_quality[worst],
      message = paste(unique(message), collapse = " ")
    )
  }, by = .(rebalance_id, agent_id)]
  rebalance_status <- exchange$portfolio_rebalances[, .(rebalance_id, rebalance_status = status)]
  out <- merge(out, rebalance_status, by = "rebalance_id", all.x = TRUE, sort = FALSE)
  superseded <- out$rebalance_status %in% c("superseded", "partially_superseded")
  if (any(superseded)) {
    data.table::set(out, i = which(superseded), j = "execution_quality", value = "superseded")
    data.table::set(out, i = which(superseded), j = "message", value = "Superseded by a later target-weight decision before all legs executed.")
  }
  out[, rebalance_status := NULL]
  out[]
}
