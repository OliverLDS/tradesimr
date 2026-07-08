#' Get live-agent command schemas
#'
#' @return A named list of empty data.tables for append-only agent commands,
#'   order requests, and order cancellations.
#' @export
sim_agent_command_schema <- function() {
  schemas <- sim_schemas()
  schemas[c("agent_commands", "order_requests", "order_cancellations")]
}

#' Submit an agent order command
#'
#' Appends an order request to the exchange command log. By default the command
#' is processed immediately into the same exchange order model used by
#' `sim_exchange_step()`.
#'
#' @inheritParams sim_exchange_place_order
#' @param process Whether to process pending commands immediately.
#' @return The generated command id.
#' @export
sim_submit_order <- function(exchange,
                             agent_id,
                             timestamp = Sys.time(),
                             symbol = NULL,
                             asset_id = NULL,
                             tgt_pos = NULL,
                             tol_pos = 0,
                             order_type = c("market", "limit"),
                             side = c("target", "buy", "sell", "flat"),
                             qty_type = NULL,
                             qty = NULL,
                             limit_price = NA_real_,
                             time_in_force = "gtc",
                             client_order_id = NA_character_,
                             process = TRUE) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  asset <- .normalize_asset_key(symbol = symbol, asset_id = asset_id, exchange = exchange)
  .ensure_agent_account(exchange, agent_id, asset_id = asset$asset_id, symbol = asset$symbol, agent_type = "human")
  order_type <- match.arg(order_type)
  side <- match.arg(side)
  if (is.null(qty_type)) qty_type <- if (side == "target") "target_pos" else "contracts"
  qty_type <- match.arg(qty_type, c("contracts", "target_pos"))
  command_id <- .next_command_id(exchange)
  request <- data.table::data.table(
    command_id = command_id,
    client_order_id = as.character(client_order_id),
    agent_id = as.character(agent_id),
    symbol = asset$symbol,
    asset_id = asset$asset_id,
    timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
    order_type = order_type,
    side = side,
    qty_type = qty_type,
    qty = if (is.null(qty)) NA_real_ else as.numeric(qty),
    limit_price = as.numeric(limit_price),
    time_in_force = as.character(time_in_force),
    tgt_pos = if (is.null(tgt_pos)) NA_real_ else as.numeric(tgt_pos),
    tol_pos = as.numeric(tol_pos),
    status = "pending",
    order_id = NA_character_,
    message = NA_character_
  )
  exchange$order_requests <- data.table::rbindlist(list(exchange$order_requests, request), fill = TRUE)
  .append_agent_command(exchange, command_id, timestamp, agent_id, "order_request", "pending", NA_character_, NA_character_)
  if (isTRUE(process)) sim_exchange_process_commands(exchange)
  invisible(command_id)
}

#' Submit an agent order cancellation command
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Agent identifier.
#' @param order_id Order id to cancel.
#' @param client_order_id Optional client order id for audit purposes.
#' @param timestamp Command timestamp.
#' @param process Whether to process pending commands immediately.
#' @return The generated command id.
#' @export
sim_cancel_order <- function(exchange,
                             agent_id,
                             order_id,
                             client_order_id = NA_character_,
                             timestamp = Sys.time(),
                             process = TRUE) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  command_id <- .next_command_id(exchange)
  cancellation <- data.table::data.table(
    command_id = command_id,
    agent_id = as.character(agent_id),
    timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
    order_id = as.character(order_id),
    client_order_id = as.character(client_order_id),
    status = "pending",
    message = NA_character_
  )
  exchange$order_cancellations <- data.table::rbindlist(list(exchange$order_cancellations, cancellation), fill = TRUE)
  .append_agent_command(exchange, command_id, timestamp, agent_id, "order_cancellation", "pending", order_id, NA_character_)
  if (isTRUE(process)) sim_exchange_process_commands(exchange)
  invisible(command_id)
}

#' Process pending agent commands
#'
#' Converts pending append-only order request and cancellation commands into
#' exchange orders and cancellation attempts.
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A data.table of processed command rows.
#' @export
sim_exchange_process_commands <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  processed <- list()

  pending_requests <- which(exchange$order_requests$status == "pending")
  if (length(pending_requests) > 0L) {
    for (idx in pending_requests) {
      request <- exchange$order_requests[idx]
      result <- tryCatch({
        order_id <- sim_exchange_place_order(
          exchange = exchange,
          agent_id = request$agent_id,
          symbol = request$symbol,
          asset_id = request$asset_id,
          timestamp = request$timestamp,
          tgt_pos = if (is.na(request$tgt_pos)) NULL else request$tgt_pos,
          tol_pos = request$tol_pos,
          order_type = request$order_type,
          side = request$side,
          qty_type = request$qty_type,
          qty = if (is.na(request$qty)) NULL else request$qty,
          limit_price = request$limit_price,
          time_in_force = request$time_in_force,
          client_order_id = request$client_order_id
        )
        list(status = "accepted", ref_id = order_id, message = "accepted")
      }, error = function(err) {
        list(status = "rejected", ref_id = NA_character_, message = conditionMessage(err))
      })
      data.table::set(exchange$order_requests, i = idx, j = "status", value = result$status)
      data.table::set(exchange$order_requests, i = idx, j = "order_id", value = result$ref_id)
      data.table::set(exchange$order_requests, i = idx, j = "message", value = result$message)
      .update_agent_command(exchange, request$command_id, result$status, result$ref_id, result$message)
      processed[[length(processed) + 1L]] <- exchange$agent_commands[exchange$agent_commands$command_id == request$command_id]
    }
  }

  pending_cancellations <- which(exchange$order_cancellations$status == "pending")
  if (length(pending_cancellations) > 0L) {
    for (idx in pending_cancellations) {
      cancellation <- exchange$order_cancellations[idx]
      ok <- sim_exchange_cancel_order(exchange, cancellation$order_id)
      status <- if (isTRUE(ok)) "accepted" else "rejected"
      message <- if (isTRUE(ok)) "cancelled" else "order not found or not accepted"
      data.table::set(exchange$order_cancellations, i = idx, j = "status", value = status)
      data.table::set(exchange$order_cancellations, i = idx, j = "message", value = message)
      .update_agent_command(exchange, cancellation$command_id, status, cancellation$order_id, message)
      processed[[length(processed) + 1L]] <- exchange$agent_commands[exchange$agent_commands$command_id == cancellation$command_id]
    }
  }

  data.table::rbindlist(processed, fill = TRUE)
}

#' @keywords internal
.next_command_id <- function(exchange) {
  if (is.null(exchange$next_command_id)) exchange$next_command_id <- 1L
  command_id <- paste0("CMD", sprintf("%06d", exchange$next_command_id))
  exchange$next_command_id <- exchange$next_command_id + 1L
  command_id
}

#' @keywords internal
.append_agent_command <- function(exchange, command_id, timestamp, agent_id, command_type, status, ref_id, message) {
  row <- data.table::data.table(
    command_id = command_id,
    timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
    agent_id = as.character(agent_id),
    command_type = as.character(command_type),
    status = as.character(status),
    ref_id = as.character(ref_id),
    message = as.character(message)
  )
  exchange$agent_commands <- data.table::rbindlist(list(exchange$agent_commands, row), fill = TRUE)
  invisible(row)
}

#' @keywords internal
.update_agent_command <- function(exchange, command_id, status, ref_id, message) {
  idx <- which(exchange$agent_commands$command_id == command_id)
  if (length(idx) == 0L) return(invisible(FALSE))
  data.table::set(exchange$agent_commands, i = idx, j = "status", value = as.character(status))
  data.table::set(exchange$agent_commands, i = idx, j = "ref_id", value = as.character(ref_id))
  data.table::set(exchange$agent_commands, i = idx, j = "message", value = as.character(message))
  invisible(TRUE)
}
