#' Create an in-memory simulated exchange state
#'
#' @param config Named list of simulation parameters passed to `sim_backtest()`.
#' @return A mutable environment containing market, intent, order, and result
#'   tables.
#' @export
sim_exchange_new <- function(config = list()) {
  state <- new.env(parent = emptyenv())
  state$config <- config
  state$market_events <- sim_schemas()$market_events[0]
  state$intents <- sim_schemas()$intents[0]
  state$agent_orders <- sim_schemas()$agent_orders[0]
  state$agent_commands <- sim_schemas()$agent_commands[0]
  state$order_requests <- sim_schemas()$order_requests[0]
  state$order_cancellations <- sim_schemas()$order_cancellations[0]
  state$event_log <- data.table::data.table(
    timestamp = as.POSIXct(character()),
    source = character(),
    event = character(),
    ref_id = character()
  )
  state$result <- NULL
  state$last_result <- NULL
  state$last_events <- data.table::data.table()
  state$new_events <- data.table::data.table()
  state_config <- config
  if (!is.null(state_config$init_cash) && is.null(state_config$cash)) {
    state_config$cash <- state_config$init_cash
  }
  state$step_state <- do.call(sim_state, state_config[intersect(names(state_config), names(formals(sim_state)))])
  state$step_snapshots <- data.table::data.table()
  state$step_events <- data.table::data.table()
  state$last_bar_count <- 0L
  state$next_order_id <- 1L
  state$next_command_id <- 1L
  class(state) <- c("tradesimr_exchange", "environment")
  state
}

#' Append market bars to a simulated exchange
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars Market bars coercible by `as_market_bars()`.
#' @return The exchange, invisibly.
#' @export
sim_exchange_add_bars <- function(exchange, bars) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  new_bars <- as_market_bars(bars)
  exchange$market_events <- data.table::rbindlist(list(exchange$market_events, new_bars), fill = TRUE)
  invisible(exchange)
}

#' Place an order into a simulated exchange
#'
#' Explicit orders use `qty_type = "contracts"` by default for buy/sell/flat
#' orders. Use `qty_type = "target_pos"` or `side = "target"` for exposure
#' targets consumed by replay-style backtests.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param agent_id Agent identifier.
#' @param timestamp Order timestamp.
#' @param tgt_pos Target exposure. Kept for compatibility with earlier
#'   intent-level calls.
#' @param tol_pos Target-position tolerance.
#' @param order_type Order type: `market` or `limit`.
#' @param side Order side: `target`, `buy`, `sell`, or `flat`.
#' @param qty Order quantity. Meaning is controlled by `qty_type`.
#' @param qty_type Quantity semantics: `contracts` or `target_pos`.
#' @param limit_price Optional limit price for limit orders.
#' @param time_in_force Time-in-force label.
#' @param client_order_id Optional client order id.
#' @return The generated order id.
#' @export
sim_exchange_place_order <- function(exchange,
                                     agent_id,
                                     timestamp,
                                     tgt_pos = NULL,
                                     tol_pos = 0,
                                     order_type = c("market", "limit"),
                                     side = c("target", "buy", "sell", "flat"),
                                     qty_type = NULL,
                                     qty = NULL,
                                     limit_price = NA_real_,
                                     time_in_force = "gtc",
                                     client_order_id = NA_character_) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  order_type <- match.arg(order_type)
  side <- match.arg(side)
  if (is.null(qty_type)) qty_type <- if (side == "target") "target_pos" else "contracts"
  qty_type <- match.arg(qty_type, c("contracts", "target_pos"))
  if (qty_type == "contracts" && side == "target") {
    stop("`side = 'target'` requires `qty_type = 'target_pos'`.", call. = FALSE)
  }
  if (qty_type == "contracts" && is.null(qty) && side != "flat") {
    stop("`qty` is required for contract orders.", call. = FALSE)
  }
  order_id <- paste0("ORD", sprintf("%06d", exchange$next_order_id))
  exchange$next_order_id <- exchange$next_order_id + 1L
  if (is.null(qty)) qty <- if (is.null(tgt_pos)) NA_real_ else abs(as.numeric(tgt_pos))
  target <- .order_to_target_pos(side = side, qty = qty, qty_type = qty_type, tgt_pos = tgt_pos)
  row <- data.table::data.table(
    order_id = order_id,
    client_order_id = as.character(client_order_id),
    agent_id = as.character(agent_id),
    timestamp = timestamp,
    order_type = order_type,
    side = side,
    qty_type = qty_type,
    qty = as.numeric(qty),
    limit_price = as.numeric(limit_price),
    time_in_force = as.character(time_in_force),
    tgt_pos = target,
    tol_pos = as.numeric(tol_pos),
    status = "accepted"
  )
  exchange$agent_orders <- data.table::rbindlist(list(exchange$agent_orders, row), fill = TRUE)
  exchange$event_log <- data.table::rbindlist(list(exchange$event_log, data.table::data.table(
    timestamp = timestamp,
    source = "agent_order",
    event = "accepted",
    ref_id = order_id
  )), fill = TRUE)
  invisible(order_id)
}

#' Cancel an intent-level order in a simulated exchange
#'
#' @param exchange A `tradesimr_exchange`.
#' @param order_id Order id returned by `sim_exchange_place_order()`.
#' @return Invisibly returns `TRUE` if an order was cancelled.
#' @export
sim_exchange_cancel_order <- function(exchange, order_id) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  idx <- which(exchange$agent_orders$order_id == order_id & exchange$agent_orders$status == "accepted")
  if (length(idx) == 0L) return(invisible(FALSE))
  data.table::set(exchange$agent_orders, i = idx, j = "status", value = "cancelled")
  exchange$event_log <- data.table::rbindlist(list(exchange$event_log, data.table::data.table(
    timestamp = Sys.time(),
    source = "agent_order",
    event = "cancelled",
    ref_id = order_id
  )), fill = TRUE)
  invisible(TRUE)
}

#' Run or refresh a simulated exchange replay
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A simulation result returned by `sim_backtest()`.
#' @export
sim_exchange_run <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  bars <- data.table::copy(exchange$market_events)
  if (nrow(bars) == 0L) stop("No market bars have been added.", call. = FALSE)

  active_orders <- exchange$agent_orders[
    exchange$agent_orders$status == "accepted" &
      exchange$agent_orders$qty_type == "target_pos"
  ]
  if (nrow(active_orders) == 0L) {
    data.table::set(bars, j = "tgt_pos", value = rep.int(0, nrow(bars)))
    data.table::set(bars, j = "tol_pos", value = rep.int(0, nrow(bars)))
  } else {
    order_intents <- active_orders[, .SD, .SDcols = c("timestamp", "tgt_pos", "tol_pos", "order_type", "limit_price")]
    data.table::setorderv(order_intents, "timestamp")
    data.table::set(bars, j = "tgt_pos", value = rep.int(0, nrow(bars)))
    data.table::set(bars, j = "tol_pos", value = rep.int(0, nrow(bars)))
    data.table::set(bars, j = "order_type", value = rep.int("market", nrow(bars)))
    data.table::set(bars, j = "limit_price", value = rep.int(NA_real_, nrow(bars)))
    for (i in seq_len(nrow(order_intents))) {
      idx <- which(bars$timestamp >= order_intents$timestamp[i])
      if (length(idx) > 0L) {
        data.table::set(bars, i = idx, j = "tgt_pos", value = order_intents$tgt_pos[i])
        data.table::set(bars, i = idx, j = "tol_pos", value = order_intents$tol_pos[i])
        data.table::set(bars, i = idx, j = "order_type", value = order_intents$order_type[i])
        data.table::set(bars, i = idx, j = "limit_price", value = order_intents$limit_price[i])
      }
    }
  }

  exchange$last_result <- exchange$result
  args <- c(list(
    data = bars,
    tol_pos_col = "tol_pos",
    order_type_col = if ("order_type" %in% names(bars)) "order_type" else NULL,
    limit_price_col = if ("limit_price" %in% names(bars)) "limit_price" else NULL
  ), exchange$config)
  exchange$result <- do.call(sim_backtest, args)
  current_events <- sim_events(exchange$result)
  if (nrow(exchange$last_events) == 0L) {
    exchange$new_events <- current_events
  } else {
    old_max <- max(exchange$last_events$event_id, na.rm = TRUE)
    exchange$new_events <- current_events[current_events$event_id > old_max]
  }
  exchange$last_events <- current_events
  exchange$last_bar_count <- nrow(exchange$market_events)
  exchange$result
}

#' Step a simulated exchange with one or more bars
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars Market bars coercible by `as_market_bars()`.
#' @return The incremental simulation snapshots.
#' @export
sim_exchange_step <- function(exchange, bars) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  new_bars <- as_market_bars(bars)
  exchange$market_events <- data.table::rbindlist(list(exchange$market_events, new_bars), fill = TRUE)
  step_results <- vector("list", nrow(new_bars))
  new_event_list <- list()

  for (i in seq_len(nrow(new_bars))) {
    bar <- new_bars[i]
    orders <- .exchange_orders_for_bar(exchange, bar$timestamp[1L])
    step_config <- exchange$config[intersect(names(exchange$config), names(formals(sim_step)))]
    step_config$init_cash <- NULL
    step_config$fill_model <- NULL
    step_config$order_type_col <- NULL
    step_config$limit_price_col <- NULL
    step_args <- c(list(
      state = exchange$step_state,
      bar = bar,
      orders = orders
    ), step_config)
    step <- do.call(sim_step, step_args)
    exchange$step_state <- step$state
    snapshot <- .state_to_snapshot(step$state, bar$timestamp[1L])
    step_results[[i]] <- snapshot
    if (nrow(step$events) > 0L) {
      new_event_list[[length(new_event_list) + 1L]] <- step$events
      .mark_orders_from_events(exchange, orders, step$events)
    }
  }

  new_snapshots <- data.table::rbindlist(step_results, fill = TRUE)
  exchange$step_snapshots <- data.table::rbindlist(list(exchange$step_snapshots, new_snapshots), fill = TRUE)
  exchange$new_events <- data.table::rbindlist(new_event_list, fill = TRUE)
  exchange$step_events <- data.table::rbindlist(list(exchange$step_events, exchange$new_events), fill = TRUE)
  exchange$result <- exchange$step_snapshots
  data.table::setattr(exchange$result, "events", exchange$step_events)
  data.table::setattr(exchange$result, "orders", sim_orders(exchange$step_events))
  exchange$last_events <- exchange$step_events
  exchange$last_bar_count <- nrow(exchange$market_events)
  exchange$result
}

#' Get simulated exchange orders
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A data.table of accepted/cancelled intent-level orders.
#' @export
sim_exchange_orders <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  data.table::copy(exchange$agent_orders)
}

#' Get new events since the previous exchange run
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A data.table of newly observed simulation events.
#' @export
sim_exchange_new_events <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  data.table::copy(exchange$new_events)
}

#' Get simulated exchange account state
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A one-row data.table with the latest account snapshot.
#' @export
sim_exchange_account <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$result)) return(data.table::data.table())
  tail(sim_account(exchange$result), 1L)
}

#' Get simulated exchange positions
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A one-row data.table with the latest position snapshot.
#' @export
sim_exchange_positions <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$result)) return(data.table::data.table())
  tail(sim_positions(exchange$result), 1L)
}

#' Export exchange events and state
#'
#' @param exchange A `tradesimr_exchange`.
#' @param path Output directory.
#' @param format File format, either `csv` or `fst`.
#' @return Invisibly returns written file paths.
#' @export
sim_exchange_save <- function(exchange, path, format = c("csv", "fst")) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  format <- match.arg(format)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  if (!is.null(exchange$result)) {
    paths <- sim_export(exchange$result, path, format = format)
  } else {
    paths <- character()
  }
  state_tables <- list(
    market_events = exchange$market_events,
    agent_orders = exchange$agent_orders,
    agent_commands = exchange$agent_commands,
    order_requests = exchange$order_requests,
    order_cancellations = exchange$order_cancellations,
    exchange_event_log = exchange$event_log
  )
  for (nm in names(state_tables)) {
    file <- file.path(path, paste0(nm, ".", format))
    if (format == "csv") {
      data.table::fwrite(state_tables[[nm]], file)
    } else {
      if (!requireNamespace("fst", quietly = TRUE)) stop("Package `fst` is required for fst export.", call. = FALSE)
      fst::write_fst(as.data.frame(state_tables[[nm]]), file)
    }
    paths[[nm]] <- file
  }
  invisible(paths)
}

#' Load exchange state from disk
#'
#' @param path Directory produced by `sim_exchange_save()`.
#' @return A `tradesimr_exchange`.
#' @export
sim_exchange_load <- function(path) {
  exchange <- sim_exchange_new()
  manifest_file <- file.path(path, "manifest.csv")
  if (file.exists(manifest_file)) {
    imported <- sim_import(path)
    exchange$result <- imported$simulation
    exchange$last_events <- if (!is.null(imported$events)) sim_events(imported$events) else data.table::data.table()
    exchange$step_events <- exchange$last_events
    exchange$step_snapshots <- if (!is.null(imported$simulation)) imported$simulation else data.table::data.table()
    if (!is.null(imported$simulation) && nrow(imported$simulation) > 0L) {
      last <- imported$simulation[nrow(imported$simulation)]
      exchange$step_state <- sim_state(
        cash = last$cash,
        pos_dir = last$pos_dir,
        ctr_unit = last$ctr_unit,
        avg_price = last$avg_price,
        last_px = last$last_px,
        old_timestamp = as.numeric(last$timestamp)
      )
    }
  }
  if (file.exists(file.path(path, "market_events.csv"))) exchange$market_events <- data.table::fread(file.path(path, "market_events.csv"))
  if (file.exists(file.path(path, "agent_orders.csv"))) exchange$agent_orders <- data.table::fread(file.path(path, "agent_orders.csv"))
  if (file.exists(file.path(path, "agent_commands.csv"))) exchange$agent_commands <- data.table::fread(file.path(path, "agent_commands.csv"))
  if (file.exists(file.path(path, "order_requests.csv"))) exchange$order_requests <- data.table::fread(file.path(path, "order_requests.csv"))
  if (file.exists(file.path(path, "order_cancellations.csv"))) exchange$order_cancellations <- data.table::fread(file.path(path, "order_cancellations.csv"))
  if (file.exists(file.path(path, "exchange_event_log.csv"))) exchange$event_log <- data.table::fread(file.path(path, "exchange_event_log.csv"))
  if (nrow(exchange$agent_orders) > 0L) {
    numeric_ids <- suppressWarnings(as.integer(sub("^ORD", "", exchange$agent_orders$order_id)))
    exchange$next_order_id <- max(numeric_ids, na.rm = TRUE) + 1L
  }
  if (nrow(exchange$agent_commands) > 0L) {
    numeric_command_ids <- suppressWarnings(as.integer(sub("^CMD", "", exchange$agent_commands$command_id)))
    exchange$next_command_id <- max(numeric_command_ids, na.rm = TRUE) + 1L
  }
  exchange
}

#' Export exchange simulation events
#'
#' @param exchange A `tradesimr_exchange`.
#' @param path Output directory.
#' @param format File format, either `csv` or `fst`.
#' @return Invisibly returns written file paths.
#' @export
sim_exchange_export_events <- function(exchange, path, format = c("csv", "fst")) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$result)) stop("Exchange has no simulation result to export.", call. = FALSE)
  sim_export(exchange$result, path, format = match.arg(format), tables = c("events", "orders", "fills", "account", "risk"))
}

#' @keywords internal
.order_to_target_pos <- function(side, qty, qty_type, tgt_pos) {
  if (qty_type == "contracts") return(NA_real_)
  if (side == "target") {
    if (is.null(tgt_pos)) stop("`tgt_pos` is required when side = 'target'.", call. = FALSE)
    return(as.numeric(tgt_pos))
  }
  if (side == "buy") return(abs(as.numeric(qty)))
  if (side == "sell") return(-abs(as.numeric(qty)))
  if (side == "flat") return(0)
  stop("Unsupported order side: ", side, call. = FALSE)
}

#' @keywords internal
.exchange_orders_for_bar <- function(exchange, timestamp) {
  orders <- exchange$agent_orders[
    exchange$agent_orders$status == "accepted" &
      exchange$agent_orders$qty_type == "contracts" &
      exchange$agent_orders$timestamp <= timestamp
  ]
  if (nrow(orders) == 0L) {
    return(data.table::data.table(
      order_id = character(),
      action = character(),
      dir = character(),
      order_type = character(),
      ctr_qty = numeric(),
      price = numeric(),
      strat_id = integer(),
      action_id = integer()
    ))
  }
  cur_dir <- as.integer(exchange$step_state$pos_dir %||% 0L)
  cur_qty <- as.numeric(exchange$step_state$ctr_unit %||% 0)
  side <- tolower(as.character(orders$side))
  action <- character(nrow(orders))
  dir <- character(nrow(orders))
  ctr_qty <- numeric(nrow(orders))
  for (i in seq_len(nrow(orders))) {
    if (!side[i] %in% c("buy", "sell", "flat")) {
      stop("Contract orders require side `buy`, `sell`, or `flat`.", call. = FALSE)
    }
    target_dir <- if (side[i] == "buy") 1L else if (side[i] == "sell") -1L else 0L
    target_qty <- abs(as.numeric(orders$qty[i]))
    if (target_dir == 0L) {
      action[i] <- "close"
      dir[i] <- "flat"
      ctr_qty[i] <- cur_qty
    } else if (cur_dir == 0L) {
      action[i] <- "open"
      dir[i] <- if (target_dir > 0L) "long" else "short"
      ctr_qty[i] <- target_qty
    } else if (cur_dir == target_dir) {
      action[i] <- "increase"
      dir[i] <- if (target_dir > 0L) "long" else "short"
      ctr_qty[i] <- target_qty
    } else {
      action[i] <- "close"
      dir[i] <- "flat"
      ctr_qty[i] <- cur_qty
    }
  }
  data.table::data.table(
    order_id = orders$order_id,
    action = action,
    dir = dir,
    order_type = orders$order_type,
    ctr_qty = ctr_qty,
    price = orders$limit_price,
    strat_id = 0L,
    action_id = seq.int(
      as.integer(exchange$step_state$action_id_now %||% 1L),
      length.out = nrow(orders)
    )
  )
}

#' @keywords internal
.mark_orders_from_events <- function(exchange, orders, events) {
  if (nrow(orders) == 0L || nrow(events) == 0L) return(invisible(NULL))
  filled_actions <- events$action_id[events$status_label == "filled"]
  idx <- which(orders$action_id %in% filled_actions)
  if (length(idx) == 0L) return(invisible(NULL))
  order_idx <- match(orders$order_id[idx], exchange$agent_orders$order_id)
  order_idx <- order_idx[!is.na(order_idx)]
  if (length(order_idx) > 0L) {
    data.table::set(exchange$agent_orders, i = order_idx, j = "status", value = "filled")
  }
  invisible(NULL)
}

#' @keywords internal
.state_to_snapshot <- function(state, timestamp) {
  data.table::data.table(
    timestamp = timestamp,
    equity = as.numeric(state$equity),
    cash = as.numeric(state$cash),
    pos_dir = as.integer(state$pos_dir),
    ctr_unit = as.numeric(state$ctr_unit),
    avg_price = as.numeric(state$avg_price),
    last_px = as.numeric(state$last_px),
    notional = as.numeric(state$notional),
    abs_notional = as.numeric(state$abs_notional),
    unrealized_pnl = as.numeric(state$unrealized_pnl),
    maintenance_margin = as.numeric(state$maintenance_margin)
  )
}
