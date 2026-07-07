#' Create a local live exchange service
#'
#' Builds an optional `plumber` app exposing local HTTP endpoints for agent
#' commands. The service is a thin API over append-only command logs and the
#' existing exchange APIs.
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A `plumber` router.
#' @export
sim_live_service <- function(exchange = sim_exchange_new()) {
  plumber_pkg <- .service_plumber_package()
  if (!requireNamespace(plumber_pkg, quietly = TRUE)) {
    stop("Package `plumber` is required for the live service. Install it with install.packages('plumber').", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required for the live service. Install it with install.packages('jsonlite').", call. = FALSE)
  }
  stopifnot(inherits(exchange, "tradesimr_exchange"))

  plumber_ns <- asNamespace(plumber_pkg)
  pr <- plumber_ns$pr()
  pr <- plumber_ns$pr_get(pr, "/health", function(res) {
    .service_headers(res)
    list(status = "ok")
  })
  pr <- plumber_ns$pr_get(pr, "/state", function(res) {
    .service_headers(res)
    .service_state(exchange)
  })
  pr <- plumber_ns$pr_get(pr, "/feed/status", function(res) {
    .service_headers(res)
    sim_feed_status(exchange)
  })
  pr <- plumber_ns$pr_post(pr, "/feed/config", function(req, res) {
    .service_headers(res)
    body <- .service_json_body(req)
    if (!is.null(body$random_walk)) body$random_walk <- as.list(body$random_walk)
    sim_feed_configure(exchange, body)
    sim_feed_status(exchange)
  })
  pr <- plumber_ns$pr_post(pr, "/feed/start", function(req, res) {
    .service_headers(res)
    body <- .service_json_body(req)
    sim_feed_start(exchange, now = .service_timestamp(body$now %||% Sys.time()))
  })
  pr <- plumber_ns$pr_post(pr, "/feed/stop", function(res) {
    .service_headers(res)
    sim_feed_stop(exchange)
  })
  pr <- plumber_ns$pr_post(pr, "/feed/step", function(req, res) {
    .service_headers(res)
    body <- .service_json_body(req)
    bars <- sim_feed_step(
      exchange,
      now = .service_timestamp(body$now %||% Sys.time()),
      max_bars = as.numeric(body$max_bars %||% Inf)
    )
    list(feed = sim_feed_status(exchange), bars = .service_records(bars), state = .service_state(exchange))
  })
  pr <- plumber_ns$pr_post(pr, "/orders", function(req, res) {
    .service_headers(res)
    body <- .service_json_body(req)
    command_id <- sim_submit_order(
      exchange = exchange,
      agent_id = body$agent_id %||% "agent",
      timestamp = .service_timestamp(body$timestamp %||% Sys.time()),
      tgt_pos = .null_if_missing(body$tgt_pos),
      tol_pos = as.numeric(body$tol_pos %||% 0),
      order_type = body$order_type %||% "market",
      side = body$side %||% "buy",
      qty_type = .null_if_missing(body$qty_type),
      qty = .null_if_missing(body$qty),
      limit_price = as.numeric(body$limit_price %||% NA_real_),
      time_in_force = body$time_in_force %||% "gtc",
      client_order_id = body$client_order_id %||% NA_character_,
      process = TRUE
    )
    list(command_id = command_id, state = .service_state(exchange))
  })
  pr <- plumber_ns$pr_post(pr, "/cancel", function(req, res) {
    .service_headers(res)
    body <- .service_json_body(req)
    if (is.null(body$order_id)) stop("`order_id` is required.", call. = FALSE)
    command_id <- sim_cancel_order(
      exchange = exchange,
      agent_id = body$agent_id %||% "agent",
      order_id = body$order_id,
      client_order_id = body$client_order_id %||% NA_character_,
      timestamp = .service_timestamp(body$timestamp %||% Sys.time()),
      process = TRUE
    )
    list(command_id = command_id, state = .service_state(exchange))
  })
  pr <- plumber_ns$pr_post(pr, "/bars", function(req, res) {
    .service_headers(res)
    body <- .service_json_body(req)
    bars <- if (!is.null(body$bars)) data.table::rbindlist(lapply(body$bars, data.table::as.data.table), fill = TRUE) else data.table::as.data.table(body)
    if ("timestamp" %in% names(bars)) data.table::set(bars, j = "timestamp", value = .service_timestamp(bars$timestamp))
    sim_exchange_step(exchange, bars)
    .service_state(exchange)
  })
  pr
}

#' Run a local live exchange service
#'
#' @param exchange A `tradesimr_exchange`.
#' @param host Host interface.
#' @param port Port.
#' @return The result of `plumber`'s `run()` method.
#' @export
sim_live_service_run <- function(exchange = sim_exchange_new(), host = "127.0.0.1", port = 8080) {
  pr <- sim_live_service(exchange)
  pr$run(host = host, port = port)
}

#' @keywords internal
.service_state <- function(exchange) {
  list(
    account = .service_records(sim_exchange_account(exchange)),
    positions = .service_records(sim_exchange_positions(exchange)),
    market_events = .service_records(exchange$market_events),
    agent_orders = .service_records(exchange$agent_orders),
    agent_commands = .service_records(exchange$agent_commands),
    order_requests = .service_records(exchange$order_requests),
    order_cancellations = .service_records(exchange$order_cancellations),
    events = .service_records(sim_exchange_new_events(exchange)),
    feed = sim_feed_status(exchange)
  )
}

#' @keywords internal
.service_records <- function(x) {
  if (is.null(x) || nrow(x) == 0L) return(list())
  lapply(seq_len(nrow(x)), function(i) as.list(x[i]))
}

#' @keywords internal
.service_headers <- function(res) {
  if (!is.null(res)) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  }
  invisible(TRUE)
}

#' @keywords internal
.service_json_body <- function(req) {
  raw <- req$postBody %||% ""
  if (!nzchar(raw)) return(list())
  out <- jsonlite::fromJSON(raw, simplifyVector = FALSE)
  if (is.null(out)) list() else out
}

#' @keywords internal
.service_timestamp <- function(x) {
  if (inherits(x, "POSIXt")) return(x)
  if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  as.POSIXct(x, tz = "UTC")
}

#' @keywords internal
.null_if_missing <- function(x) {
  if (is.null(x) || length(x) == 0L || identical(x, "") || (is.atomic(x) && length(x) == 1L && is.na(x))) NULL else x
}

#' @keywords internal
.service_plumber_package <- function() {
  paste0("plum", "ber")
}
