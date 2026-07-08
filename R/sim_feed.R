#' Default live feed configuration
#'
#' @param symbol Instrument symbol.
#' @param timeframe Bar interval, such as `"4h"`, `"1h"`, or `"15m"`.
#' @param tz Time zone used to align completed bar boundaries.
#' @param feed_mode Feed mode: `simulation` or `external`.
#' @param feed_adapter Optional external adapter function with signature
#'   `function(symbol, timeframe, start, end, tz = "UTC")`.
#' @param start_time Optional first completed boundary to process.
#' @param random_walk List of random-walk simulation settings: `start_price`,
#'   `drift`, `vol`, and `seed`.
#' @return A list suitable for `sim_feed_configure()`.
#' @export
sim_feed_config <- function(symbol = "BTC-USDT-SWAP",
                            timeframe = "4h",
                            tz = "UTC",
                            feed_mode = c("simulation", "external"),
                            feed_adapter = NULL,
                            start_time = NULL,
                            random_walk = list(start_price = 100, drift = 0, vol = 0.02, seed = 1L)) {
  feed_mode <- match.arg(feed_mode)
  list(
    symbol = symbol,
    timeframe = timeframe,
    tz = tz,
    feed_mode = feed_mode,
    feed_adapter = feed_adapter,
    start_time = start_time,
    random_walk = utils::modifyList(list(start_price = 100, drift = 0, vol = 0.02, seed = 1L), random_walk),
    running = FALSE,
    last_completed_end = NULL,
    last_price = NULL
  )
}

#' Configure a live exchange feed
#'
#' @param exchange A `tradesimr_exchange`.
#' @param config Feed configuration list.
#' @return The feed configuration, invisibly.
#' @export
sim_feed_configure <- function(exchange, config = sim_feed_config()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  previous <- exchange$feed
  defaults <- sim_feed_config()
  supplied <- names(config)
  config <- utils::modifyList(defaults, config)
  if (!is.null(previous)) {
    for (field in c("running", "last_completed_end", "last_price")) {
      if (!field %in% supplied) config[[field]] <- previous[[field]]
    }
  }
  config$feed_mode <- match.arg(config$feed_mode, c("simulation", "external"))
  .feed_parse_timeframe(config$timeframe)
  if (identical(config$feed_mode, "external") && !is.function(config$feed_adapter)) {
    stop("`feed_adapter` must be a function when feed_mode = 'external'.", call. = FALSE)
  }
  if (is.null(config$last_price)) {
    config$last_price <- as.numeric(config$random_walk$start_price)
  }
  exchange$feed <- config
  invisible(exchange$feed)
}

#' Start a configured live feed
#'
#' @param exchange A `tradesimr_exchange`.
#' @param now Current time used to initialize the schedule.
#' @return Feed status.
#' @export
sim_feed_start <- function(exchange, now = Sys.time()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$feed)) sim_feed_configure(exchange)
  exchange$feed$running <- TRUE
  now <- .feed_as_time(now, exchange$feed$tz)
  if (is.null(exchange$feed$last_completed_end)) {
    start_time <- exchange$feed$start_time
    if (is.null(start_time)) {
      exchange$feed$last_completed_end <- .feed_latest_completed_end(now, exchange$feed$timeframe, exchange$feed$tz)
    } else {
      exchange$feed$last_completed_end <- .feed_as_time(start_time, exchange$feed$tz)
    }
  }
  sim_feed_status(exchange)
}

#' Stop a configured live feed
#'
#' @param exchange A `tradesimr_exchange`.
#' @return Feed status.
#' @export
sim_feed_stop <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$feed)) sim_feed_configure(exchange)
  exchange$feed$running <- FALSE
  sim_feed_status(exchange)
}

#' Step a live feed through completed bars
#'
#' Generates or fetches all completed bars after the last processed feed
#' boundary and appends them through `sim_exchange_step()`.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param now Current time.
#' @param max_bars Maximum bars to append in one call.
#' @return A data.table of bars appended by this call.
#' @export
sim_feed_step <- function(exchange, now = Sys.time(), max_bars = Inf) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$feed)) sim_feed_configure(exchange)
  feed <- exchange$feed
  now <- .feed_as_time(now, feed$tz)
  latest_end <- .feed_latest_completed_end(now, feed$timeframe, feed$tz)
  if (is.null(feed$last_completed_end)) {
    start_time <- feed$start_time
    feed$last_completed_end <- if (is.null(start_time)) latest_end else .feed_as_time(start_time, feed$tz)
  }
  step_seconds <- .feed_parse_timeframe(feed$timeframe)
  if (as.numeric(feed$last_completed_end) > as.numeric(latest_end) - step_seconds) {
    exchange$feed <- feed
    return(sim_schemas()$market_events[0])
  }
  starts <- seq(
    from = as.numeric(feed$last_completed_end),
    to = as.numeric(latest_end) - step_seconds,
    by = step_seconds
  )
  if (!length(starts) || starts[1] > as.numeric(latest_end) - step_seconds) {
    exchange$feed <- feed
    return(sim_schemas()$market_events[0])
  }
  if (is.finite(max_bars)) starts <- utils::head(starts, max_bars)

  bars <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    start <- as.POSIXct(starts[i], origin = "1970-01-01", tz = feed$tz)
    end <- as.POSIXct(starts[i] + step_seconds, origin = "1970-01-01", tz = feed$tz)
    bars[[i]] <- .feed_get_bar(feed, start, end)
    feed$last_completed_end <- end
    if (nrow(bars[[i]]) > 0L && "close" %in% names(bars[[i]])) {
      feed$last_price <- as.numeric(tail(bars[[i]]$close, 1L))
    }
  }
  out <- data.table::rbindlist(bars, fill = TRUE)
  if (nrow(out) > 0L) {
    for (i in seq_len(nrow(out))) {
      sim_agents_step(exchange, out[i])
      sim_exchange_process_commands(exchange)
      sim_exchange_step(exchange, out[i])
    }
  }
  exchange$feed <- feed
  out[]
}

#' Generate historical simulation bars before starting a live feed
#'
#' Appends `n_bars` simulated OHLC bars ending at the latest completed boundary.
#' This is a market-history warmup: it does not step AI agents or process
#' pending orders.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param n_bars Number of historical bars to append.
#' @param now Current time used to align the latest completed boundary.
#' @return A data.table of appended market bars.
#' @export
sim_feed_warmup <- function(exchange, n_bars = 100L, now = Sys.time()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$feed)) sim_feed_configure(exchange)
  feed <- exchange$feed
  if (!identical(feed$feed_mode, "simulation")) {
    stop("Feed warmup currently supports `feed_mode = 'simulation'` only.", call. = FALSE)
  }
  n_bars <- as.integer(n_bars)
  if (is.na(n_bars) || n_bars <= 0L) return(sim_schemas()$market_events[0])
  step_seconds <- .feed_parse_timeframe(feed$timeframe)
  latest_end <- .feed_latest_completed_end(.feed_as_time(now, feed$tz), feed$timeframe, feed$tz)
  starts <- seq(
    from = as.numeric(latest_end) - step_seconds * n_bars,
    to = as.numeric(latest_end) - step_seconds,
    by = step_seconds
  )
  if (!length(starts)) return(sim_schemas()$market_events[0])
  bars <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    start <- as.POSIXct(starts[i], origin = "1970-01-01", tz = feed$tz)
    end <- as.POSIXct(starts[i] + step_seconds, origin = "1970-01-01", tz = feed$tz)
    bars[[i]] <- .feed_get_bar(feed, start, end)
    feed$last_completed_end <- end
    if (nrow(bars[[i]]) > 0L && "close" %in% names(bars[[i]])) {
      feed$last_price <- as.numeric(tail(bars[[i]]$close, 1L))
    }
  }
  out <- data.table::rbindlist(bars, fill = TRUE)
  if (nrow(out) > 0L) {
    exchange$market_events <- data.table::rbindlist(list(exchange$market_events, out), fill = TRUE)
  }
  exchange$feed <- feed
  out[]
}

#' Get live feed status
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A list describing feed state.
#' @export
sim_feed_status <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$feed)) sim_feed_configure(exchange)
  feed <- exchange$feed
  list(
    symbol = feed$symbol,
    timeframe = feed$timeframe,
    tz = feed$tz,
    feed_mode = feed$feed_mode,
    running = isTRUE(feed$running),
    last_completed_end = feed$last_completed_end,
    last_price = feed$last_price,
    bars = nrow(exchange$market_events)
  )
}

#' @keywords internal
.feed_get_bar <- function(feed, start, end) {
  if (identical(feed$feed_mode, "external")) {
    bar <- feed$feed_adapter(feed$symbol, feed$timeframe, start, end, tz = feed$tz)
    return(as_market_bars(bar))
  }
  .feed_random_walk_bar(feed, start, end)
}

#' @keywords internal
.feed_random_walk_bar <- function(feed, start, end) {
  rw <- feed$random_walk
  seed <- as.integer(rw$seed %||% 1L)
  key <- as.integer(as.numeric(start) / max(1, .feed_parse_timeframe(feed$timeframe)))
  set.seed(seed + key)
  open <- as.numeric(feed$last_price %||% rw$start_price %||% 100)
  drift <- as.numeric(rw$drift %||% 0)
  vol <- as.numeric(rw$vol %||% 0.02)
  ret <- drift + stats::rnorm(1L, mean = 0, sd = vol)
  close <- max(.Machine$double.eps, open * exp(ret))
  wiggle <- abs(stats::rnorm(2L, mean = 0, sd = vol / 2))
  high <- max(open, close) * (1 + wiggle[1])
  low <- min(open, close) * max(.Machine$double.eps, 1 - wiggle[2])
  data.table::data.table(
    timestamp = start,
    open = open,
    high = high,
    low = low,
    close = close
  )
}

#' @keywords internal
.feed_parse_timeframe <- function(timeframe) {
  x <- tolower(trimws(as.character(timeframe)[1L]))
  value <- suppressWarnings(as.numeric(sub("^([0-9.]+).*$", "\\1", x)))
  unit <- sub("^[0-9.]+\\s*", "", x)
  if (is.na(value) || value <= 0) stop("Invalid timeframe: ", timeframe, call. = FALSE)
  multiplier <- switch(unit,
    s = 1, sec = 1, secs = 1, second = 1, seconds = 1,
    m = 60, min = 60, mins = 60, minute = 60, minutes = 60,
    h = 3600, hr = 3600, hour = 3600, hours = 3600,
    d = 86400, day = 86400, days = 86400,
    stop("Unsupported timeframe unit: ", unit, call. = FALSE)
  )
  as.numeric(value * multiplier)
}

#' @keywords internal
.feed_latest_completed_end <- function(now, timeframe, tz) {
  seconds <- .feed_parse_timeframe(timeframe)
  now <- .feed_as_time(now, tz)
  epoch <- floor(as.numeric(now) / seconds) * seconds
  as.POSIXct(epoch, origin = "1970-01-01", tz = tz)
}

#' @keywords internal
.feed_as_time <- function(x, tz = "UTC") {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = tz))
  if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = tz))
  as.POSIXct(x, tz = tz)
}
