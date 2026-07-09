#' Default live feed configuration
#'
#' @param symbol Instrument symbol.
#' @param asset_id Optional integer asset id.
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
                            asset_id = NULL,
                            timeframe = "4h",
                            tz = "UTC",
                            feed_mode = c("simulation", "external"),
                            feed_adapter = NULL,
                            start_time = NULL,
                            random_walk = list(start_price = 100, drift = 0, vol = 0.02, seed = 1L)) {
  feed_mode <- match.arg(feed_mode)
  list(
    symbol = symbol,
    asset_id = asset_id %||% .asset_id_from_symbol(symbol),
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
#' @param config Feed configuration list. A list with `configs` may be used to
#'   configure all registered assets in one call.
#' @return The feed configuration, invisibly.
#' @export
sim_feed_configure <- function(exchange, config = sim_feed_config()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (!is.null(config$configs)) {
    configured <- lapply(config$configs, function(one) sim_feed_configure(exchange, as.list(one)))
    return(invisible(configured))
  }
  previous <- exchange$feed
  defaults <- sim_feed_config()
  supplied <- names(config)
  config <- utils::modifyList(defaults, config)
  if (!is.null(previous)) {
    for (field in c("symbol", "asset_id")) {
      if (!field %in% supplied) config[[field]] <- previous[[field]]
    }
  }
  asset <- .asset_require_registered(
    exchange,
    symbol = config$symbol %||% NULL,
    asset_id = config$asset_id %||% NULL,
    context = "feed asset"
  )
  feed_key <- as.character(asset$asset_id)
  previous <- if (!is.null(exchange$feeds[[feed_key]])) exchange$feeds[[feed_key]] else previous
  config$symbol <- asset$symbol
  config$asset_id <- asset$asset_id
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
  if (is.null(exchange$feeds)) exchange$feeds <- list()
  exchange$feeds[[feed_key]] <- config
  exchange$feed <- config
  invisible(exchange$feed)
}

#' Start a configured live feed
#'
#' @param exchange A `tradesimr_exchange`.
#' @param now Current time used to initialize the schedule.
#' @param symbol,asset_id Optional feed asset selector. If omitted, all
#'   configured active asset feeds are started.
#' @return Feed status.
#' @export
sim_feed_start <- function(exchange, now = Sys.time(), symbol = NULL, asset_id = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  keys <- .feed_request_keys(exchange, symbol = symbol, asset_id = asset_id)
  for (key in keys) {
    selected <- .feed_select(exchange, asset_id = as.integer(key))
    feed <- selected$feed
    feed$running <- TRUE
    now_feed <- .feed_as_time(now, feed$tz)
    if (is.null(feed$last_completed_end)) {
      start_time <- feed$start_time
      if (is.null(start_time)) {
        feed$last_completed_end <- .feed_latest_completed_end(now_feed, feed$timeframe, feed$tz)
      } else {
        feed$last_completed_end <- .feed_as_time(start_time, feed$tz)
      }
    }
    .feed_store(exchange, selected$key, feed)
  }
  sim_feed_status(exchange)
}

#' Stop a configured live feed
#'
#' @param exchange A `tradesimr_exchange`.
#' @param symbol,asset_id Optional feed asset selector. If omitted, all
#'   configured active asset feeds are stopped.
#' @return Feed status.
#' @export
sim_feed_stop <- function(exchange, symbol = NULL, asset_id = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  keys <- .feed_request_keys(exchange, symbol = symbol, asset_id = asset_id)
  for (key in keys) {
    selected <- .feed_select(exchange, asset_id = as.integer(key))
    feed <- selected$feed
    feed$running <- FALSE
    .feed_store(exchange, selected$key, feed)
  }
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
#' @param symbol,asset_id Optional feed asset selector. If omitted, all
#'   configured active asset feeds are stepped.
#' @return A data.table of bars appended by this call.
#' @export
sim_feed_step <- function(exchange, now = Sys.time(), max_bars = Inf, symbol = NULL, asset_id = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  keys <- .feed_request_keys(exchange, symbol = symbol, asset_id = asset_id)
  out <- lapply(keys, function(key) .feed_step_one(exchange, now = now, max_bars = max_bars, asset_id = as.integer(key)))
  data.table::rbindlist(out, fill = TRUE)
}

#' @keywords internal
.feed_step_one <- function(exchange, now = Sys.time(), max_bars = Inf, asset_id) {
  selected <- .feed_select(exchange, asset_id = asset_id)
  feed <- selected$feed
  now <- .feed_as_time(now, feed$tz)
  latest_end <- .feed_latest_completed_end(now, feed$timeframe, feed$tz)
  if (is.null(feed$last_completed_end)) {
    start_time <- feed$start_time
    feed$last_completed_end <- if (is.null(start_time)) latest_end else .feed_as_time(start_time, feed$tz)
  }
  step_seconds <- .feed_parse_timeframe(feed$timeframe)
  if (as.numeric(feed$last_completed_end) > as.numeric(latest_end) - step_seconds) {
    .feed_store(exchange, selected$key, feed)
    return(sim_schemas()$market_events[0])
  }
  starts <- seq(
    from = as.numeric(feed$last_completed_end),
    to = as.numeric(latest_end) - step_seconds,
    by = step_seconds
  )
  if (!length(starts) || starts[1] > as.numeric(latest_end) - step_seconds) {
    .feed_store(exchange, selected$key, feed)
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
  .feed_store(exchange, selected$key, feed)
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
#' @param symbol,asset_id Optional feed asset selector. If omitted, all
#'   configured active asset feeds are warmed up.
#' @return A data.table of appended market bars.
#' @export
sim_feed_warmup <- function(exchange, n_bars = 100L, now = Sys.time(), symbol = NULL, asset_id = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  keys <- .feed_request_keys(exchange, symbol = symbol, asset_id = asset_id)
  out <- lapply(keys, function(key) .feed_warmup_one(exchange, n_bars = n_bars, now = now, asset_id = as.integer(key)))
  data.table::rbindlist(out, fill = TRUE)
}

#' @keywords internal
.feed_warmup_one <- function(exchange, n_bars = 100L, now = Sys.time(), asset_id) {
  selected <- .feed_select(exchange, asset_id = asset_id)
  feed <- selected$feed
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
  out <- .validate_market_bar_assets(exchange, out)
  if (nrow(out) > 0L) {
    exchange$market_events <- data.table::rbindlist(list(exchange$market_events, out), fill = TRUE)
  }
  .feed_store(exchange, selected$key, feed)
  out[]
}

#' Get live feed status
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A list describing feed state.
#' @export
sim_feed_status <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(exchange$feed)) exchange$feed <- sim_feed_config()
  feed <- exchange$feed
  feeds <- .feed_status_table(exchange)
  list(
    symbol = feed$symbol,
    asset_id = feed$asset_id %||% .asset_id_from_symbol(feed$symbol),
    timeframe = feed$timeframe,
    tz = feed$tz,
    feed_mode = feed$feed_mode,
    running = any(feeds$running, na.rm = TRUE),
    last_completed_end = feed$last_completed_end,
    last_price = feed$last_price,
    bars = nrow(exchange$market_events),
    feeds = .feed_records(feeds)
  )
}

#' @keywords internal
.feed_records <- function(x) {
  if (is.null(x) || nrow(x) == 0L) return(list())
  lapply(seq_len(nrow(x)), function(i) {
    row <- as.list(x[i])
    lapply(row, function(value) {
      if (length(value) == 1L) return(value[[1L]])
      value
    })
  })
}

#' @keywords internal
.feed_status_scalar_table <- function(exchange) {
  status <- sim_feed_status(exchange)
  status$feeds <- NULL
  data.table::as.data.table(status)
}

#' @keywords internal
.feed_status_table <- function(exchange) {
  if (is.null(exchange$feeds)) exchange$feeds <- list()
  keys <- union(names(exchange$feeds), as.character(exchange$assets$asset_id[exchange$assets$status != "removed"]))
  if (!length(keys)) {
    return(data.table::data.table(
      symbol = character(),
      asset_id = integer(),
      timeframe = character(),
      tz = character(),
      feed_mode = character(),
      running = logical(),
      start_price = numeric(),
      drift = numeric(),
      vol = numeric(),
      seed = integer(),
      last_completed_end = as.POSIXct(character()),
      last_price = numeric()
    ))
  }
  rows <- lapply(keys, function(key) {
    selected <- .feed_select(exchange, asset_id = as.integer(key))
    feed <- selected$feed
    data.table::data.table(
      symbol = as.character(feed$symbol),
      asset_id = as.integer(feed$asset_id %||% key),
      timeframe = as.character(feed$timeframe),
      tz = as.character(feed$tz),
      feed_mode = as.character(feed$feed_mode),
      running = isTRUE(feed$running),
      start_price = as.numeric(feed$random_walk$start_price %||% NA_real_),
      drift = as.numeric(feed$random_walk$drift %||% NA_real_),
      vol = as.numeric(feed$random_walk$vol %||% NA_real_),
      seed = as.integer(feed$random_walk$seed %||% NA_integer_),
      last_completed_end = feed$last_completed_end,
      last_price = as.numeric(feed$last_price %||% NA_real_)
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

#' @keywords internal
.feed_select <- function(exchange, symbol = NULL, asset_id = NULL) {
  if (!is.null(symbol) || !is.null(asset_id)) {
    asset <- .asset_require_registered(exchange, symbol = symbol, asset_id = asset_id, context = "feed asset")
    key <- as.character(asset$asset_id)
    if (!is.null(exchange$feeds[[key]])) {
      feed <- exchange$feeds[[key]]
    } else {
      feed <- sim_feed_config(
        symbol = asset$symbol,
        asset_id = asset$asset_id,
        random_walk = list(seed = .feed_asset_seed(asset$asset_id))
      )
      feed$last_price <- as.numeric(feed$random_walk$start_price)
      if (is.null(exchange$feeds)) exchange$feeds <- list()
      exchange$feeds[[key]] <- feed
    }
    exchange$feed <- feed
    return(list(key = key, feed = feed))
  }
  if (is.null(exchange$feed)) exchange$feed <- sim_feed_config()
  key <- as.character(exchange$feed$asset_id %||% .asset_id_from_symbol(exchange$feed$symbol))
  if (is.null(exchange$feeds)) exchange$feeds <- list()
  if (is.null(exchange$feeds[[key]])) exchange$feeds[[key]] <- exchange$feed
  list(key = key, feed = exchange$feeds[[key]])
}

#' @keywords internal
.feed_request_keys <- function(exchange, symbol = NULL, asset_id = NULL) {
  if (!is.null(symbol) || !is.null(asset_id)) {
    asset <- .asset_require_registered(exchange, symbol = symbol, asset_id = asset_id, context = "feed asset")
    return(as.character(asset$asset_id))
  }
  if (is.null(exchange$feeds)) exchange$feeds <- list()
  keys <- names(exchange$feeds)
  asset_keys <- as.character(exchange$assets$asset_id[exchange$assets$status == "active"])
  keys <- union(keys, asset_keys)
  if (length(keys)) return(keys)
  if (!is.null(exchange$feed)) return(as.character(exchange$feed$asset_id %||% .asset_id_from_symbol(exchange$feed$symbol)))
  character()
}

#' @keywords internal
.feed_store <- function(exchange, key, feed) {
  if (is.null(exchange$feeds)) exchange$feeds <- list()
  exchange$feeds[[as.character(key)]] <- feed
  exchange$feed <- feed
  invisible(feed)
}

#' @keywords internal
.feed_get_bar <- function(feed, start, end) {
  if (identical(feed$feed_mode, "external")) {
    bar <- feed$feed_adapter(feed$symbol, feed$timeframe, start, end, tz = feed$tz)
    return(as_market_bars(bar, symbol = feed$symbol, asset_id = feed$asset_id %||% .asset_id_from_symbol(feed$symbol)))
  }
  .feed_random_walk_bar(feed, start, end)
}

#' @keywords internal
.feed_random_walk_bar <- function(feed, start, end) {
  rw <- feed$random_walk
  seed <- as.integer(rw$seed %||% 1L)
  key <- as.integer(as.numeric(start) / max(1, .feed_parse_timeframe(feed$timeframe)))
  set.seed(.feed_effective_seed(seed, feed$asset_id, key))
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
    symbol = as.character(feed$symbol),
    asset_id = as.integer(feed$asset_id %||% .asset_id_from_symbol(feed$symbol)),
    open = open,
    high = high,
    low = low,
    close = close
  )
}

#' @keywords internal
.feed_asset_seed <- function(asset_id) {
  id <- abs(as.numeric(asset_id %||% 0))
  out <- (id * 1009 + 17) %% (.Machine$integer.max - 1)
  as.integer(if (out <= 0) 1L else out)
}

#' @keywords internal
.feed_effective_seed <- function(seed, asset_id, key) {
  seed <- abs(as.numeric(seed %||% 1))
  asset_seed <- .feed_asset_seed(asset_id)
  key <- abs(as.numeric(key %||% 0))
  out <- (seed + asset_seed + key * 9176) %% (.Machine$integer.max - 1)
  as.integer(if (out <= 0) 1L else out)
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
