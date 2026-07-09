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
#' @param simulation_model Simulation model: `random_walk`, `ar`, `garch11`,
#'   or `ar_garch`.
#' @param random_walk List of random-walk simulation settings: `start_price`,
#'   `drift`, `vol`, and `seed`.
#' @param simulation Advanced simulation settings. Supported nested lists are
#'   `ar`, `garch11`, and `ohlc`.
#' @return A list suitable for `sim_feed_configure()`.
#' @export
sim_feed_config <- function(symbol = "BTC-USDT-SWAP",
                            asset_id = NULL,
                            timeframe = "4h",
                            tz = "UTC",
                            feed_mode = c("simulation", "external"),
                            feed_adapter = NULL,
                            start_time = NULL,
                            simulation_model = c("random_walk", "ar", "garch11", "ar_garch"),
                            random_walk = list(start_price = 100, drift = 0, vol = 0.02, seed = 1L),
                            simulation = list()) {
  feed_mode <- match.arg(feed_mode)
  simulation_model <- match.arg(simulation_model)
  list(
    symbol = symbol,
    asset_id = asset_id %||% .asset_id_from_symbol(symbol),
    timeframe = timeframe,
    tz = tz,
    feed_mode = feed_mode,
    feed_adapter = feed_adapter,
    start_time = start_time,
    simulation_model = simulation_model,
    random_walk = utils::modifyList(list(start_price = 100, drift = 0, vol = 0.02, seed = 1L), random_walk),
    simulation = utils::modifyList(.feed_default_simulation_config(), simulation),
    simulation_state = NULL,
    running = FALSE,
    last_completed_end = NULL,
    last_price = NULL
  )
}

#' Configure market-level multi-asset simulation
#'
#' @param model Market model. `independent` keeps per-asset feeds independent.
#'   Other values generate synchronized multi-asset return batches.
#' @param corr Optional static correlation matrix.
#' @param cov Optional static covariance matrix. If supplied, it takes
#'   precedence over `corr` and per-asset `vol`.
#' @param factors Optional factor model settings for `factor_random_walk`.
#' @param regimes Optional regime settings for `regime_random_walk`.
#' @param seed Base random seed for synchronized draws.
#' @param timeframe Optional common timeframe. If omitted, all selected feeds
#'   must share the same timeframe.
#' @param tz Optional common time zone. If omitted, all selected feeds must
#'   share the same time zone.
#' @param start_time Optional first completed boundary to process.
#' @return A market model configuration list.
#' @export
sim_market_model_config <- function(model = c(
                                      "independent",
                                      "multi_asset_random_walk",
                                      "multi_asset_ar_garch",
                                      "factor_random_walk",
                                      "regime_random_walk"
                                    ),
                                    corr = NULL,
                                    cov = NULL,
                                    factors = NULL,
                                    regimes = NULL,
                                    seed = 1L,
                                    timeframe = NULL,
                                    tz = NULL,
                                    start_time = NULL) {
  model <- match.arg(model)
  list(
    model = model,
    corr = corr,
    cov = cov,
    factors = factors,
    regimes = regimes,
    seed = as.integer(seed %||% 1L),
    timeframe = timeframe,
    tz = tz,
    start_time = start_time,
    running = FALSE,
    last_completed_end = NULL,
    state = list()
  )
}

#' Configure a market-level simulation model
#'
#' @param exchange A `tradesimr_exchange`.
#' @param config A list from `sim_market_model_config()`.
#' @return The market model configuration, invisibly.
#' @export
sim_market_model_configure <- function(exchange, config = sim_market_model_config()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  previous <- exchange$market_model %||% sim_market_model_config()
  supplied <- names(config)
  config <- utils::modifyList(sim_market_model_config(), config)
  config$model <- .market_model_match(config$model)
  for (field in c("running", "last_completed_end")) {
    if (!field %in% supplied) config[[field]] <- previous[[field]]
  }
  if (!"state" %in% supplied && identical(config$model, previous$model)) config$state <- previous$state
  exchange$market_model <- config
  invisible(exchange$market_model)
}

#' Get market-level simulation model status
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A list describing the market-level simulation model.
#' @export
sim_market_model_status <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  model <- exchange$market_model %||% sim_market_model_config()
  list(
    model = model$model %||% "independent",
    running = isTRUE(model$running),
    seed = as.integer(model$seed %||% 1L),
    timeframe = model$timeframe,
    tz = model$tz,
    last_completed_end = model$last_completed_end,
    has_corr = !is.null(model$corr),
    has_cov = !is.null(model$cov),
    has_factors = !is.null(model$factors),
    has_regimes = !is.null(model$regimes),
    regime = model$state$current_regime %||% NA_integer_
  )
}

#' @keywords internal
.market_model_table <- function(exchange) {
  model <- exchange$market_model %||% sim_market_model_config()
  data.table::data.table(
    model = as.character(model$model %||% "independent"),
    seed = as.integer(model$seed %||% 1L),
    timeframe = as.character(model$timeframe %||% NA_character_),
    tz = as.character(model$tz %||% NA_character_),
    running = isTRUE(model$running),
    last_completed_end = as.POSIXct(model$last_completed_end %||% NA, origin = "1970-01-01", tz = model$tz %||% "UTC"),
    current_regime = as.integer(model$state$current_regime %||% NA_integer_),
    has_corr = !is.null(model$corr),
    has_cov = !is.null(model$cov),
    has_factors = !is.null(model$factors),
    has_regimes = !is.null(model$regimes),
    corr = .serialize_field(model$corr),
    cov = .serialize_field(model$cov),
    factors = .serialize_field(model$factors),
    regimes = .serialize_field(model$regimes),
    state = .serialize_field(model$state)
  )
}

#' @keywords internal
.market_model_from_table <- function(x) {
  x <- data.table::as.data.table(x)
  if (nrow(x) == 0L) return(sim_market_model_config())
  row <- x[1L]
  value <- function(name, default = NA) if (name %in% names(row)) row[[name]][1L] else default
  config <- sim_market_model_config(
    model = value("model", "independent") %||% "independent",
    corr = .unserialize_field(value("corr", NA_character_)),
    cov = .unserialize_field(value("cov", NA_character_)),
    factors = .unserialize_field(value("factors", NA_character_)),
    regimes = .unserialize_field(value("regimes", NA_character_)),
    seed = as.integer(value("seed", 1L) %||% 1L),
    timeframe = .na_null(value("timeframe", NA_character_)),
    tz = .na_null(value("tz", NA_character_)),
    start_time = NULL
  )
  config$running <- .truthy(value("running", FALSE))
  last_completed_end <- value("last_completed_end", NA)
  config$last_completed_end <- if (!is.na(last_completed_end)) as.POSIXct(last_completed_end, tz = config$tz %||% "UTC") else NULL
  config$state <- .unserialize_field(value("state", NA_character_)) %||% list()
  current_regime <- value("current_regime", NA_integer_)
  if (!is.na(current_regime)) config$state$current_regime <- as.integer(current_regime)
  config
}

#' @keywords internal
.serialize_field <- function(x) {
  if (is.null(x)) return(NA_character_)
  rawToChar(serialize(x, NULL, ascii = TRUE))
}

#' @keywords internal
.unserialize_field <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(x)) return(NULL)
  unserialize(charToRaw(as.character(x)))
}

#' @keywords internal
.na_null <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(as.character(x))) return(NULL)
  as.character(x)
}

#' @keywords internal
.truthy <- function(x) {
  identical(x, TRUE) || identical(x, 1L) || identical(x, 1) || as.character(x) %in% c("TRUE", "true", "1")
}

#' Configure a live exchange feed
#'
#' @param exchange A `tradesimr_exchange`.
#' @param config Feed configuration list. A list with `configs` may be used to
#'   configure all registered assets in one call. A `market_model` element may
#'   be used to configure synchronized multi-asset simulation.
#' @return The feed configuration, invisibly.
#' @export
sim_feed_configure <- function(exchange, config = sim_feed_config()) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (!is.null(config$market_model)) {
    sim_market_model_configure(exchange, as.list(config$market_model))
    config$market_model <- NULL
    if (!length(config)) return(invisible(exchange$market_model))
  }
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
    if (!any(c("simulation_model", "simulation", "random_walk") %in% supplied)) {
      config$simulation_state <- previous$simulation_state
    }
  }
  config$feed_mode <- match.arg(config$feed_mode, c("simulation", "external"))
  config$simulation_model <- match.arg(config$simulation_model, c("random_walk", "ar", "garch11", "ar_garch"))
  config$simulation <- utils::modifyList(.feed_default_simulation_config(), config$simulation %||% list())
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
  if (.market_model_active(exchange) && is.null(symbol) && is.null(asset_id)) {
    .market_model_start(exchange, now = now)
  }
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
  if (.market_model_active(exchange) && is.null(symbol) && is.null(asset_id)) {
    exchange$market_model$running <- FALSE
  }
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
  if (.market_model_active(exchange) && is.null(symbol) && is.null(asset_id)) {
    return(.market_model_step(exchange, now = now, max_bars = max_bars))
  }
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
    generated <- .feed_get_bar_result(feed, start, end)
    bars[[i]] <- generated$bar
    feed <- generated$feed
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
  if (.market_model_active(exchange) && is.null(symbol) && is.null(asset_id)) {
    return(.market_model_warmup(exchange, n_bars = n_bars, now = now))
  }
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
    generated <- .feed_get_bar_result(feed, start, end)
    bars[[i]] <- generated$bar
    feed <- generated$feed
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
    simulation_model = feed$simulation_model,
    running = any(feeds$running, na.rm = TRUE),
    last_completed_end = feed$last_completed_end,
    last_price = feed$last_price,
    bars = nrow(exchange$market_events),
    market_model = sim_market_model_status(exchange),
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
  status$market_model <- NULL
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
      simulation_model = character(),
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
      simulation_model = as.character(feed$simulation_model %||% "random_walk"),
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
.market_model_active <- function(exchange) {
  model <- exchange$market_model %||% sim_market_model_config()
  !identical(as.character(model$model %||% "independent"), "independent")
}

#' @keywords internal
.market_model_match <- function(model) {
  match.arg(as.character(model %||% "independent"), c(
    "independent",
    "multi_asset_random_walk",
    "multi_asset_ar_garch",
    "factor_random_walk",
    "regime_random_walk"
  ))
}

#' @keywords internal
.market_model_start <- function(exchange, now = Sys.time()) {
  model <- exchange$market_model %||% sim_market_model_config()
  spec <- .market_model_spec(exchange, model)
  model$running <- TRUE
  now_model <- .feed_as_time(now, spec$tz)
  if (is.null(model$last_completed_end)) {
    start_time <- model$start_time
    model$last_completed_end <- if (is.null(start_time)) {
      .feed_latest_completed_end(now_model, spec$timeframe, spec$tz)
    } else {
      .feed_as_time(start_time, spec$tz)
    }
  }
  exchange$market_model <- model
  invisible(model)
}

#' @keywords internal
.market_model_step <- function(exchange, now = Sys.time(), max_bars = Inf) {
  model <- exchange$market_model %||% sim_market_model_config()
  spec <- .market_model_spec(exchange, model)
  now <- .feed_as_time(now, spec$tz)
  latest_end <- .feed_latest_completed_end(now, spec$timeframe, spec$tz)
  if (is.null(model$last_completed_end)) {
    model$last_completed_end <- if (is.null(model$start_time)) latest_end else .feed_as_time(model$start_time, spec$tz)
  }
  step_seconds <- .feed_parse_timeframe(spec$timeframe)
  if (as.numeric(model$last_completed_end) > as.numeric(latest_end) - step_seconds) {
    exchange$market_model <- model
    return(sim_schemas()$market_events[0])
  }
  starts <- seq(
    from = as.numeric(model$last_completed_end),
    to = as.numeric(latest_end) - step_seconds,
    by = step_seconds
  )
  if (!length(starts) || starts[1] > as.numeric(latest_end) - step_seconds) {
    exchange$market_model <- model
    return(sim_schemas()$market_events[0])
  }
  if (is.finite(max_bars)) starts <- utils::head(starts, max_bars)
  bars <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    start <- as.POSIXct(starts[i], origin = "1970-01-01", tz = spec$tz)
    end <- as.POSIXct(starts[i] + step_seconds, origin = "1970-01-01", tz = spec$tz)
    step_spec <- .market_model_spec(exchange, model)
    generated <- .market_model_generate_batch(exchange, model, step_spec, start, end)
    bars[[i]] <- generated$bars
    model <- generated$model
    model$last_completed_end <- end
    if (nrow(bars[[i]]) > 0L) {
      sim_agents_step(exchange, bars[[i]])
      sim_exchange_process_commands(exchange)
      sim_exchange_step(exchange, bars[[i]])
    }
  }
  exchange$market_model <- model
  data.table::rbindlist(bars, fill = TRUE)[]
}

#' @keywords internal
.market_model_warmup <- function(exchange, n_bars = 100L, now = Sys.time()) {
  n_bars <- as.integer(n_bars)
  if (is.na(n_bars) || n_bars <= 0L) return(sim_schemas()$market_events[0])
  model <- exchange$market_model %||% sim_market_model_config()
  spec <- .market_model_spec(exchange, model)
  step_seconds <- .feed_parse_timeframe(spec$timeframe)
  latest_end <- .feed_latest_completed_end(.feed_as_time(now, spec$tz), spec$timeframe, spec$tz)
  starts <- seq(
    from = as.numeric(latest_end) - step_seconds * n_bars,
    to = as.numeric(latest_end) - step_seconds,
    by = step_seconds
  )
  if (!length(starts)) return(sim_schemas()$market_events[0])
  bars <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    start <- as.POSIXct(starts[i], origin = "1970-01-01", tz = spec$tz)
    end <- as.POSIXct(starts[i] + step_seconds, origin = "1970-01-01", tz = spec$tz)
    step_spec <- .market_model_spec(exchange, model)
    generated <- .market_model_generate_batch(exchange, model, step_spec, start, end)
    bars[[i]] <- generated$bars
    model <- generated$model
    model$last_completed_end <- end
  }
  out <- data.table::rbindlist(bars, fill = TRUE)
  out <- .validate_market_bar_assets(exchange, out)
  if (nrow(out) > 0L) {
    exchange$market_events <- data.table::rbindlist(list(exchange$market_events, out), fill = TRUE)
  }
  exchange$market_model <- model
  out[]
}

#' @keywords internal
.market_model_spec <- function(exchange, model) {
  keys <- .feed_request_keys(exchange)
  if (length(keys) < 2L) {
    stop("`multi_asset_random_walk` requires at least two active/configured assets.", call. = FALSE)
  }
  selected <- lapply(keys, function(key) .feed_select(exchange, asset_id = as.integer(key))$feed)
  bad_mode <- vapply(selected, function(feed) !identical(feed$feed_mode, "simulation"), logical(1))
  if (any(bad_mode)) {
    stop("`multi_asset_random_walk` requires all selected feeds to use `feed_mode = 'simulation'`.", call. = FALSE)
  }
  timeframes <- unique(vapply(selected, function(feed) as.character(feed$timeframe), character(1)))
  tzs <- unique(vapply(selected, function(feed) as.character(feed$tz), character(1)))
  timeframe <- model$timeframe %||% if (length(timeframes) == 1L) timeframes else NULL
  tz <- model$tz %||% if (length(tzs) == 1L) tzs else NULL
  if (is.null(timeframe)) stop("Set `market_model$timeframe` when selected feeds have different timeframes.", call. = FALSE)
  if (is.null(tz)) stop("Set `market_model$tz` when selected feeds have different time zones.", call. = FALSE)
  feeds <- data.table::rbindlist(lapply(selected, function(feed) {
    data.table::data.table(
      symbol = as.character(feed$symbol),
      asset_id = as.integer(feed$asset_id),
      drift = as.numeric(feed$random_walk$drift %||% 0),
      vol = abs(as.numeric(feed$random_walk$vol %||% 0.02)),
      start_price = as.numeric(feed$random_walk$start_price %||% 100),
      last_price = as.numeric(feed$last_price %||% feed$random_walk$start_price %||% 100)
    )
  }), fill = TRUE)
  data.table::setorder(feeds, asset_id)
  list(keys = as.character(feeds$asset_id), feeds = feeds, timeframe = timeframe, tz = tz)
}

#' @keywords internal
.market_model_generate_batch <- function(exchange, model, spec, start, end) {
  feeds <- spec$feeds
  seed <- as.integer(model$seed %||% 1L)
  key <- as.integer(as.numeric(start) / max(1, .feed_parse_timeframe(spec$timeframe)))
  set.seed(.feed_effective_seed(seed, sum(feeds$asset_id), key))
  generated_returns <- .market_model_returns(exchange, model, feeds)
  returns <- generated_returns$returns
  model <- generated_returns$model
  bars <- feeds[, .(
    timestamp = start,
    symbol,
    asset_id,
    open = last_price,
    high = NA_real_,
    low = NA_real_,
    close = pmax(.Machine$double.eps, last_price * exp(returns))
  )]
  wiggle <- abs(stats::rnorm(nrow(bars) * 2L, mean = 0, sd = rep(feeds$vol * 0.5, each = 2L)))
  wiggle <- matrix(wiggle, ncol = 2L, byrow = TRUE)
  bars[, high := pmax(open, close) * (1 + wiggle[, 1L])]
  bars[, low := pmin(open, close) * pmax(.Machine$double.eps, 1 - wiggle[, 2L])]
  for (i in seq_len(nrow(bars))) {
    selected <- .feed_select(exchange, asset_id = bars$asset_id[i])
    feed <- selected$feed
    feed$last_price <- bars$close[i]
    feed$last_completed_end <- end
    .feed_store(exchange, selected$key, feed)
  }
  list(bars = bars, model = model)
}

#' @keywords internal
.market_model_returns <- function(exchange, model, feeds) {
  model$model <- .market_model_match(model$model)
  if (model$model == "multi_asset_random_walk") {
    cov <- .market_model_covariance(model, feeds)
    returns <- as.numeric(feeds$drift + crossprod(.market_model_chol(cov), stats::rnorm(nrow(feeds))))
    return(list(returns = returns, model = model))
  }
  if (model$model == "multi_asset_ar_garch") {
    return(.market_model_ar_garch_returns(exchange, model, feeds))
  }
  if (model$model == "factor_random_walk") {
    return(.market_model_factor_returns(model, feeds))
  }
  if (model$model == "regime_random_walk") {
    return(.market_model_regime_returns(model, feeds))
  }
  stop("Unsupported market model: ", model$model, call. = FALSE)
}

#' @keywords internal
.market_model_ar_garch_returns <- function(exchange, model, feeds) {
  n <- nrow(feeds)
  sigmas <- numeric(n)
  ar_terms <- numeric(n)
  states <- vector("list", n)
  feed_list <- vector("list", n)
  for (i in seq_len(n)) {
    selected <- .feed_select(exchange, asset_id = feeds$asset_id[i])
    feed <- selected$feed
    cfg <- utils::modifyList(.feed_default_simulation_config(), feed$simulation %||% list())
    state <- .feed_simulation_state(feed)
    ar <- cfg$ar
    g <- cfg$garch11
    a <- as.numeric(ar$a %||% 0.25)
    ar_terms[i] <- sum(a * .feed_lags(state$ret_lags, length(a)), na.rm = TRUE)
    sigma2 <- .feed_garch_sigma2(state, g, feeds$vol[i])
    sigmas[i] <- sqrt(sigma2)
    states[[i]] <- list(state = state, g = g, sigma2 = sigma2, key = selected$key)
    feed_list[[i]] <- feed
  }
  corr <- if (!is.null(model$corr)) .market_model_matrix(model$corr, n, "corr") else diag(n)
  cov <- diag(sigmas, nrow = n) %*% corr %*% diag(sigmas, nrow = n)
  shocks <- as.numeric(crossprod(.market_model_chol(cov), stats::rnorm(n)))
  returns <- feeds$drift + ar_terms + shocks
  for (i in seq_len(n)) {
    feed <- feed_list[[i]]
    state <- states[[i]]$state
    state$sigma2 <- .feed_garch_next_sigma2(states[[i]]$g, shocks[i], states[[i]]$sigma2, feeds$vol[i])
    state$ret_lags <- c(returns[i], utils::head(state$ret_lags, 9L))
    state$shock_lags <- c(shocks[i], utils::head(state$shock_lags, 9L))
    feed$simulation_state <- state
    .feed_store(exchange, states[[i]]$key, feed)
  }
  list(returns = returns, model = model)
}

#' @keywords internal
.market_model_factor_returns <- function(model, feeds) {
  n <- nrow(feeds)
  factors <- model$factors %||% list()
  k <- as.integer(factors$n_factors %||% 1L)
  loadings <- if (!is.null(factors$loadings)) {
    .market_model_loadings(factors$loadings, n)
  } else {
    matrix(rep(0.7, n * k), nrow = n, ncol = k)
  }
  k <- ncol(loadings)
  factor_vol <- rep_len(as.numeric(factors$factor_vol %||% 0.01), k)
  idio_vol <- rep_len(as.numeric(factors$idio_vol %||% feeds$vol), n)
  factor_shocks <- stats::rnorm(k, sd = factor_vol)
  idio_shocks <- stats::rnorm(n, sd = idio_vol)
  returns <- as.numeric(feeds$drift + loadings %*% factor_shocks + idio_shocks)
  list(returns = returns, model = model)
}

#' @keywords internal
.market_model_regime_returns <- function(model, feeds) {
  n <- nrow(feeds)
  regimes <- .market_model_regimes(model$regimes, n)
  current <- as.integer(model$state$current_regime %||% regimes$initial_state)
  transition <- regimes$transition
  probs <- transition[current, ]
  next_state <- sample.int(nrow(transition), 1L, prob = probs)
  model$state$current_regime <- next_state
  state <- regimes$states[[next_state]]
  drift <- feeds$drift + rep_len(as.numeric(state$drift %||% 0), n)
  vol_multiplier <- rep_len(as.numeric(state$vol_multiplier %||% 1), n)
  state_feeds <- data.table::copy(feeds)
  state_feeds[, vol := vol * vol_multiplier]
  state_model <- model
  state_model$corr <- state$corr %||% model$corr
  state_model$cov <- state$cov %||% NULL
  cov <- .market_model_covariance(state_model, state_feeds)
  returns <- as.numeric(drift + crossprod(.market_model_chol(cov), stats::rnorm(n)))
  list(returns = returns, model = model)
}

#' @keywords internal
.market_model_covariance <- function(model, feeds) {
  n <- nrow(feeds)
  if (!is.null(model$cov)) {
    cov <- .market_model_matrix(model$cov, n, "cov")
  } else {
    corr <- if (!is.null(model$corr)) .market_model_matrix(model$corr, n, "corr") else diag(n)
    cov <- diag(feeds$vol, nrow = n) %*% corr %*% diag(feeds$vol, nrow = n)
  }
  if (!isSymmetric(cov, tol = 1e-8)) stop("Market model covariance/correlation must be symmetric.", call. = FALSE)
  cov
}

#' @keywords internal
.market_model_matrix <- function(x, n, name) {
  mat <- if (is.matrix(x)) x else matrix(as.numeric(unlist(x, use.names = FALSE)), nrow = n, byrow = TRUE)
  if (!identical(dim(mat), c(n, n))) {
    stop("Market model `", name, "` must be a ", n, "x", n, " matrix.", call. = FALSE)
  }
  storage.mode(mat) <- "double"
  mat
}

#' @keywords internal
.market_model_loadings <- function(x, n) {
  raw <- as.numeric(unlist(x, use.names = FALSE))
  if (length(raw) %% n != 0L) stop("Factor loadings length must be divisible by the number of assets.", call. = FALSE)
  mat <- if (is.matrix(x)) x else matrix(raw, nrow = n, byrow = TRUE)
  if (nrow(mat) != n) stop("Factor loadings must have one row per asset.", call. = FALSE)
  storage.mode(mat) <- "double"
  mat
}

#' @keywords internal
.market_model_regimes <- function(regimes, n) {
  if (is.null(regimes)) {
    stress_corr <- matrix(0.75, nrow = n, ncol = n)
    diag(stress_corr) <- 1
    return(list(
      initial_state = 1L,
      transition = matrix(c(0.96, 0.04, 0.20, 0.80), nrow = 2, byrow = TRUE),
      states = list(
        list(name = "calm", corr = diag(n), vol_multiplier = 1),
        list(name = "stress", corr = stress_corr, vol_multiplier = 2.5)
      )
    ))
  }
  out <- as.list(regimes)
  states <- out$states %||% list()
  if (!length(states)) stop("Regime model requires at least one state.", call. = FALSE)
  states <- lapply(states, as.list)
  transition <- if (!is.null(out$transition)) {
    .market_model_matrix(out$transition, length(states), "transition")
  } else {
    diag(length(states))
  }
  row_sums <- rowSums(transition)
  if (any(row_sums <= 0 | !is.finite(row_sums))) stop("Regime transition rows must have positive finite sums.", call. = FALSE)
  transition <- transition / row_sums
  initial_state <- as.integer(out$initial_state %||% 1L)
  if (is.na(initial_state) || initial_state < 1L || initial_state > length(states)) {
    stop("Regime initial_state must identify a configured state.", call. = FALSE)
  }
  list(initial_state = initial_state, transition = transition, states = states)
}

#' @keywords internal
.market_model_chol <- function(cov) {
  tryCatch(chol(cov), error = function(e) {
    jitter <- diag(max(diag(cov), 1e-8) * 1e-8, nrow(cov))
    chol(cov + jitter)
  })
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
  .feed_get_bar_result(feed, start, end)$bar
}

#' @keywords internal
.feed_get_bar_result <- function(feed, start, end) {
  if (identical(feed$feed_mode, "external")) {
    bar <- feed$feed_adapter(feed$symbol, feed$timeframe, start, end, tz = feed$tz)
    bar <- as_market_bars(bar, symbol = feed$symbol, asset_id = feed$asset_id %||% .asset_id_from_symbol(feed$symbol))
    return(list(bar = bar, feed = feed))
  }
  .feed_simulation_bar(feed, start, end)
}

#' @keywords internal
.feed_simulation_bar <- function(feed, start, end) {
  rw <- feed$random_walk
  seed <- as.integer(rw$seed %||% 1L)
  key <- as.integer(as.numeric(start) / max(1, .feed_parse_timeframe(feed$timeframe)))
  set.seed(.feed_effective_seed(seed, feed$asset_id, key))
  open <- as.numeric(feed$last_price %||% rw$start_price %||% 100)
  simulated <- .feed_simulation_return(feed)
  ret <- simulated$ret
  feed <- simulated$feed
  vol <- abs(as.numeric(simulated$sigma %||% rw$vol %||% 0.02))
  close <- max(.Machine$double.eps, open * exp(ret))
  wiggle_scale <- as.numeric((feed$simulation %||% list())$ohlc$wiggle_scale %||% 0.5)
  wiggle <- abs(stats::rnorm(2L, mean = 0, sd = vol * wiggle_scale))
  high <- max(open, close) * (1 + wiggle[1])
  low <- min(open, close) * max(.Machine$double.eps, 1 - wiggle[2])
  bar <- data.table::data.table(
    timestamp = start,
    symbol = as.character(feed$symbol),
    asset_id = as.integer(feed$asset_id %||% .asset_id_from_symbol(feed$symbol)),
    open = open,
    high = high,
    low = low,
    close = close
  )
  list(bar = bar, feed = feed)
}

#' @keywords internal
.feed_simulation_return <- function(feed) {
  rw <- feed$random_walk
  model <- as.character(feed$simulation_model %||% "random_walk")
  cfg <- utils::modifyList(.feed_default_simulation_config(), feed$simulation %||% list())
  state <- .feed_simulation_state(feed)
  drift <- as.numeric(rw$drift %||% 0)
  vol <- abs(as.numeric(rw$vol %||% 0.02))

  if (model == "random_walk") {
    shock <- stats::rnorm(1L, mean = 0, sd = vol)
    ret <- drift + shock
    sigma <- vol
  } else if (model == "ar") {
    ar <- cfg$ar
    a <- as.numeric(ar$a %||% 0.25)
    sigma <- abs(as.numeric(ar$sigma %||% vol))
    shock <- stats::rnorm(1L, mean = 0, sd = sigma)
    ret <- drift + sum(a * .feed_lags(state$ret_lags, length(a)), na.rm = TRUE) + shock
  } else if (model == "garch11") {
    g <- cfg$garch11
    sigma2 <- .feed_garch_sigma2(state, g, vol)
    z <- .feed_draw_standardized(g)
    shock <- sqrt(sigma2) * z
    ret <- drift + shock
    sigma <- sqrt(sigma2)
    state$sigma2 <- .feed_garch_next_sigma2(g, shock, sigma2, vol)
  } else if (model == "ar_garch") {
    ar <- cfg$ar
    g <- cfg$garch11
    a <- as.numeric(ar$a %||% 0.25)
    sigma2 <- .feed_garch_sigma2(state, g, vol)
    z <- .feed_draw_standardized(g)
    shock <- sqrt(sigma2) * z
    ret <- drift + sum(a * .feed_lags(state$ret_lags, length(a)), na.rm = TRUE) + shock
    sigma <- sqrt(sigma2)
    state$sigma2 <- .feed_garch_next_sigma2(g, shock, sigma2, vol)
  } else {
    stop("Unsupported simulation model: ", model, call. = FALSE)
  }

  state$ret_lags <- c(ret, utils::head(state$ret_lags, 9L))
  state$shock_lags <- c(shock, utils::head(state$shock_lags, 9L))
  feed$simulation_state <- state
  list(ret = ret, sigma = sigma, feed = feed)
}

#' @keywords internal
.feed_default_simulation_config <- function() {
  list(
    ar = list(a = 0.25, sigma = NULL),
    garch11 = list(alpha0 = NULL, alpha1 = 0.08, beta1 = 0.90, sigma2_0 = NULL, z_dist = "norm", df = 8),
    ohlc = list(wiggle_scale = 0.5)
  )
}

#' @keywords internal
.feed_simulation_state <- function(feed) {
  state <- feed$simulation_state %||% list()
  state$ret_lags <- as.numeric(state$ret_lags %||% numeric())
  state$shock_lags <- as.numeric(state$shock_lags %||% numeric())
  if (!is.null(state$sigma2)) state$sigma2 <- as.numeric(state$sigma2)
  state
}

#' @keywords internal
.feed_garch_sigma2 <- function(state, g, vol) {
  alpha1 <- as.numeric(g$alpha1 %||% 0.08)
  beta1 <- as.numeric(g$beta1 %||% 0.90)
  alpha0 <- as.numeric(g$alpha0 %||% (vol * vol * max(1e-8, 1 - alpha1 - beta1)))
  sigma2 <- as.numeric(state$sigma2 %||% g$sigma2_0 %||% NA_real_)
  if (!is.finite(sigma2) || sigma2 <= 0) {
    denom <- 1 - alpha1 - beta1
    sigma2 <- if (is.finite(denom) && denom > 0) alpha0 / denom else vol * vol
  }
  max(.Machine$double.eps, sigma2)
}

#' @keywords internal
.feed_garch_next_sigma2 <- function(g, shock, sigma2, vol) {
  alpha1 <- as.numeric(g$alpha1 %||% 0.08)
  beta1 <- as.numeric(g$beta1 %||% 0.90)
  alpha0 <- as.numeric(g$alpha0 %||% (vol * vol * max(1e-8, 1 - alpha1 - beta1)))
  out <- alpha0 + alpha1 * shock * shock + beta1 * sigma2
  max(.Machine$double.eps, out)
}

#' @keywords internal
.feed_lags <- function(x, n) {
  if (n <= 0L) return(numeric())
  out <- numeric(n)
  x <- as.numeric(x %||% numeric())
  if (length(x) > 0L) out[seq_len(min(n, length(x)))] <- utils::head(x, n)
  out
}

#' @keywords internal
.feed_draw_standardized <- function(g) {
  z_dist <- match.arg(as.character(g$z_dist %||% "norm"), c("norm", "stdt"))
  if (z_dist == "norm") return(stats::rnorm(1L))
  df <- as.numeric(g$df %||% 8)
  if (!is.finite(df) || df <= 2) stop("Student-t GARCH innovations require `df > 2`.", call. = FALSE)
  stats::rt(1L, df = df) / sqrt(df / (df - 2))
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
