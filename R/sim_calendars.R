#' List built-in trading calendars
#'
#' @return A data.table describing the deterministic built-in session rules.
#' @export
sim_trading_calendars <- function() {
  data.table::data.table(
    calendar_id = c("ALWAYS_OPEN", "CRYPTO_24_7", "FX_24_5", "XNYS", "CME"),
    timezone = c("UTC", "UTC", "UTC", "America/New_York", "America/Chicago"),
    session_rule = c("24x7", "24x7", "weekdays", "09:30-16:00 weekdays", "Sun 17:00-Fri 16:00 CT")
  )
}

#' Get a trading-calendar specification
#'
#' @param calendar_id Built-in calendar identifier.
#' @return A one-row data.table with timezone, session rule, and holiday policy.
#' @export
sim_calendar_spec <- function(calendar_id) {
  requested_calendar_id <- toupper(as.character(calendar_id))
  out <- sim_trading_calendars()[["calendar_id"]] == requested_calendar_id
  out <- sim_trading_calendars()[out]
  if (nrow(out) != 1L) stop("Unknown calendar_id: ", requested_calendar_id, call. = FALSE)
  out[, `:=`(
    holiday_policy = ifelse(calendar_id == "XNYS", "US equity regular and observed holidays", "session only"),
    early_close_policy = ifelse(calendar_id == "XNYS", "Black Friday 13:00 ET", "none")
  )]
  out
}

#' List deterministic built-in calendar holidays
#'
#' @param calendar_id Built-in calendar identifier.
#' @param start,end Date/POSIXct bounds.
#' @return A data.table of session dates, labels, and optional early close time.
#' @export
sim_calendar_holidays <- function(calendar_id, start, end) {
  calendar_id <- toupper(as.character(calendar_id))
  dates <- seq(as.Date(start), as.Date(end), by = "day")
  if (!length(dates) || calendar_id != "XNYS") {
    return(data.table::data.table(session_date = as.Date(character()), label = character(), action = character(), close_time = character()))
  }
  years <- unique(as.integer(format(dates, "%Y")))
  observed <- function(date) {
    weekday <- as.POSIXlt(date)$wday
    date + ifelse(weekday == 6L, -1L, ifelse(weekday == 0L, 1L, 0L))
  }
  nth_weekday <- function(year, month, weekday, nth) {
    first <- as.Date(sprintf("%04d-%02d-01", year, month))
    first + ((weekday - as.POSIXlt(first)$wday + 7L) %% 7L) + 7L * (nth - 1L)
  }
  last_weekday <- function(year, month, weekday) {
    next_month <- if (month == 12L) as.Date(sprintf("%04d-01-01", year + 1L)) else as.Date(sprintf("%04d-%02d-01", year, month + 1L))
    last <- next_month - 1L
    last - ((as.POSIXlt(last)$wday - weekday + 7L) %% 7L)
  }
  easter <- function(year) {
    a <- year %% 19L; b <- year %/% 100L; c <- year %% 100L; d <- b %/% 4L; e <- b %% 4L
    f <- (b + 8L) %/% 25L; g <- (b - f + 1L) %/% 3L; h <- (19L * a + b - d - g + 15L) %% 30L
    i <- c %/% 4L; k <- c %% 4L; l <- (32L + 2L * e + 2L * i - h - k) %% 7L; m <- (a + 11L * h + 22L * l) %/% 451L
    as.Date(sprintf("%04d-%02d-%02d", year, (h + l - 7L * m + 114L) %/% 31L, (h + l - 7L * m + 114L) %% 31L + 1L))
  }
  closed <- data.table::rbindlist(lapply(years, function(year) data.table::data.table(
    session_date = c(observed(as.Date(sprintf("%04d-01-01", year))), nth_weekday(year, 1L, 1L, 3L),
      nth_weekday(year, 2L, 1L, 3L), easter(year) - 2L, last_weekday(year, 5L, 1L),
      observed(as.Date(sprintf("%04d-06-19", year))), observed(as.Date(sprintf("%04d-07-04", year))),
      nth_weekday(year, 9L, 1L, 1L), nth_weekday(year, 11L, 4L, 4L), observed(as.Date(sprintf("%04d-12-25", year)))),
    label = c("New Year", "MLK Day", "Presidents Day", "Good Friday", "Memorial Day", "Juneteenth", "Independence Day", "Labor Day", "Thanksgiving", "Christmas"),
    action = "closed", close_time = NA_character_
  )), fill = TRUE)
  early <- closed[label == "Thanksgiving", .(session_date = session_date + 1L, label = "Black Friday", action = "early_close", close_time = "13:00")]
  out <- data.table::rbindlist(list(closed, early))
  out[session_date >= as.Date(start) & session_date <= as.Date(end)]
}

#' Generate expected completed bar timestamps from a calendar
#'
#' @param calendar_id Built-in calendar identifier.
#' @param start,end Timestamp bounds.
#' @param cadence_seconds Positive bar cadence in seconds.
#' @param exceptions Optional exception table with `session_date`, `action`, and
#'   `close_time` fields.
#' @return A data.table of expected completed-bar timestamps.
#' @export
sim_calendar_expected_bars <- function(calendar_id, start, end, cadence_seconds, exceptions = NULL) {
  if (!is.finite(cadence_seconds) || cadence_seconds <= 0) stop("`cadence_seconds` must be positive.", call. = FALSE)
  spec <- sim_calendar_spec(calendar_id)
  start <- as.POSIXct(start, tz = "UTC"); end <- as.POSIXct(end, tz = "UTC")
  if (is.na(start) || is.na(end) || end < start) stop("`start` and `end` must be ordered timestamps.", call. = FALSE)
  first <- as.POSIXct(ceiling(as.numeric(start) / cadence_seconds) * cadence_seconds, origin = "1970-01-01", tz = "UTC")
  stamps <- seq(first, end, by = cadence_seconds)
  if (!length(stamps)) return(data.table::data.table(timestamp = as.POSIXct(character()), calendar_id = character()))
  # A completed bar ending at a session close is valid, so inspect one second
  # before its endpoint.
  open <- sim_calendar_is_open(stamps - 1, spec$calendar_id, exceptions = exceptions)
  data.table::data.table(timestamp = stamps[open], calendar_id = spec$calendar_id)
}

#' Calculate a calendar-aware settlement timestamp
#'
#' Settlement lags count tradable calendar dates, not raw 24-hour periods.
#' This gives FX spot its conventional weekday progression while preserving
#' same-day settlement for 24/7 instruments. Exchange-specific closed-date
#' exceptions are respected.
#'
#' @param calendar_id Built-in settlement calendar identifier.
#' @param timestamp Trade timestamp.
#' @param settlement_lag_days Non-negative whole settlement days.
#' @param exceptions Optional calendar-exception rows.
#' @return A UTC `POSIXct` settlement timestamp.
#' @export
sim_calendar_settlement_timestamp <- function(calendar_id, timestamp,
                                               settlement_lag_days = 0L,
                                               exceptions = NULL) {
  lag_days <- as.integer(settlement_lag_days)
  if (length(lag_days) != 1L || is.na(lag_days) || lag_days < 0L) {
    stop("`settlement_lag_days` must be one non-negative integer.", call. = FALSE)
  }
  spec <- sim_calendar_spec(calendar_id)
  timestamp <- as.POSIXct(timestamp, tz = "UTC")
  if (is.na(timestamp)) stop("`timestamp` must be a valid timestamp.", call. = FALSE)
  if (lag_days == 0L) return(timestamp)
  local <- as.POSIXlt(timestamp, tz = spec$timezone)
  date <- as.Date(local)
  clock <- sprintf("%02d:%02d:%02d", local$hour, local$min, local$sec)
  is_settlement_day <- function(candidate) {
    # Inspect noon local time so session-open clock boundaries do not turn an
    # otherwise valid business date into a settlement holiday.
    noon <- as.POSIXct(paste(candidate, "12:00:00"), tz = spec$timezone)
    sim_calendar_is_open(noon, spec$calendar_id, exceptions = exceptions)
  }
  counted <- 0L
  while (counted < lag_days) {
    date <- date + 1L
    if (is_settlement_day(date)) counted <- counted + 1L
  }
  local_due <- as.POSIXct(paste(date, clock), tz = spec$timezone)
  as.POSIXct(as.numeric(local_due), origin = "1970-01-01", tz = "UTC")
}

#' Add an exchange-specific calendar exception
#'
#' @param exchange A `tradesimr_exchange`.
#' @param session_date Local session date.
#' @param action `"closed"` or `"early_close"`.
#' @param calendar_id Optional calendar identifier.
#' @param symbol,asset_id Optional registered-asset scope.
#' @param close_time Required `"HH:MM"` for an early close.
#' @param message Public-safe description.
#' @return Invisibly returns the durable exception row.
#' @export
sim_exchange_calendar_exception <- function(exchange, session_date,
                                            action = c("closed", "early_close"),
                                            calendar_id = NULL, symbol = NULL, asset_id = NULL,
                                            close_time = NULL, message = "") {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  action <- match.arg(action)
  asset <- NULL
  if (!is.null(symbol) || !is.null(asset_id)) {
    index <- .asset_registry_index(exchange, symbol = symbol, asset_id = asset_id)
    if (length(index) != 1L) stop("Exception asset must identify exactly one registered asset.", call. = FALSE)
    asset <- exchange$assets[index]
  }
  if (identical(action, "early_close") && (is.null(close_time) || !grepl("^[0-2][0-9]:[0-5][0-9]$", close_time))) {
    stop("`close_time` must be HH:MM for an early close.", call. = FALSE)
  }
  calendar_id <- toupper(as.character(calendar_id %||% if (is.null(asset)) "XNYS" else exchange$assets[asset_id == asset$asset_id, calendar_id][1L]))
  if (!any(sim_trading_calendars()[["calendar_id"]] == calendar_id)) stop("Unknown calendar_id.", call. = FALSE)
  row <- data.table::data.table(exception_id = paste0("CAL", sprintf("%06d", nrow(exchange$calendar_exceptions) + 1L)),
    calendar_id = calendar_id, asset_id = if (is.null(asset)) NA_integer_ else asset$asset_id,
    symbol = if (is.null(asset)) NA_character_ else asset$symbol, session_date = as.Date(session_date), action = action,
    close_time = if (is.null(close_time)) NA_character_ else close_time, message = as.character(message), created_at = Sys.time())
  exchange$calendar_exceptions <- data.table::rbindlist(list(exchange$calendar_exceptions, row))
  invisible(row)
}

#' Test whether timestamps fall in a built-in tradable session
#'
#' This deliberately provides deterministic session rules, not a vendor
#' holiday database. `XNYS` includes fixed-date observed holidays; callers may
#' mark exceptional closures with `is_tradable = FALSE` in market bars.
#'
#' @param timestamp POSIXct timestamps.
#' @param calendar_id Built-in calendar identifier.
#' @param exceptions Optional calendar-exception rows from
#'   [sim_exchange_calendar_exception()].
#' @return A logical vector.
#' @export
sim_calendar_is_open <- function(timestamp, calendar_id = "ALWAYS_OPEN", exceptions = NULL) {
  requested_calendar_id <- toupper(as.character(calendar_id))
  if (length(requested_calendar_id) != 1L || is.na(requested_calendar_id)) {
    stop("`calendar_id` must be one non-missing identifier.", call. = FALSE)
  }
  calendars <- sim_trading_calendars()
  calendar <- calendars[calendars[["calendar_id"]] == requested_calendar_id]
  if (nrow(calendar) != 1L) stop("Unknown calendar_id: ", requested_calendar_id, call. = FALSE)
  time <- as.POSIXlt(as.POSIXct(timestamp, tz = "UTC"), tz = calendar$timezone)
  weekday <- time$wday
  hour <- time$hour + time$min / 60 + time$sec / 3600
  if (requested_calendar_id %in% c("ALWAYS_OPEN", "CRYPTO_24_7")) {
    open <- rep.int(TRUE, length(time$wday))
  } else if (requested_calendar_id == "FX_24_5") {
    open <- weekday >= 1L & weekday <= 5L
  } else if (requested_calendar_id == "CME") {
    open <- (weekday == 0L & hour >= 17) |
      (weekday >= 1L & weekday <= 4L & (hour < 16 | hour >= 17)) |
      (weekday == 5L & hour < 16)
  } else {
  # XNYS: weekday regular session. Fixed-date holidays are observed on Friday
  # or Monday when they land on a weekend; movable holidays remain feed input.
  date <- as.Date(time)
  year <- as.integer(format(date, "%Y"))
  observed <- function(month, day) {
    candidate <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
    shift <- ifelse(weekdays(candidate) == "Saturday", -1L, ifelse(weekdays(candidate) == "Sunday", 1L, 0L))
    candidate + shift
  }
    holidays <- sim_calendar_holidays("XNYS", min(date), max(date))
    holiday <- date %in% holidays[action == "closed", session_date]
    early <- holidays[action == "early_close"]
    closes <- rep.int(16, length(date))
    for (i in seq_len(nrow(early))) {
      closes[date == early$session_date[i]] <- as.numeric(strsplit(early$close_time[i], ":", fixed = TRUE)[[1L]][1L])
    }
    open <- weekday >= 1L & weekday <= 5L & !holiday & hour >= 9.5 & hour < closes
  }
  if (!is.null(exceptions) && nrow(data.table::as.data.table(exceptions))) {
    exceptions <- data.table::as.data.table(exceptions)
    if (!"calendar_id" %in% names(exceptions)) exceptions[, calendar_id := requested_calendar_id]
    exceptions <- exceptions[calendar_id == requested_calendar_id]
    for (i in seq_len(nrow(exceptions))) {
      rows <- as.Date(time) == as.Date(exceptions$session_date[i])
      if (exceptions$action[i] == "closed") open[rows] <- FALSE
      if (exceptions$action[i] == "early_close" && !is.na(exceptions$close_time[i])) {
        clock <- as.numeric(strsplit(exceptions$close_time[i], ":", fixed = TRUE)[[1L]])
        open[rows & hour >= clock[1L] + clock[2L] / 60] <- FALSE
      }
    }
  }
  open
}

#' Calendarize registered-asset market bars
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars Market bars.
#' @param strict Whether a supplied tradable bar outside its registered session
#'   should error rather than be converted to valuation-only.
#' @return Canonical market bars with calendar-derived `is_tradable` values.
#' @export
sim_exchange_calendarize_bars <- function(exchange, bars, strict = FALSE) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  out <- .validate_market_bar_assets(exchange, as_market_bars(bars))
  specs <- exchange$assets[match(out$asset_id, exchange$assets$asset_id)]
  session_open <- vapply(seq_len(nrow(out)), function(i) {
    sim_calendar_is_open(out$timestamp[i], specs$calendar_id[i], .sim_calendar_exceptions_for_asset(exchange, specs$asset_id[i]))
  }, logical(1L))
  invalid <- (out$is_tradable %in% TRUE) & !session_open
  if (isTRUE(strict) && any(invalid)) {
    stop("Tradable bar falls outside its registered calendar session: ",
      paste(out$symbol[invalid], collapse = ", "), call. = FALSE)
  }
  out[, is_tradable := (is_tradable %in% TRUE) & session_open]
  out
}

#' Validate registered-asset bar cadence
#'
#' @param exchange A `tradesimr_exchange`.
#' @param bars Market bars.
#' @param strict Whether cadence violations should error.
#' @return A data.table with `calendar_open` and `cadence_ok` columns.
#' @export
sim_exchange_validate_cadence <- function(exchange, bars, strict = FALSE) {
  original <- .validate_market_bar_assets(exchange, as_market_bars(bars))
  out <- sim_exchange_calendarize_bars(exchange, original, strict = strict)
  specs <- exchange$assets[match(out$asset_id, exchange$assets$asset_id)]
  out[, calendar_open := vapply(seq_len(.N), function(i) {
    sim_calendar_is_open(timestamp[i], specs$calendar_id[i], .sim_calendar_exceptions_for_asset(exchange, specs$asset_id[i]))
  }, logical(1L))]
  out[, cadence_ok := TRUE]
  for (asset in unique(out$asset_id)) {
    rows <- which(out$asset_id == asset)
    cadence <- specs$bar_cadence_seconds[match(asset, specs$asset_id)]
    if (!is.finite(cadence) || cadence <= 0 || length(rows) < 2L) next
    ordered <- rows[order(out$timestamp[rows])]
    delta <- diff(as.numeric(out$timestamp[ordered]))
    ok <- abs(delta / cadence - round(delta / cadence)) < 1e-8
    out[ordered[-1L], cadence_ok := ok]
  }
  if (isTRUE(strict) && any(!out$cadence_ok)) stop("Market bars violate registered bar cadence.", call. = FALSE)
  out
}

#' @keywords internal
.sim_exchange_prepare_market_bars <- function(exchange, bars) {
  out <- .validate_market_bar_assets(exchange, as_market_bars(bars))
  mode <- as.character(exchange$config$calendar_mode %||% "raw")
  if (identical(mode, "raw")) return(out)
  # Cadence is defined against the preceding accepted bar as well as the
  # supplied batch. Keep the historical rows only for validation; callers
  # receive precisely their original new-bar batch.
  prior <- exchange$market_events[asset_id %in% out$asset_id]
  combined <- data.table::rbindlist(list(prior, out), fill = TRUE)
  checked <- sim_exchange_validate_cadence(exchange, combined, strict = FALSE)
  checked <- checked[(nrow(prior) + 1L):nrow(checked)]
  supplied_tradable <- out$is_tradable %in% TRUE
  if (identical(mode, "strict") && any(supplied_tradable & !checked$calendar_open)) {
    stop("Tradable bar falls outside its registered calendar session.", call. = FALSE)
  }
  if (identical(mode, "strict") && any(!checked$cadence_ok)) {
    stop("Market bars violate registered bar cadence.", call. = FALSE)
  }
  if (identical(mode, "calendarize")) {
    # A timestamp that cannot align to the asset's configured cadence is a
    # valuation observation only. It cannot create fills or decisions.
    checked[checked$cadence_ok %in% FALSE, is_tradable := FALSE]
  }
  checked[, c("calendar_open", "cadence_ok") := NULL]
  checked
}

#' @keywords internal
.sim_calendar_exceptions_for_asset <- function(exchange, requested_asset_id) {
  exceptions <- exchange$calendar_exceptions %||% sim_schemas()$calendar_exceptions[0]
  if (!nrow(exceptions)) return(exceptions)
  exceptions[is.na(asset_id) | asset_id == as.integer(requested_asset_id)]
}
