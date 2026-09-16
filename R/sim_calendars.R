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

#' Test whether timestamps fall in a built-in tradable session
#'
#' This deliberately provides deterministic session rules, not a vendor
#' holiday database. `XNYS` includes fixed-date observed holidays; callers may
#' mark exceptional closures with `is_tradable = FALSE` in market bars.
#'
#' @param timestamp POSIXct timestamps.
#' @param calendar_id Built-in calendar identifier.
#' @return A logical vector.
#' @export
sim_calendar_is_open <- function(timestamp, calendar_id = "ALWAYS_OPEN") {
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
  if (requested_calendar_id %in% c("ALWAYS_OPEN", "CRYPTO_24_7")) return(rep.int(TRUE, length(time$wday)))
  if (requested_calendar_id == "FX_24_5") return(weekday >= 1L & weekday <= 5L)
  if (requested_calendar_id == "CME") {
    return((weekday == 0L & hour >= 17) |
      (weekday >= 1L & weekday <= 4L & (hour < 16 | hour >= 17)) |
      (weekday == 5L & hour < 16))
  }
  # XNYS: weekday regular session. Fixed-date holidays are observed on Friday
  # or Monday when they land on a weekend; movable holidays remain feed input.
  date <- as.Date(time)
  year <- as.integer(format(date, "%Y"))
  observed <- function(month, day) {
    candidate <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
    shift <- ifelse(weekdays(candidate) == "Saturday", -1L, ifelse(weekdays(candidate) == "Sunday", 1L, 0L))
    candidate + shift
  }
  holiday <- date %in% c(observed(1, 1), observed(6, 19), observed(7, 4), observed(12, 25))
  weekday >= 1L & weekday <= 5L & !holiday & hour >= 9.5 & hour < 16
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
  open <- vapply(seq_len(nrow(out)), function(i) {
    sim_calendar_is_open(out$timestamp[i], specs$calendar_id[i])
  }, logical(1L))
  invalid <- out$is_tradable %in% TRUE & !open
  if (isTRUE(strict) && any(invalid)) {
    stop("Tradable bar falls outside its registered calendar session: ",
      paste(out$symbol[invalid], collapse = ", "), call. = FALSE)
  }
  out[, is_tradable := is_tradable %in% TRUE & open]
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
    sim_calendar_is_open(timestamp[i], specs$calendar_id[i])
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
