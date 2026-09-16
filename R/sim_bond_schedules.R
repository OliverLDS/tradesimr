#' Register a calendar-driven bond schedule
#'
#' The schedule uses a fixed ACT/day-count convention and equal coupon periods.
#' At each eligible heterogeneous account boundary, C++ emits non-cash accrual
#' events, books due coupons into settled cash, and redeems remaining inventory
#' at maturity. A sparse replay boundary crossing multiple coupon dates is
#' processed coupon-by-coupon before any remaining partial-period accrual. The
#' cursor fields are durable exchange state, so resumed replay continues from
#' the same coupon boundary.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param symbol Registered bond symbol.
#' @param coupon_rate Annual decimal coupon rate.
#' @param coupon_frequency Number of equal coupon payments per 365-day year.
#' @param issue_timestamp Schedule start timestamp.
#' @param maturity_timestamp Maturity/redemption timestamp after `issue_timestamp`.
#' @param face_value Redemption value per inventory unit.
#' @param accrual_day_count Positive ACT denominator used for accrual events.
#' @param currency Coupon and redemption currency. Defaults to the asset quote
#'   currency.
#' @return Invisibly returns the registered schedule row.
#' @export
sim_bond_schedule_add <- function(exchange, symbol, coupon_rate,
                                  coupon_frequency = 2L,
                                  issue_timestamp, maturity_timestamp,
                                  face_value = 100,
                                  accrual_day_count = 365,
                                  currency = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  asset <- .asset_require_registered(exchange, symbol = symbol, context = "bond schedule asset")
  spec <- exchange$assets[asset_id == asset$asset_id]
  if (!identical(as.character(spec$instrument_profile[1L]), "bond")) {
    stop("Bond schedules require an asset with `instrument_profile = \"bond\"`.", call. = FALSE)
  }
  issue_timestamp <- .profile_utc_timestamp(issue_timestamp)
  maturity_timestamp <- .profile_utc_timestamp(maturity_timestamp)
  coupon_rate <- as.numeric(coupon_rate)
  coupon_frequency <- as.numeric(coupon_frequency)
  face_value <- as.numeric(face_value)
  accrual_day_count <- as.numeric(accrual_day_count)
  if (!is.finite(coupon_rate) || coupon_rate < 0 || !is.finite(coupon_frequency) || coupon_frequency <= 0 ||
      !is.finite(face_value) || face_value <= 0 || !is.finite(accrual_day_count) || accrual_day_count <= 0 ||
      maturity_timestamp <= issue_timestamp) {
    stop("Bond schedule fields must be finite, positive where required, and have maturity after issue.", call. = FALSE)
  }
  period_seconds <- 365 * 86400 / coupon_frequency
  row <- data.table::data.table(
    asset_id = as.integer(asset$asset_id), symbol = as.character(asset$symbol),
    currency = .profile_currency(exchange, currency %||% spec$quote_ccy[1L]),
    coupon_rate = coupon_rate, coupon_frequency = coupon_frequency,
    face_value = face_value, accrual_day_count = accrual_day_count,
    issue_timestamp = issue_timestamp, maturity_timestamp = maturity_timestamp,
    last_accrual_timestamp = issue_timestamp,
    next_coupon_timestamp = issue_timestamp + period_seconds,
    status = "active"
  )
  current <- exchange$bond_schedules
  exchange$bond_schedules <- data.table::rbindlist(list(current[asset_id != row$asset_id], row), fill = TRUE)
  invisible(row)
}

#' List durable bond schedules
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A data.table of bond schedule state.
#' @export
sim_bond_schedules <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  data.table::copy(exchange$bond_schedules)
}

#' @keywords internal
.bond_schedule_kernel_rows <- function(exchange, bars, timestamp) {
  schedules <- exchange$bond_schedules[
    status == "active" & asset_id %in% as.integer(bars$asset_id) & issue_timestamp <= timestamp
  ]
  if (!nrow(schedules)) return(data.table::data.table())
  schedules[, schedule_type := "bond"]
  schedules[, .(asset_id, schedule_type, currency, coupon_rate, coupon_frequency,
    face_value, accrual_day_count, last_accrual_timestamp = as.numeric(last_accrual_timestamp),
    next_coupon_timestamp = as.numeric(next_coupon_timestamp),
    maturity_timestamp = as.numeric(maturity_timestamp))]
}

#' @keywords internal
.bond_schedule_advance <- function(exchange, schedules, timestamp) {
  if (!nrow(schedules)) return(invisible(NULL))
  timestamp <- .profile_utc_timestamp(timestamp)
  for (asset_id in unique(as.integer(schedules$asset_id))) {
    index <- which(exchange$bond_schedules$asset_id == asset_id & exchange$bond_schedules$status == "active")
    if (!length(index)) next
    row <- exchange$bond_schedules[index[1L]]
    cutoff <- min(timestamp, row$maturity_timestamp[1L])
    data.table::set(exchange$bond_schedules, i = index[1L], j = "last_accrual_timestamp", value = cutoff)
    period <- 365 * 86400 / row$coupon_frequency[1L]
    next_coupon <- row$next_coupon_timestamp[1L]
    while (next_coupon <= cutoff) next_coupon <- next_coupon + period
    data.table::set(exchange$bond_schedules, i = index[1L], j = "next_coupon_timestamp", value = next_coupon)
    if (timestamp >= row$maturity_timestamp[1L]) {
      data.table::set(exchange$bond_schedules, i = index[1L], j = "status", value = "matured")
    }
  }
  invisible(NULL)
}
