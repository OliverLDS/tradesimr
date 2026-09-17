#' List supported instrument profiles
#'
#' Instrument profiles define accounting and calendar defaults. They are
#' intentionally declarative: the current C++ execution kernel still uses the
#' registered contract multiplier and quantity step as its numeric inputs.
#' The `synthetic_price_return` profile models signed marked price exposure
#' only. It makes no custody, borrow availability or cost, dividend, funding,
#' carry, settlement, or corporate-action claims, and is therefore suitable
#' for stated price-return simulations rather than brokerage execution.
#' Target-derived fee scaling preserves target ratios subject to contract-step
#' rounding and records `fee_scaled` fills as durable partial
#' execution-quality outcomes. Explicit contract orders remain all-or-nothing.
#'
#' @return A data.table of supported profiles and their defaults.
#' @export
sim_instrument_profiles <- function() {
  data.table::data.table(
    instrument_profile = c("equity", "etf", "future", "fx_spot", "crypto_spot", "crypto_perp", "bond", "synthetic_price_return", "other"),
    asset_class = c("stock", "etf", "commodity_future", "fx", "cryptocurrency", "crypto_perp", "bond", "synthetic_price_return", "other"),
    calendar_id = c("XNYS", "XNYS", "CME", "FX_24_5", "CRYPTO_24_7", "CRYPTO_24_7", "XNYS", "ALWAYS_OPEN", "ALWAYS_OPEN"),
    timezone = c("America/New_York", "America/New_York", "America/Chicago", "UTC", "UTC", "UTC", "America/New_York", "UTC", "UTC"),
    settlement_lag_days = c(1L, 1L, 0L, 2L, 0L, 0L, 1L, 0L, 0L),
    margin_model = c("cash", "cash", "futures", "fx", "cash", "perpetual", "cash", "synthetic_margin", "generic"),
    accounting_model = c("equity", "equity", "futures", "fx", "spot", "perpetual", "bond", "price_return_margin", "generic"),
    settlement_model = c("T+1 cash", "T+1 cash", "daily variation margin", "T+2 calendar settlement", "immediate", "collateral plus funding", "T+1 cash", "marked synthetic exposure", "immediate"),
    funding_or_carry = c("borrow and cash interest", "borrow and cash interest", "cash interest", "FX carry", "none", "funding calendar and collateral", "accrual and coupon", "none; price-return methodology", "none"),
    corporate_action_hooks = c("dividend, split, delisting", "dividend, split, delisting", "expiry, roll", "none", "none", "none", "coupon, accrual, redemption", "none", "none"),
    limitations = c(
      "No tax-lot accounting or jurisdictional withholding.",
      "No tax-lot accounting or jurisdictional withholding.",
      "Roll requires an explicit registered successor and settlement price.",
      "Settlement is calendar-aware; value-date conventions beyond T+2 require a custom calendar.",
      "Fully paid inventory only; no chain-specific custody model.",
      "Single collateral currency per position; no venue-specific insurance fund model.",
      "Fixed-income schedules are deterministic; no yield-curve pricing model.",
      "Synthetic signed price exposure only; no custody, borrow, dividends, splits, funding, or physical settlement. Use for stated price-return simulations, not brokerage execution.",
      "Generic marked instrument only; lifecycle, settlement, and corporate actions are unsupported."
    )
  )
}

#' Resolve an instrument profile
#'
#' @param instrument_profile Supported profile name.
#' @return A one-row data.table of profile defaults.
#' @export
sim_instrument_profile <- function(instrument_profile) {
  profile <- .instrument_profile_resolve(instrument_profile)
  data.table::copy(profile)
}

#' @keywords internal
.instrument_profile_resolve <- function(instrument_profile = "other") {
  aliases <- c(
    stock = "equity", equity = "equity", etf = "etf", future = "future",
    commodity_future = "future", fx = "fx_spot", fx_spot = "fx_spot",
    cryptocurrency = "crypto_spot", crypto_spot = "crypto_spot",
    crypto_perp = "crypto_perp", bond = "bond",
    synthetic_price_return = "synthetic_price_return", price_return = "synthetic_price_return",
    other = "other"
  )
  requested <- tolower(as.character(instrument_profile %||% "other"))
  if (length(requested) != 1L || is.na(requested) || !requested %in% names(aliases)) {
    stop("Unsupported `instrument_profile`. Use one of: ", paste(unique(unname(aliases)), collapse = ", "), call. = FALSE)
  }
  profiles <- sim_instrument_profiles()
  data.table::copy(profiles[instrument_profile == aliases[[requested]]])
}

#' @keywords internal
.instrument_metadata_encode <- function(metadata) {
  if (is.null(metadata) || !length(metadata)) return(NA_character_)
  if (!is.list(metadata) || is.null(names(metadata)) || any(!nzchar(names(metadata)))) {
    stop("`metadata` must be a named list.", call. = FALSE)
  }
  paste(names(metadata), vapply(metadata, as.character, character(1L)), sep = "=", collapse = ";")
}
