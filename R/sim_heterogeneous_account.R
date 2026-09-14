#' Step a heterogeneous profile-aware account kernel
#'
#' This compatibility-safe primitive marks inventory and settles
#' futures/perpetual variation margin in their native cash currencies before
#' valuing the account in `base_currency`. Settlement, corporate-action, and
#' order tables are accepted as part of the stable account contract but are not
#' yet mutated by this futures-first implementation.
#'
#' @param base_currency Account reporting currency.
#' @param cash_balances Data frame with `currency`, `settled`, and `unsettled`.
#' @param inventory_positions Reserved inventory-position table.
#' @param margin_positions Data frame with `asset_id`, `currency`,
#'   `signed_units`, `settlement_price`, `last_price`, `contract_size`, and
#'   `maintenance_rate`.
#' @param bars Profile-tagged bars with `asset_id`, `close`, and optional
#'   `instrument_profile`.
#' @param fx_rates Data frame with `currency` and `rate_to_base`.
#' @param settlements,corporate_actions,orders Reserved durable input tables.
#' @param timestamp Settlement timestamp.
#' @return Updated account tables, base-currency equity and maintenance margin,
#'   liquidation status, and typed events.
#' @export
sim_heterogeneous_account_step <- function(base_currency,
                                           cash_balances,
                                           inventory_positions = data.frame(),
                                           margin_positions,
                                           bars,
                                           fx_rates,
                                           settlements = data.frame(),
                                           corporate_actions = data.frame(),
                                           orders = data.frame(),
                                           timestamp = Sys.time()) {
  required <- list(
    cash_balances = c("currency", "settled", "unsettled"),
    margin_positions = c("asset_id", "currency", "signed_units", "settlement_price", "last_price", "contract_size", "maintenance_rate"),
    bars = c("asset_id", "close"), fx_rates = c("currency", "rate_to_base")
  )
  if (nrow(inventory_positions) > 0L) {
    required_inventory <- c("asset_id", "currency", "units", "average_cost", "last_price", "contract_size")
    if (!all(required_inventory %in% names(inventory_positions))) {
      stop("`inventory_positions` is missing required columns.", call. = FALSE)
    }
  } else {
    inventory_positions <- data.frame(
      asset_id = integer(), currency = character(), units = numeric(),
      average_cost = numeric(), last_price = numeric(), contract_size = numeric()
    )
  }
  supplied <- list(cash_balances = cash_balances, margin_positions = margin_positions, bars = bars, fx_rates = fx_rates)
  for (name in names(required)) if (!all(required[[name]] %in% names(supplied[[name]]))) stop("`", name, "` is missing required columns.", call. = FALSE)
  out <- heterogeneous_account_step_rcpp(as.character(base_currency), data.frame(cash_balances), data.frame(inventory_positions), data.frame(margin_positions), data.frame(bars), data.frame(fx_rates), data.frame(settlements), data.frame(corporate_actions), data.frame(orders), as.numeric(as.POSIXct(timestamp, tz = "UTC")))
  out$events <- data.table::as.data.table(out$events)
  if (nrow(out$events)) data.table::set(out$events, j = "timestamp", value = as.POSIXct(out$events$timestamp, origin = "1970-01-01", tz = "UTC"))
  out
}
