#' Empty normalized heterogeneous order-batch schema
#'
#' The schema carries generic order identity and eligibility fields plus
#' derivative compatibility fields: action/direction codes, strategy/action
#' identifiers, target-derived admission flag, per-asset quantity step, and
#' funding settings. Inventory adapters may leave the compatibility fields at
#' their typed defaults.
#'
#' @return A typed empty data.table accepted by
#'   [sim_heterogeneous_account_step()].
#' @export
sim_heterogeneous_order_batch_schema <- function() {
  data.table::data.table(
    order_id = character(), asset_id = integer(), instrument_profile = character(),
    side = character(), qty = numeric(), order_type = character(),
    limit_price = numeric(), execution_price = numeric(), fee_rt = numeric(),
    eligible_after = as.POSIXct(character()), atomic_group_id = character(),
    target_derived = logical(), time_in_force = character(),
    action_code = integer(), dir_code = integer(), order_type_code = integer(),
    strat_id = integer(), action_id = integer(), ctr_step = numeric(),
    fund_rt = numeric(), funding_interval_hours = numeric()
  )
}

#' @keywords internal
.normalize_heterogeneous_orders <- function(orders, timestamp) {
  if (nrow(orders) == 0L) return(sim_heterogeneous_order_batch_schema())
  orders <- data.table::as.data.table(data.table::copy(orders))
  required <- c("order_id", "asset_id", "instrument_profile", "side", "qty", "order_type", "execution_price", "fee_rt", "eligible_after", "atomic_group_id", "target_derived", "time_in_force")
  missing <- setdiff(required, names(orders))
  if (length(missing)) stop("Normalized heterogeneous orders are missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!"limit_price" %in% names(orders)) orders[, limit_price := NA_real_]
  optional <- list(
    action_code = 0L, dir_code = 0L, order_type_code = 0L,
    strat_id = 0L, action_id = 0L, ctr_step = 1,
    fund_rt = 0, funding_interval_hours = 8
  )
  for (name in names(optional)) if (!name %in% names(orders)) orders[, (name) := optional[[name]]]
  orders[, eligible_after := as.POSIXct(eligible_after, tz = "UTC")]
  if (any(!(orders$order_type %in% c("market", "limit")))) stop("Heterogeneous orders require `market` or `limit` order types.", call. = FALSE)
  if (any(!(orders$side %in% c("buy", "sell", "flat")))) stop("Heterogeneous orders require buy, sell, or flat sides.", call. = FALSE)
  if (any(!(orders$time_in_force %in% c("gtc", "ioc", "fok", "next_eligible_bar")))) stop("Unsupported heterogeneous `time_in_force`.", call. = FALSE)
  if (any(!is.finite(orders$qty) | orders$qty < 0)) stop("Heterogeneous order quantities must be non-negative and finite.", call. = FALSE)
  if (any(orders$order_type == "limit" & (!is.finite(orders$limit_price) | orders$limit_price <= 0))) stop("Limit orders require a positive `limit_price`.", call. = FALSE)
  if (any(is.na(orders$eligible_after) | orders$eligible_after >= as.POSIXct(timestamp, tz = "UTC"))) stop("Only orders eligible strictly before this market boundary may enter a heterogeneous batch.", call. = FALSE)
  groups <- as.character(orders$atomic_group_id)
  if (anyNA(groups) || any(!nzchar(groups))) stop("Every heterogeneous order requires an `atomic_group_id`.", call. = FALSE)
  if (anyDuplicated(rle(groups)$values) > 0L) {
    stop("Rows belonging to an atomic group must be contiguous in the normalized batch.", call. = FALSE)
  }
  schema_columns <- names(sim_heterogeneous_order_batch_schema())
  orders[, ..schema_columns]
}

#' Step a heterogeneous profile-aware account kernel
#'
#' Marks inventory, settles futures/perpetual variation margin, and evaluates
#' a normalized order batch without mutating the supplied R input tables.
#'
#' @param base_currency Account reporting currency.
#' @param cash_balances Data frame with `currency`, `settled`, and `unsettled`.
#' @param inventory_positions Data frame with inventory units and valuation.
#' @param margin_positions Data frame with margin positions and settlement prices.
#' @param bars Profile-tagged market bars.
#' @param fx_rates Data frame with `currency` and `rate_to_base`.
#' @param settlements Durable engine settings and settlement inputs.
#' @param corporate_actions A durable input table. Rows with `asset_i`,
#'   `asset_j`, and `covariance` provide covariance-margin inputs. Explicit
#'   bond lifecycle rows use `asset_id`, `action_type` (`coupon`,
#'   `bond_accrual`, or `redemption`), `amount`, `currency`, and optional
#'   `effective_timestamp`; they mutate settled cash and emit typed events at
#'   the eligible account boundary. Calendar rows use `schedule_type = "bond"`
#'   with coupon, accrual-cursor, and maturity fields from
#'   [sim_bond_schedule_add()]. C++ carries accrued interest on the typed
#'   inventory position, emits non-cash accrual events, settles due coupons,
#'   and redeems inventory at maturity.
#' @param orders A normalized heterogeneous order batch.
#' @param timestamp Market-boundary timestamp.
#' @return Updated account state, typed events, fills, and group outcomes.
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
  orders <- .normalize_heterogeneous_orders(orders, timestamp)
  out <- heterogeneous_account_step_rcpp(as.character(base_currency), data.frame(cash_balances), data.frame(inventory_positions), data.frame(margin_positions), data.frame(bars), data.frame(fx_rates), data.frame(settlements), data.frame(corporate_actions), data.frame(orders), as.numeric(as.POSIXct(timestamp, tz = "UTC")))
  out$events <- data.table::as.data.table(out$events)
  out$fills <- data.table::as.data.table(out$fills)
  if (nrow(out$events)) data.table::set(out$events, j = "timestamp", value = as.POSIXct(out$events$timestamp, origin = "1970-01-01", tz = "UTC"))
  out
}
