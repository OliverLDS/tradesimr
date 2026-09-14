#' Step a fully paid spot-inventory state
#'
#' This Rcpp-backed state machine is separate from the derivatives margin
#' kernel. It models long-only inventory, quote-currency cash, fees, marked
#' inventory value, dividends, and stock splits.
#'
#' @param state A prior spot state, or `NULL` for a new account.
#' @param close Current mark price.
#' @param signed_qty Positive to buy units and negative to sell units.
#' @param execution_price Optional execution price; defaults to `close`.
#' @param contract_size Units represented by one inventory unit.
#' @param fee_rt Fee rate applied to execution notional.
#' @param dividend_per_unit Cash dividend per unit.
#' @param split_ratio Inventory split ratio.
#' @return A list containing cash, units, average cost, market value, equity,
#'   P&L, fees, corporate-action cash, and execution status.
#' @export
sim_spot_step <- function(state = NULL,
                          close,
                          signed_qty = 0,
                          execution_price = NA_real_,
                          contract_size = 1,
                          fee_rt = 0,
                          dividend_per_unit = 0,
                          split_ratio = 1) {
  if (is.null(state)) state <- list(cash = 0, units = 0, avg_cost = NA_real_)
  spot_step_rcpp(
    state = state,
    close = as.numeric(close),
    signed_qty = as.numeric(signed_qty),
    execution_price = as.numeric(execution_price),
    contract_size = as.numeric(contract_size),
    fee_rt = as.numeric(fee_rt),
    dividend_per_unit = as.numeric(dividend_per_unit),
    split_ratio = as.numeric(split_ratio)
  )
}
