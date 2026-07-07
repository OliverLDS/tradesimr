#' @title Calculate Unrealized PnL
#' @description Internal function to update unrealized PnL
#' @keywords internal
.calculate_unrealized_pnl <- function(price, long_avg_entry_price, short_avg_entry_price, long_notional, short_notional, fee_rate) {
  (price - long_avg_entry_price) * long_notional + (short_avg_entry_price - price) * short_notional - (price + long_avg_entry_price) * long_notional * fee_rate - (price + short_avg_entry_price) * short_notional * fee_rate
}
