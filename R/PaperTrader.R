#' Paper trader demo client
#'
#' Legacy R6 demonstration client for `PaperTradingPlatform`. The production
#' simulation path is `sim_backtest()` and the C++ execution engine.
#'
#' @export
PaperTrader <- R6::R6Class("PaperTrader",
  public = list(
    user_id = NULL,
    platform = NULL,  # reference to PaperTradingPlatform

    initialize = function(user_id, platform) {
      self$user_id <- user_id
      self$platform <- platform
    },

    place_order = function(inst_id, type, pos, size, price, pricing_method, tag) {
      order_id <- self$platform$place_user_order(self$user_id, inst_id, type, pos, size, price, pricing_method, tag)
      return(order_id)
    },
    
    cancel_order = function(order_id) {
      self$platform$cancel_user_order(self$user_id, order_id)
    },
    
    get_all_orders = function() {
      self$platform$get_user_orders(self$user_id)
    },
    
    get_order = function(order_id) {
      self$platform$get_user_order(self$user_id, order_id)
    },
    
    get_wallet = function() {
      self$platform$get_user_wallet(self$user_id)
    },
    
    get_position = function() {
      self$platform$get_user_position(self$user_id)
    }
  )
)
