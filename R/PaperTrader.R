#' Paper trader demo client
#'
#' Legacy R6 demonstration client for `PaperTradingPlatform`. The production
#' simulation path is `sim_backtest()` and the C++ execution engine.
#'
#' @field user_id User identifier registered on the demo platform.
#' @field platform Reference to a `PaperTradingPlatform` instance.
#' @export
PaperTrader <- R6::R6Class("PaperTrader",
  public = list(
    user_id = NULL,
    platform = NULL,  # reference to PaperTradingPlatform

    #' @description Create a paper trader demo client.
    #' @param user_id User identifier.
    #' @param platform A `PaperTradingPlatform` instance.
    initialize = function(user_id, platform) {
      self$user_id <- user_id
      self$platform <- platform
    },

    #' @description Place a demo order through the platform.
    #' @param inst_id Instrument identifier.
    #' @param type Order type label.
    #' @param pos Position side label.
    #' @param size Order size.
    #' @param price Order price.
    #' @param pricing_method Pricing method label.
    #' @param tag Optional order tag.
    #' @return Order id.
    place_order = function(inst_id, type, pos, size, price, pricing_method, tag) {
      order_id <- self$platform$place_user_order(self$user_id, inst_id, type, pos, size, price, pricing_method, tag)
      return(order_id)
    },
    
    #' @description Cancel a demo order.
    #' @param order_id Order id.
    #' @return Platform cancel result.
    cancel_order = function(order_id) {
      self$platform$cancel_user_order(self$user_id, order_id)
    },
    
    #' @description Get all orders for this trader.
    #' @return A data.table of orders.
    get_all_orders = function() {
      self$platform$get_user_orders(self$user_id)
    },
    
    #' @description Get one order for this trader.
    #' @param order_id Order id.
    #' @return A data.table with matching order rows.
    get_order = function(order_id) {
      self$platform$get_user_order(self$user_id, order_id)
    },
    
    #' @description Get this trader's wallet balance.
    #' @return Wallet balance or account cash.
    get_wallet = function() {
      self$platform$get_user_wallet(self$user_id)
    },
    
    #' @description Get this trader's position.
    #' @return A data.table of positions.
    get_position = function() {
      self$platform$get_user_position(self$user_id)
    }
  )
)
