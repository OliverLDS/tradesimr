#' Paper trading platform demo
#'
#' Legacy R6 demonstration of a paper trading platform. The production
#' simulation path is `sim_backtest()` and the C++ execution engine.
#'
#' @export
PaperTradingPlatform <- R6::R6Class("PaperTradingPlatform",
  public = list(
    # ---- inst info functions ----
    inst_info = list(
      "ETH-USDT-SWAP" = list(contract_size = 0.1, min_digit = 2),
      "SOL-USDT-SWAP" = list(contract_size = 1,   min_digit = 2),
      "BNB-USDT-SWAP" = list(contract_size = 0.01, min_digit = 1)
    ),
    
    # ---- bar data ----
    # utime, open, high, low, close
    bar_info = list(),
    
    # ---- user data ----
    user_data = list(),
    
    # ---- order pool ----
    order_pool = data.table::data.table(
      order_id = character(),
      user_id = character(),
      inst_id = character(),
      type = character(),
      pos = character(),
      size = numeric(),
      price = numeric(),
      fill_price = numeric(),
      pricing_method = character(),
      tag = character(),
      status = character(),
      ctime = as.POSIXct(character()),
      utime = as.POSIXct(character())
    ),
    order_id_counter = 1,
    
    # ---- fun function ----
    # run = function() {
    #   
    # },

    # ---- user functions ----
    register_user = function(user_id, initial_balance = 10000) {
      if (!is.null(self$user_data[[user_id]])) stop("User exists")
      self$user_data[[user_id]] <- list(
        wallet_balance = initial_balance,
        position = data.table::data.table(
          inst_id    = character(),
          pos        = character(),   # "long" or "short"
          size       = numeric(),     # number of contracts
          avg_price  = numeric()
        )
      )
      user <- PaperTrader$new(user_id, self)
      return(user)
    },
    
    # ---- order functions ----
    place_user_order = function(user_id, inst_id, type, pos, size, price, pricing_method, tag) {
      order_id <- paste0("ORD", sprintf("%06d", self$order_id_counter))
      self$order_id_counter <- self$order_id_counter + 1
      
      order_dt <- data.table::data.table(
        order_id = order_id,
        
        user_id = user_id,
        inst_id = inst_id,
        type = type,
        pos = pos,
        size = size,
        price = price,
        pricing_method = pricing_method,
        tag = tag,
        
        status = "live",
        fill_price = NA_real_,
        ctime = Sys.time(),
        utime = Sys.time()
      )

      data.table::setcolorder(order_dt, names(self$order_pool))  # ensure column order
      self$order_pool <- data.table::rbindlist(list(self$order_pool, order_dt))

      return(order_id)
    },
    
    cancel_user_order = function(user_id, order_id) {
      order_pool <- self$order_pool
      row_idx <- which(order_pool$user_id == user_id & order_pool$order_id == order_id)
      if (length(row_idx) > 0) {
        self$order_pool <- order_pool[-row_idx, ]
      }
    },

    get_user_orders = function(user_id) {
      row_idx <- which(self$order_pool$user_id == user_id)
      self$order_pool[row_idx, ]
    },
    
    get_user_order = function(user_id, order_id) {
      order_pool <- self$order_pool
      row_idx <- which(order_pool$user_id == user_id & order_pool$order_id == order_id)
      self$order_pool[row_idx, ]
    },
    
    get_live_orders = function() {
      row_idx <- which(self$order_pool$status == 'live')
      self$order_pool[row_idx, ]
    },
    
    get_order = function(order_id) {
      row_idx <- which(self$order_pool$order_id == order_id)
      self$order_pool[row_idx, ]
    },
    
    # ---- update market bar functions ----
    
    update_bar = function(inst_id, timestamp, open, high, low, close) {
      self$bar_info[[inst_id]]$timestamp <- timestamp
      self$bar_info[[inst_id]]$open <- open
      self$bar_info[[inst_id]]$high <- high
      self$bar_info[[inst_id]]$low <- low
      self$bar_info[[inst_id]]$close <- close
    },
    
    process_order = function(order) {
      order_id <- order$order_id
      inst_id <- order$inst_id
      pricing_method <- order$pricing_method
      price <- order$price
      type <- order$type
      pos <- order$pos
      
      bar <- self$bar_info[[inst_id]]
      open <- bar$open
      high <- bar$high
      low <- bar$low
      close <- bar$close
      
      if (pricing_method == 'market') {
        price <- open
      }
      if ((type == 'OPEN' && pos == 'long') || (type == 'CLOSE' && pos == 'short')) {
        if (price>=low) {
          self$fill_order(order_id, price)
        }
      }
      if ((type == 'CLOSE' && pos == 'long') || (type == 'OPEN' && pos == 'short')) {
        if (price<=high) {
          self$fill_order(order_id, price)
        }
      }
  
    },
    
    # ---- position functions ----
    fill_order = function(order_id, fill_price) {
      # update order status
      row_idx <- which(self$order_pool$order_id == order_id)
      if (length(row_idx) > 0) {
        data.table::set(self$order_pool, i = row_idx, j = "status", value = "filled")
        data.table::set(self$order_pool, i = row_idx, j = "fill_price", value = fill_price)
        data.table::set(self$order_pool, i = row_idx, j = "utime", value = Sys.time())
        
        # update user position
        order <- self$order_pool[row_idx, ]
        user_id <- order$user_id
        self$update_user_wallet(user_id, self$update_user_position(order))
      }
    },
    
    get_user_wallet = function(user_id) {
      self$user_data[[user_id]]$wallet_balance
    },
    
    get_user_position = function(user_id) {
      self$user_data[[user_id]]$position 
    },
    
    update_user_wallet = function(user_id, delta_wallet_balance) {
      old_wallet_balance <- self$get_user_wallet(user_id)
      self$user_data[[user_id]]$wallet_balance <- old_wallet_balance + delta_wallet_balance
    },
    
    update_user_position = function(order) {
      user_id <- order$user_id
      inst_id <- order$inst_id
      pos <- order$pos
      size <- order$size
      fill_price <- order$fill_price
      type <- order$type
    
      position_dt <- self$user_data[[user_id]]$position
      row_idx <- which(position_dt$inst_id == inst_id & position_dt$pos == pos)
      
      delta_wallet_balance <- 0
    
      if (type == "OPEN") { # mortgage may be considered in the future
        if (length(row_idx) == 0) {
          # Create new position
          new_row <- data.table::data.table(
            inst_id = inst_id,
            pos = pos,
            size = size,
            avg_price = fill_price
          )
          self$user_data[[user_id]]$position <- data.table::rbindlist(list(position_dt, new_row))
        } else {
          # Update existing position (weighted average)
          old_size <- position_dt[row_idx, ]$size
          old_avg_price <- position_dt[row_idx, ]$avg_price
          new_size <- old_size + size
          new_avg_price <- (old_size * old_avg_price + size * fill_price) / new_size
          
          data.table::set(position_dt, i = row_idx, j = "size", value = new_size)
          data.table::set(position_dt, i = row_idx, j = "avg_price", value = new_avg_price)
          self$user_data[[user_id]]$position <- position_dt
        }
      } else if (type == "CLOSE") {
        if (length(row_idx) == 0) {
          warning("Trying to close a position that does not exist")
          return()
        }
    
        old_size <- position_dt[row_idx, ]$size
        old_avg_price <- position_dt[row_idx, ]$avg_price
        if (old_size < size) {
          warning("Trying to close more contracts than owned")
          size <- old_size  # cap to max possible
        }
        delta_wallet_balance <- size * self$inst_info[[inst_id]]$contract_size * (fill_price - old_avg_price) * ifelse(pos=='long', 1, -1)
        new_size <- old_size - size
    
        if (new_size == 0) {
          self$user_data[[user_id]]$position <- position_dt[-row_idx, ]
        } else {
          data.table::set(position_dt, i = row_idx, j = "size", value = new_size)
          self$user_data[[user_id]]$position <- position_dt
        }
      }
      return(delta_wallet_balance)
    }
  )
)
