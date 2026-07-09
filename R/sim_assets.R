#' Register a tradable asset on an exchange
#'
#' @param exchange A `tradesimr_exchange`.
#' @param symbol Asset symbol, for example `"BTC-USDT-SWAP"`.
#' @param asset_id Optional integer asset id. Defaults to a stable id derived
#'   from `symbol`.
#' @param status Asset status: `active`, `paused`, or `removed`.
#' @param asset_class Asset class label, such as `crypto_perp`, `stock`,
#'   `bond`, `etf`, `commodity_future`, `fx`, or `other`.
#' @param contract_size Contract multiplier used by execution/accounting.
#' @param tick_size Minimum price increment.
#' @param qty_step Minimum order quantity increment.
#' @param base_ccy,quote_ccy Optional currency labels.
#' @return Invisibly returns the registered asset row.
#' @export
sim_asset_add <- function(exchange,
                          symbol,
                          asset_id = NULL,
                          status = c("active", "paused", "removed"),
                          asset_class = "other",
                          contract_size = 1,
                          tick_size = NA_real_,
                          qty_step = 1,
                          base_ccy = NA_character_,
                          quote_ccy = NA_character_) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  if (is.null(symbol) || !nzchar(as.character(symbol))) {
    stop("`symbol` is required.", call. = FALSE)
  }
  status <- match.arg(status)
  symbol <- as.character(symbol)
  asset_id <- as.integer(asset_id %||% .asset_id_from_symbol(symbol))
  existing <- which(exchange$assets$asset_id == asset_id | exchange$assets$symbol == symbol)
  row <- data.table::data.table(
    asset_id = asset_id,
    symbol = symbol,
    status = status,
    asset_class = as.character(asset_class %||% "other"),
    contract_size = as.numeric(contract_size),
    tick_size = as.numeric(tick_size),
    qty_step = as.numeric(qty_step),
    base_ccy = as.character(base_ccy),
    quote_ccy = as.character(quote_ccy),
    created_at = Sys.time()
  )
  if (length(existing) > 0L) {
    for (col in names(row)) data.table::set(exchange$assets, i = existing[1L], j = col, value = row[[col]])
  } else {
    exchange$assets <- data.table::rbindlist(list(exchange$assets, row), fill = TRUE)
  }
  exchange$asset_symbols[[as.character(asset_id)]] <- symbol
  invisible(row)
}

#' Remove an asset from an exchange registry
#'
#' Removed assets remain in the durable registry with status `removed`.
#'
#' @param exchange A `tradesimr_exchange`.
#' @param symbol Optional symbol.
#' @param asset_id Optional integer asset id.
#' @return Invisibly returns `TRUE` when an asset was updated.
#' @export
sim_asset_remove <- function(exchange, symbol = NULL, asset_id = NULL) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  idx <- .asset_registry_index(exchange, symbol = symbol, asset_id = asset_id)
  if (length(idx) == 0L) return(invisible(FALSE))
  data.table::set(exchange$assets, i = idx, j = "status", value = "removed")
  invisible(TRUE)
}

#' List registered exchange assets
#'
#' @param exchange A `tradesimr_exchange`.
#' @return A data.table of assets.
#' @export
sim_assets <- function(exchange) {
  stopifnot(inherits(exchange, "tradesimr_exchange"))
  data.table::copy(exchange$assets)
}

#' @keywords internal
.asset_registry_index <- function(exchange, symbol = NULL, asset_id = NULL) {
  if (is.null(exchange$assets) || nrow(exchange$assets) == 0L) return(integer())
  idx <- seq_len(nrow(exchange$assets))
  if (!is.null(symbol)) idx <- idx[exchange$assets$symbol[idx] == as.character(symbol)]
  if (!is.null(asset_id)) idx <- idx[exchange$assets$asset_id[idx] == as.integer(asset_id)]
  idx
}

#' @keywords internal
.asset_auto_register_enabled <- function(exchange) {
  isTRUE(exchange$config$auto_register_assets %||% FALSE)
}

#' @keywords internal
.asset_require_registered <- function(exchange, symbol = NULL, asset_id = NULL, context = "asset") {
  requested_symbol <- symbol
  requested_asset_id <- asset_id
  asset <- .normalize_asset_key(symbol = symbol, asset_id = asset_id, exchange = NULL, validate = FALSE)
  if (is.null(requested_symbol) && is.null(requested_asset_id)) {
    requested_symbol <- asset$symbol
    requested_asset_id <- asset$asset_id
  }
  idx <- .asset_registry_index(
    exchange,
    symbol = if (is.null(requested_symbol)) NULL else asset$symbol,
    asset_id = if (is.null(requested_asset_id)) NULL else asset$asset_id
  )
  if (length(idx) > 0L && !identical(exchange$assets$status[idx[1L]], "removed")) {
    row <- exchange$assets[idx[1L]]
    exchange$asset_symbols[[as.character(row$asset_id[1L])]] <- row$symbol[1L]
    return(list(symbol = row$symbol[1L], asset_id = as.integer(row$asset_id[1L])))
  }
  if (.asset_auto_register_enabled(exchange)) {
    sim_asset_add(exchange, symbol = asset$symbol, asset_id = asset$asset_id)
    return(asset)
  }
  stop(
    "Unregistered ", context, ": symbol=", asset$symbol, ", asset_id=", asset$asset_id,
    ". Register it with sim_asset_add() or create the exchange with auto_register_assets = TRUE.",
    call. = FALSE
  )
}
