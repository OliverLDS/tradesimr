#' tradesimr
#'
#' Execution and simulation engine for trading strategies, with durable event
#' exports, append-only agent commands, scheduled live-feed stepping, local
#' live-service APIs, separate replay, live-state, and agent dashboards, and
#' local orchestration entrypoints.
#'
#' @keywords internal
#' @useDynLib tradesimr, .registration = TRUE
#' @importFrom data.table copy nafill shift
#' @importFrom graphics mtext par
#' @importFrom Rcpp sourceCpp
#' @importFrom R6 R6Class
#' @importFrom utils tail
"_PACKAGE"
