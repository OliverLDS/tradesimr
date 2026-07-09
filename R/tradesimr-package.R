#' tradesimr
#'
#' Execution and simulation engine for trading strategies, with durable event
#' exports, append-only agent commands, registered tradable assets, multi-asset
#' order routing, per-agent shared-cash cross-margin live accounts, AI agent
#' competitors, scheduled live-feed stepping, coordinated multi-asset market
#' simulation, local live-service APIs, separate replay, live-state, and agent
#' dashboards, and local orchestration entrypoints.
#'
#' @keywords internal
#' @useDynLib tradesimr, .registration = TRUE
#' @importFrom data.table copy nafill shift
#' @importFrom graphics mtext par
#' @importFrom Rcpp sourceCpp
#' @importFrom R6 R6Class
#' @importFrom utils tail
"_PACKAGE"
