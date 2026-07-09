#' tradesimr
#'
#' Execution and simulation engine for trading strategies, with durable event
#' exports, append-only agent commands, registered tradable assets, multi-asset
#' order routing, per-agent shared-cash cross-margin live accounts, AI agent
#' competitors, strategy-backed agent diagnostics, scheduled live-feed stepping,
#' calibrated multi-asset market simulation, durable per-feed simulation state,
#' optional portfolio-margin enforcement through a multi-asset C++ step kernel,
#' local live-service APIs, separate replay, live-state, and agent dashboards,
#' and local orchestration entrypoints.
#'
#' @keywords internal
#' @useDynLib tradesimr, .registration = TRUE
#' @importFrom data.table copy nafill shift
#' @importFrom graphics mtext par
#' @importFrom Rcpp sourceCpp
#' @importFrom R6 R6Class
#' @importFrom utils tail
"_PACKAGE"
