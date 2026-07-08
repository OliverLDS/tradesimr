#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x

arg_value <- function(flag, default = NULL) {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) default else args[[idx + 1L]]
}

flag_present <- function(flag) {
  flag %in% args
}

if (flag_present("--help") || flag_present("-h")) {
  cat(
    "Usage: scripts/run_inferencer_eth_4h_dashboard.R [options]\n\n",
    "Loads locally cached OKX ETH-USDT-SWAP 4H candles through investdatar,\n",
    "applies the Zelina Dual_Pulse target-position logic, runs tradesimr,\n",
    "and exports replay dashboard CSV/assets. Use the companion .zsh script\n",
    "to serve and open the replay UI.\n\n",
    "Options:\n",
    "  --out-dir PATH   Dashboard output directory.\n",
    "  --inst-id ID     OKX instrument id. Default: ETH-USDT-SWAP.\n",
    "  --bar BAR        OKX candle interval. Default: 4H.\n",
    "  -h, --help       Show this help.\n",
    sep = ""
  )
  quit(status = 0L)
}

script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1] %||% "")
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path, mustWork = FALSE)) else getwd()
repo_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
out_dir <- normalizePath(
  arg_value("--out-dir", file.path(repo_root, "scripts", "_outputs", "inferencer-eth-4h-dashboard")),
  mustWork = FALSE
)
inst_id <- arg_value("--inst-id", "ETH-USDT-SWAP")
bar <- arg_value("--bar", "4H")

load_tradesimr <- function(repo_root) {
  if (requireNamespace("pkgload", quietly = TRUE) && file.exists(file.path(repo_root, "DESCRIPTION"))) {
    suppressPackageStartupMessages(pkgload::load_all(repo_root, quiet = TRUE))
  } else {
    suppressPackageStartupMessages(library(tradesimr))
  }
}

call_strategyr <- function(name, ...) {
  fn <- getFromNamespace(name, "strategyr")
  fn(...)
}

require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package `", pkg, "` is required for this script.", call. = FALSE)
  }
}

normalize_okx_bars <- function(x) {
  DT <- data.table::as.data.table(x)
  if (!"datetime" %in% names(DT) && "timestamp" %in% names(DT)) {
    data.table::setnames(DT, "timestamp", "datetime")
  }
  required <- c("datetime", "open", "high", "low", "close")
  missing <- setdiff(required, names(DT))
  if (length(missing) > 0L) {
    stop("Cached OKX data is missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  DT <- DT[order(datetime)]
  DT <- DT[!is.na(datetime) & complete.cases(DT[, ..required])]
  data.table::set(DT, j = "timestamp", value = as.POSIXct(DT$datetime, tz = "UTC"))
  DT[]
}

add_zelina_dual_pulse_targets <- function(DT) {
  call_strategyr("calc_EMA", DT, ns = c(20, 50))
  call_strategyr("calc_ATR", DT, ns = NULL, hs = c(12))
  call_strategyr("calc_ATR_quantile", DT, hs = c(12), thresholds = c(0.05, 0.1, 0.2, 0.3))

  DT[, low_atr_5 := atr_logr_12 < atr_q_5_12_300]
  DT[, low_atr_10 := atr_logr_12 < atr_q_10_12_300]
  DT[, low_atr_20 := atr_logr_12 < atr_q_20_12_300]
  DT[, low_atr_30 := atr_logr_12 < atr_q_30_12_300]

  call_strategyr("calc_EMA_cross", DT, low_atr_threshold = 5)
  call_strategyr("calc_EMA_cross", DT, low_atr_threshold = 10)
  call_strategyr("calc_EMA_cross", DT, low_atr_threshold = 20)
  call_strategyr("calc_EMA_cross", DT, low_atr_threshold = 30)
  call_strategyr("calc_ladder_index", DT, cycle_N = 180L, detailed_report = TRUE)

  tgt_pos_ladder_breakout3 <- ifelse(
    DT$ladder_index_180 %in% c(7, -7),
    -1,
    ifelse(DT$ladder_index_180 %in% c(13, -13), 1, 0)
  )
  tgt_pos_ladder_breakout4 <- ifelse(
    DT$ladder_index_180 %in% c(6, -6),
    -1,
    ifelse(DT$ladder_index_180 %in% c(14, -14), 1, 0)
  )

  A <- 2.5 * DT$tgt_pos_20_50_5
  B <- 1.0 * tgt_pos_ladder_breakout4
  C <- 0.5 * tgt_pos_ladder_breakout3
  D <- 1.0 * DT$tgt_pos_20_50_10
  E <- 0.7 * DT$tgt_pos_20_50_20
  F <- 0.5 * DT$tgt_pos_20_50_30

  DT[, tgt_pos := data.table::fcase(
    A != 0, A,
    B != 0, B,
    C != 0, C,
    D != 0, D,
    E != 0, E,
    F != 0, F,
    default = 0
  )]

  DT[, pos_strat := data.table::fcase(
    A != 0, 1L,
    B != 0, 2L,
    C != 0, 3L,
    D != 0, 4L,
    E != 0, 5L,
    F != 0, 6L,
    default = 0L
  )]

  DT[, tol_pos := data.table::fcase(
    A != 0, 0.3,
    B != 0, 0.05,
    C != 0, 0.1,
    D != 0, 0.1,
    E != 0, 0.1,
    F != 0, 0.1,
    default = 0.1
  )]

  DT[]
}

message("Loading tradesimr from: ", repo_root)
load_tradesimr(repo_root)
require_package("data.table")
require_package("investdatar")
require_package("strategyr")

message("Loading cached OKX candles via investdatar::get_local_okx_candle(\"", inst_id, "\", \"", bar, "\")")
DT <- normalize_okx_bars(investdatar::get_local_okx_candle(inst_id, bar))
message("Rows: ", nrow(DT), "; range: ", min(DT$timestamp), " to ", max(DT$timestamp))

message("Applying Zelina Dual_Pulse target-position logic")
DT <- add_zelina_dual_pulse_targets(DT)

market <- DT[, .(
  timestamp,
  open = as.numeric(open),
  high = as.numeric(high),
  low = as.numeric(low),
  close = as.numeric(close),
  tgt_pos = as.numeric(tgt_pos),
  pos_strat = as.integer(pos_strat),
  tol_pos = as.numeric(tol_pos)
)]

message("Running tradesimr::sim_backtest()")
sim <- sim_backtest(
  market,
  timestamp_col = "timestamp",
  open_col = "open",
  high_col = "high",
  low_col = "low",
  close_col = "close",
  tgt_pos_col = "tgt_pos",
  pos_strat_col = "pos_strat",
  tol_pos_col = "tol_pos",
  strat = 1L,
  asset = 8002L,
  ctr_size = 0.1,
  ctr_step = 0.01,
  lev = 10.0,
  fee_rt = 0.0005,
  fund_rt = 0.0004,
  record = TRUE
)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}
paths <- sim_dashboard_export(sim, out_dir)
account <- sim_account(sim)
metrics <- sim_metrics(sim)

message("Dashboard exported to: ", out_dir)
message("Files written: ", length(paths))
message("Final equity: ", tail(account$equity, 1L))
message("Total return: ", metrics$total_return)
message("Max drawdown: ", metrics$max_drawdown)
