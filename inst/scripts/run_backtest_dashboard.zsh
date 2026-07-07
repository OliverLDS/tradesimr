#!/usr/bin/env zsh
set -euo pipefail

out_dir="${1:-$PWD/tradesimr-backtest-dashboard}"
open_flag="${2:-}"

Rscript --vanilla - "$out_dir" "$open_flag" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
out_dir <- normalizePath(args[[1]], mustWork = FALSE)
open_flag <- if (length(args) >= 2L) args[[2]] else ""

suppressPackageStartupMessages({
  library(data.table)
  library(tradesimr)
})

bars <- data.table(
  timestamp = as.POSIXct("2026-01-01", tz = "UTC") + 0:71 * 3600,
  open = 100 + sin(seq(0, 8, length.out = 72)) * 4 + seq(0, 5, length.out = 72),
  high = NA_real_,
  low = NA_real_,
  close = NA_real_,
  tgt_pos = rep(c(0, 1, 1, 0, -1, -1), length.out = 72)
)
bars[, close := open + sin(seq(0, 12, length.out = .N))]
bars[, high := pmax(open, close) + 1]
bars[, low := pmin(open, close) - 1]

sim <- sim_backtest(
  bars,
  ctr_step = 0.01,
  lev = 10,
  fee_rt = 0.0005,
  fund_rt = 0.0001,
  slippage = 0.02,
  spread = 0.01
)

paths <- sim_dashboard_export(sim, out_dir)
cat("Dashboard exported to:", out_dir, "\n")
cat("Files written:", length(paths), "\n")
cat("Final equity:", tail(sim_account(sim)$equity, 1), "\n")

if (identical(open_flag, "--open")) {
  sim_dashboard_open(out_dir)
}
RSCRIPT

