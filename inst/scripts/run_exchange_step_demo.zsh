#!/usr/bin/env zsh
set -euo pipefail

out_dir="${1:-$PWD/tradesimr-exchange-step-demo}"
open_flag="${2:-}"

Rscript --vanilla - "$out_dir" "$open_flag" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
out_dir <- normalizePath(args[[1]], mustWork = FALSE)
open_flag <- if (length(args) >= 2L) args[[2]] else ""

suppressPackageStartupMessages({
  library(data.table)
  library(tradesimr)
})

exchange <- sim_exchange_new(list(
  cash = 10000,
  ctr_step = 0.01,
  lev = 10,
  fee_rt = 0.0005,
  fund_rt = 0.0001
))

bars <- data.table(
  timestamp = as.POSIXct("2026-01-01", tz = "UTC") + 0:23 * 3600,
  open = 100 + seq(0, 4, length.out = 24) + sin(seq(0, 5, length.out = 24)),
  high = NA_real_,
  low = NA_real_,
  close = NA_real_
)
bars[, close := open + cos(seq(0, 5, length.out = .N)) * 0.8]
bars[, high := pmax(open, close) + 0.75]
bars[, low := pmin(open, close) - 0.75]

sim_exchange_place_order(exchange, "agent-a", bars$timestamp[1], side = "buy", qty = 1)
sim_exchange_place_order(exchange, "agent-a", bars$timestamp[9], side = "buy", qty = 0.5)
sim_exchange_place_order(exchange, "agent-a", bars$timestamp[16], side = "flat")

for (i in seq_len(nrow(bars))) {
  sim_exchange_step(exchange, bars[i])
}

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
sim_exchange_save(exchange, file.path(out_dir, "exchange_state"))
paths <- sim_dashboard_export(exchange$result, file.path(out_dir, "dashboard"))

cat("Exchange state saved to:", file.path(out_dir, "exchange_state"), "\n")
cat("Dashboard exported to:", file.path(out_dir, "dashboard"), "\n")
cat("Files written:", length(paths), "\n")
cat("Final equity:", sim_exchange_account(exchange)$equity[1], "\n")

if (identical(open_flag, "--open")) {
  sim_dashboard_open(file.path(out_dir, "dashboard"))
}
RSCRIPT

