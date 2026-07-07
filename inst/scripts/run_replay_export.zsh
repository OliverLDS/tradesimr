#!/usr/bin/env zsh
set -euo pipefail

if [[ $# -lt 3 ]]; then
  print -u2 "Usage: $0 <market_csv> <intent_csv> <out_dir> [--open]"
  print -u2 "market_csv columns: timestamp,open,high,low,close"
  print -u2 "intent_csv columns: timestamp,tgt_pos; optional tol_pos,order_type,limit_price"
  exit 64
fi

market_csv="$1"
intent_csv="$2"
out_dir="$3"
open_flag="${4:-}"

Rscript --vanilla - "$market_csv" "$intent_csv" "$out_dir" "$open_flag" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
market_csv <- args[[1]]
intent_csv <- args[[2]]
out_dir <- normalizePath(args[[3]], mustWork = FALSE)
open_flag <- if (length(args) >= 4L) args[[4]] else ""

suppressPackageStartupMessages({
  library(data.table)
  library(tradesimr)
})

market <- fread(market_csv)
intents <- fread(intent_csv)

validate_market_data(market)
validate_intents(intents)

market[, timestamp := as.POSIXct(timestamp, tz = "UTC")]
intents[, timestamp := as.POSIXct(timestamp, tz = "UTC")]
setorderv(market, "timestamp")
setorderv(intents, "timestamp")

data <- intents[market, on = "timestamp", roll = TRUE]
for (col in c("tgt_pos", "tol_pos")) {
  if (!col %in% names(data)) data[, (col) := 0]
  data[is.na(get(col)), (col) := 0]
}

sim <- sim_replay(
  data,
  ctr_step = 0.01,
  lev = 10,
  fee_rt = 0.0005,
  tol_pos_col = "tol_pos",
  order_type_col = if ("order_type" %in% names(data)) "order_type" else NULL,
  limit_price_col = if ("limit_price" %in% names(data)) "limit_price" else NULL
)

durable_dir <- file.path(out_dir, "durable")
dashboard_dir <- file.path(out_dir, "dashboard")
sim_export(sim, durable_dir, format = "csv")
sim_dashboard_export(sim, dashboard_dir)

cat("Durable export written to:", durable_dir, "\n")
cat("Dashboard exported to:", dashboard_dir, "\n")
cat("Events:", nrow(sim_events(sim)), "\n")
cat("Final equity:", tail(sim_account(sim)$equity, 1), "\n")

if (identical(open_flag, "--open")) {
  sim_dashboard_open(dashboard_dir)
}
RSCRIPT

