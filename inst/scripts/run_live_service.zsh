#!/usr/bin/env zsh
set -euo pipefail

port="${1:-8080}"

Rscript - "$port" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
port <- as.integer(args[[1]])

.add_user_libs <- function() {
  candidates <- c(
    Sys.getenv("R_LIBS_USER"),
    file.path(Sys.getenv("HOME"), "Library", "R", paste(R.version$major, R.version$minor, sep = "."), "library"),
    file.path(Sys.getenv("HOME"), "Library", "R", R.version$platform, paste(R.version$major, R.version$minor, sep = "."), "library")
  )
  candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]
  .libPaths(unique(c(candidates, .libPaths())))
}

.add_user_libs()
suppressPackageStartupMessages(library(tradesimr))

missing <- c(
  if (!requireNamespace("plumber", quietly = TRUE)) "plumber",
  if (!requireNamespace("jsonlite", quietly = TRUE)) "jsonlite"
)
if (length(missing) > 0L) {
  stop(
    "Missing service dependencies: ", paste(missing, collapse = ", "), "\n",
    "Install them in this R with: install.packages(c('plumber', 'jsonlite'))\n",
    "Current .libPaths():\n  ", paste(.libPaths(), collapse = "\n  "),
    call. = FALSE
  )
}

exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10, fee_rt = 0.0005))
cat("Starting tradesimr live service at http://127.0.0.1:", port, "\n", sep = "")
cat("Endpoints: GET /health, GET /state, POST /orders, POST /cancel, POST /bars\n")
cat("Feed endpoints: GET /feed/status, POST /feed/config, POST /feed/start, POST /feed/stop, POST /feed/step\n")
sim_live_service_run(exchange, host = "127.0.0.1", port = port)
RSCRIPT
