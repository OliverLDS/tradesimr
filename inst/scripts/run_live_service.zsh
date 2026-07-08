#!/usr/bin/env zsh
set -euo pipefail

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  print "Usage: $0 [service_port] [dashboard] [dashboard_port]"
  print ""
  print "dashboard: none | state | agent"
  print "Examples:"
  print "  $0 8080"
  print "  $0 8080 state 8765"
  print "  $0 8080 agent 8766"
  exit 0
fi

port="${1:-8080}"
dashboard="${2:-none}"
dashboard_port="${3:-8765}"
dashboard_dir="${TMPDIR:-/tmp}/tradesimr-live-${dashboard}-dashboard"
dashboard_pid=""

if [[ "$dashboard" != "none" ]]; then
  if [[ "$dashboard" != "state" && "$dashboard" != "agent" ]]; then
    print -u2 "dashboard must be one of: none, state, agent"
    exit 64
  fi
  if ! command -v python3 >/dev/null 2>&1; then
    print -u2 "python3 is required to serve the static dashboard."
    exit 69
  fi
  Rscript - "$dashboard" "$dashboard_dir" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
dashboard <- args[[1]]
dashboard_dir <- args[[2]]
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
exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 0.01, lev = 10, fee_rt = 0.0005))
if (identical(dashboard, "state")) {
  sim_state_dashboard_export(exchange, dashboard_dir)
} else {
  sim_agent_dashboard_export(exchange, dashboard_dir)
}
cat("Dashboard exported to:", dashboard_dir, "\n")
RSCRIPT
  (
    cd "$dashboard_dir"
    python3 -m http.server "$dashboard_port" --bind 127.0.0.1
  ) &
  dashboard_pid="$!"
  trap '[[ -n "$dashboard_pid" ]] && kill "$dashboard_pid" 2>/dev/null || true' EXIT INT TERM
  dashboard_url="http://127.0.0.1:${dashboard_port}/index.html"
  print "Serving ${dashboard} dashboard at $dashboard_url"
  if command -v open >/dev/null 2>&1; then
    open "$dashboard_url" >/dev/null 2>&1 || true
  fi
fi

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
