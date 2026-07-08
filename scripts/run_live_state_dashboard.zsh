#!/usr/bin/env zsh
set -euo pipefail

script_dir="${0:A:h}"
repo_root="${script_dir:h}"
service_port="8080"
dashboard_port="8765"

usage() {
  cat <<'USAGE'
Usage: scripts/run_live_state_dashboard.zsh [options]

Starts the local tradesimr live service and opens the god-facing live-state UI.
This UI configures/steps the feed and inspects state; it does not place orders.

Options:
  --service-port PORT    Live service port. Default: 8080.
  --dashboard-port PORT  Static dashboard port. Default: 8765.
  -h, --help            Show this help.

The live service stays in the foreground. Press Ctrl-C to stop it and the
static dashboard server.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --service-port)
      service_port="$2"
      shift 2
      ;;
    --dashboard-port)
      dashboard_port="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      print -u2 "Unknown argument: $1"
      usage >&2
      exit 2
      ;;
  esac
done

exec zsh "$repo_root/inst/scripts/run_live_service.zsh" "$service_port" state "$dashboard_port"
