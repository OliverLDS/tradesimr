#!/usr/bin/env zsh
set -euo pipefail

if [[ $# -lt 1 || "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  print "Usage: $0 <dashboard_dir> [port]"
  print ""
  print "Serve a directory produced by sim_dashboard_export() or one of:"
  print "  run_backtest_dashboard.zsh"
  print "  run_exchange_step_demo.zsh"
  print "  run_replay_export.zsh"
  print ""
  print "Example:"
  print "  $0 /tmp/tradesimr-backtest-dashboard 8765"
  exit 64
fi

dashboard_dir="$1"
port="${2:-8765}"

if [[ ! -f "$dashboard_dir/index.html" ]]; then
  print -u2 "Dashboard index not found: $dashboard_dir/index.html"
  print -u2 "Run sim_dashboard_export() or one of the export scripts first."
  exit 66
fi

cd "$dashboard_dir"
print "Serving dashboard at http://127.0.0.1:$port"
print "Press Ctrl-C to stop."

if command -v python3 >/dev/null 2>&1; then
  python3 -m http.server "$port" --bind 127.0.0.1
elif command -v python >/dev/null 2>&1; then
  python -m SimpleHTTPServer "$port"
else
  print -u2 "Python is required to serve the static dashboard."
  exit 69
fi
