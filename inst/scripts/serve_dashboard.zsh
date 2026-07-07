#!/usr/bin/env zsh
set -euo pipefail

dashboard_dir="${1:-$PWD}"
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

