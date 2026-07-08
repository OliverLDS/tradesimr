#!/usr/bin/env zsh
set -euo pipefail

script_dir="${0:A:h}"
repo_root="${script_dir:h}"
out_dir="$repo_root/scripts/_outputs/live-agent-dashboard"
dashboard_port="8766"
service_url="http://127.0.0.1:8080"
agent_id=""
open_dashboard="1"

usage() {
  cat <<'USAGE'
Usage: scripts/open_live_agent_dashboard.zsh [options]

Exports and serves the agent-facing live UI. The UI connects to an already
running tradesimr live service selected by --service-url. No human agent is
created by default; pass --agent-id to register one before opening the UI.

Options:
  --service-url URL      Running live service URL. Default: http://127.0.0.1:8080.
  --agent-id ID          Register this human agent in the running service.
  --out-dir PATH        Dashboard output directory.
  --dashboard-port PORT Static dashboard port. Default: 8766.
  --no-open             Serve only; do not open the browser.
  -h, --help            Show this help.

Start a live service separately, for example:
  scripts/run_live_state_dashboard.zsh --service-port 8080
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --service-url)
      service_url="$2"
      shift 2
      ;;
    --agent-id)
      agent_id="$2"
      shift 2
      ;;
    --out-dir)
      out_dir="$2"
      shift 2
      ;;
    --dashboard-port)
      dashboard_port="$2"
      shift 2
      ;;
    --no-open)
      open_dashboard="0"
      shift
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

if ! command -v python3 >/dev/null 2>&1; then
  print -u2 "python3 is required to serve the dashboard over HTTP."
  exit 1
fi

Rscript --vanilla -e 'args <- commandArgs(TRUE); repo <- args[[1]]; out <- args[[2]]; if (requireNamespace("pkgload", quietly = TRUE) && file.exists(file.path(repo, "DESCRIPTION"))) { suppressPackageStartupMessages(pkgload::load_all(repo, quiet = TRUE)) } else { suppressPackageStartupMessages(library(tradesimr)) }; sim_agent_dashboard_export(sim_exchange_new(), out)' "$repo_root" "$out_dir"

if [[ -n "$agent_id" ]]; then
  if ! command -v curl >/dev/null 2>&1; then
    print -u2 "curl is required to register --agent-id with the live service."
    exit 1
  fi
  payload="$(python3 -c 'import json,sys; print(json.dumps({"agent_id": sys.argv[1], "agent_type": "human", "status": "active"}))' "$agent_id")"
  curl -sS -X POST -H "Content-Type: text/plain" --data "$payload" "${service_url%/}/agents" >/dev/null
  print "Registered human agent: $agent_id"
fi

encoded_service_url="$(python3 -c 'import urllib.parse,sys; print(urllib.parse.quote(sys.argv[1], safe=""))' "$service_url")"
encoded_agent_id="$(python3 -c 'import urllib.parse,sys; print(urllib.parse.quote(sys.argv[1], safe=""))' "$agent_id")"
url="http://127.0.0.1:${dashboard_port}/index.html?service_url=${encoded_service_url}"
if [[ -n "$agent_id" ]]; then
  url="${url}&agent_id=${encoded_agent_id}"
fi

print "Serving agent dashboard from: $out_dir"
print "Live service URL: $service_url"
if [[ -n "$agent_id" ]]; then
  print "Human agent ID: $agent_id"
else
  print "Human agent ID: none registered by this launcher"
fi
print "Open URL: $url"
print "Press Ctrl-C to stop the local dashboard server."

if [[ "$open_dashboard" == "1" ]] && command -v open >/dev/null 2>&1; then
  open "$url" >/dev/null 2>&1 || true
fi

cd "$out_dir"
python3 -m http.server "$dashboard_port" --bind 127.0.0.1
