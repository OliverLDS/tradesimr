#!/usr/bin/env zsh
set -euo pipefail

script_dir="${0:A:h}"
repo_root="${script_dir:h}"
out_dir="$repo_root/scripts/_outputs/inferencer-eth-4h-dashboard"
port="8765"
inst_id="ETH-USDT-SWAP"
bar="4H"
open_dashboard="1"

usage() {
  cat <<'USAGE'
Usage: scripts/run_inferencer_eth_4h_dashboard.zsh [options]

Exports and serves a backtest/replay dashboard for cached OKX ETH-USDT-SWAP 4H
data using the Zelina Dual_Pulse target-position logic.

Options:
  --out-dir PATH   Dashboard output directory.
  --inst-id ID     OKX instrument id. Default: ETH-USDT-SWAP.
  --bar BAR        OKX candle interval. Default: 4H.
  --port PORT      Local dashboard HTTP port. Default: 8765.
  --no-open        Serve only; do not open the browser.
  -h, --help       Show this help.

The script keeps the local HTTP server in the foreground. Press Ctrl-C to stop.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --out-dir)
      out_dir="$2"
      shift 2
      ;;
    --inst-id)
      inst_id="$2"
      shift 2
      ;;
    --bar)
      bar="$2"
      shift 2
      ;;
    --port)
      port="$2"
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

Rscript "$script_dir/run_inferencer_eth_4h_dashboard.R" \
  --out-dir "$out_dir" \
  --inst-id "$inst_id" \
  --bar "$bar"

url="http://127.0.0.1:${port}/index.html?mode=backtest"
print "Serving replay dashboard from: $out_dir"
print "Open URL: $url"
print "Press Ctrl-C to stop the local dashboard server."

if [[ "$open_dashboard" == "1" ]] && command -v open >/dev/null 2>&1; then
  open "$url" >/dev/null 2>&1 || true
fi

cd "$out_dir"
python3 -m http.server "$port" --bind 127.0.0.1
