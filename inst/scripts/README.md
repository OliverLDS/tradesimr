# tradesimr installed scripts

These scripts are end-to-end local orchestration entrypoints. They call the
public R API and write durable CSV/dashboard outputs; they do not contain
exchange/accounting logic.

## Scripts

- `run_backtest_dashboard.zsh [out_dir] [--open]`
  - Builds a small synthetic target-position backtest.
  - Exports `manifest.csv`, event/account/risk/order/fill CSVs, and dashboard
    assets.

- `run_exchange_step_demo.zsh [out_dir] [--open]`
  - Runs an incremental simulated exchange step-by-step with explicit contract
    orders.
  - Saves exchange state and exports a static dashboard.

- `run_replay_export.zsh <market_csv> <intent_csv> <out_dir> [--open]`
  - Replays user-provided CSV files through `sim_replay()`.
  - Exports durable tables plus a dashboard.
  - `market_csv` requires `timestamp,open,high,low,close`.
  - `intent_csv` requires `timestamp,tgt_pos`; optional `tol_pos,order_type,limit_price`.

- `serve_dashboard.zsh [dashboard_dir] [port]`
  - Starts a local static HTTP server so browser fetch can read CSV files.

## Installed Location

After package installation, find scripts with:

```r
system.file("scripts", package = "tradesimr")
```

