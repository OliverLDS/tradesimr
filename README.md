# tradesimr

`tradesimr` is an R-native trading execution and simulation engine with a C++
execution core. It turns strategy intentions and explicit orders into simulated
trades, positions, cash, P&L, risk, and performance outputs under configurable
execution, margin, funding, and cost assumptions.

The package is designed to sit between strategy packages and market-data
adapters:

- Strategy packages, such as `strategyr`, produce signals, target exposures, or
  order intents.
- `tradesimr` executes those intentions under simulated exchange/accounting
  semantics.
- Data adapters, such as `okxr` or other local packages, provide historical or
  live market data.

## Current Scope

- Stateful historical backtesting and replay.
- Incremental live-style exchange stepping.
- Explicit order APIs and append-only command logs.
- Registered tradable assets and multi-asset order routing.
- Per-agent shared-cash live accounts.
- Optional portfolio-margin enforcement through a multi-asset C++ step kernel.
- Durable event schemas with import/export helpers.
- Scheduled simulated feeds with random walk, AR, GARCH, AR-GARCH, factor, and
  regime-style market models.
- Strategy-backed AI agents with diagnostics.
- Static replay, live-state, and live-agent dashboards.
- Local orchestration scripts for examples and dashboard/service launchers.

## Installation

From the local repository:

```r
install.packages("devtools")
devtools::install("/Users/oliver/Documents/2025/_2025-07-21_tradesimr/tradesimr")
```

Or from GitHub:

```r
devtools::install_github("OliverLDS/tradesimr")
```

## Minimal Backtest

```r
library(tradesimr)

bars <- data.frame(
  timestamp = as.POSIXct("2026-01-01", tz = "UTC") + 0:4 * 60,
  open = c(100, 101, 102, 101, 103),
  high = c(101, 102, 103, 102, 104),
  low = c(99, 100, 101, 100, 102),
  close = c(101, 102, 101, 103, 104),
  tgt_pos = c(0, 1, 1, 0, -1)
)

sim <- sim_backtest(bars, init_cash = 10000, lev = 10, fee_rt = 0.0005)

sim_metrics(sim)
sim_orders(sim)
sim_account(sim)
```

## Fee-Aware Target Semantics

Target positions and target weights are first translated into contract actions
at their decision boundary. An opening or increasing target action fills only
on its next eligible bar. At that fill price, tradesimr clips the requested
quantity to the largest contract-step quantity that satisfies:

```text
equity - transaction_fee >= initial_margin
```

For example, a `+1` target with `lev = 1` and a nonzero fee opens a
near-100%-notional long position after reserving the fee, rather than failing
because the original target consumed exactly all cash. Explicit contract orders
are not resized and still fail if their requested quantity violates margin.

## Incremental Exchange Example

```r
library(tradesimr)

exchange <- sim_exchange_new(list(
  cash = 10000,
  ctr_step = 1,
  lev = 10,
  mmr = 0.02,
  portfolio_margin = TRUE
))

sim_asset_add(exchange, "BTC-USDT-SWAP", asset_id = 1L)

bar <- data.frame(
  timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
  symbol = "BTC-USDT-SWAP",
  asset_id = 1L,
  open = 100,
  high = 102,
  low = 99,
  close = 101
)

sim_exchange_add_bars(exchange, bar)
sim_submit_order(
  exchange,
  agent_id = "agent-a",
  symbol = "BTC-USDT-SWAP",
  asset_id = 1L,
  side = "buy",
  qty = 1,
  process = TRUE
)

sim_exchange_step(exchange, bar)
sim_exchange_account(exchange)
sim_exchange_orders(exchange)
```

## Dashboards And Scripts

The package includes separate static dashboards:

- `inst/dashboard/replay/`: read-only backtest/replay dashboard.
- `inst/dashboard/live_state/`: state-admin live market dashboard.
- `inst/dashboard/live_agent/`: agent-facing trading dashboard.

Local entrypoints live under:

- `scripts/`: project-level local orchestration.
- `inst/scripts/`: installed package examples and shell entrypoints.

Example:

```zsh
zsh scripts/run_live_state_dashboard.zsh
zsh scripts/open_live_agent_dashboard.zsh
```

## Persistence

Simulation and exchange state can be exported as durable files:

```r
sim_export(sim, "sim-out")
loaded <- sim_import("sim-out")
```

Live exchange sessions can also be saved and loaded:

```r
sim_exchange_save(exchange, "exchange-out")
exchange2 <- sim_exchange_load("exchange-out")
```

## Development Status

`tradesimr` is under active development. The current design favors stable event
schemas, replayability, and explicit exchange/accounting boundaries before
expanding production-grade live service features.
