# tradesimr News

## tradesimr 0.12.0

- Added `sim_portfolio_step()`, an exported multi-asset C++ portfolio step
  wrapper.
- Added a C++ `portfolio_step_rcpp()` kernel for one-timestamp multi-asset
  execution under one shared cash balance.
- Rewired `sim_exchange_step()` to use the portfolio kernel when
  `portfolio_margin = TRUE`.
- Added covariance-aware portfolio-margin order acceptance and account-level
  liquidation behavior.
- Updated order lifecycle handling so margin-rejected events mark orders as
  `failed`.
- Added tests for C++ portfolio-margin order acceptance, rejection, and live
  exchange order status behavior.

## tradesimr 0.11.0

- Added editable dashboard correlation matrix support with frontend validation.
- Added dashboard controls for AR/GARCH simulation parameters, shock settings,
  and OHLC intrabar model selection.
- Added durable per-feed simulation state for AR/GARCH lags, shock lags, sigma
  state, and regime state.
- Added single-asset regime simulation support.
- Added Brownian-bridge OHLC generation and jump/skew shock controls.
- Added market-model calibration helpers from historical bars:
  `sim_market_model_calibrate()` and `sim_market_model_calibrate_exchange()`.
- Expanded cross-asset risk outputs with allocation, concentration, factor
  exposure, drawdown, and risk-contribution fields.

## tradesimr 0.10.0

- Added strategy-backed AI agents and strategy diagnostics.
- Added strategy adapter contracts for target vectors, target tables, order
  intent tables, and action-plan style outputs.
- Added tests for multi-asset strategyr-style allocation.

## tradesimr 0.9.0

- Added coordinated multi-asset market models, including static covariance,
  AR-GARCH, factor, and regime-style models.
- Added market-model persistence and replay metadata.
- Added cross-asset simulation and warmup behavior so registered assets can
  advance together.

## tradesimr 0.7.0

- Split static dashboard apps into replay, live-state, and live-agent views.
- Added shared dashboard assets under `inst/dashboard/shared/`.
- Added local orchestration scripts under `scripts/` and installed examples
  under `inst/scripts/`.

## tradesimr 0.6.0

- Added static dashboard export helpers and initial dashboard views.
- Added live service endpoints for feed control and local order submission.
- Added feed scheduling controls for simulation and external adapter modes.

## tradesimr 0.5.0

- Added durable event schemas and import/export helpers.
- Added exchange save/load helpers.
- Added append-only command schemas for agent commands, order requests, and
  cancellations.

## tradesimr 0.4.0

- Migrated the stateful C++ execution engine into `tradesimr`.
- Added stable R entrypoints for backtesting, replay, order extraction, and
  metrics.
- Demoted legacy R6 paper-trading classes toward wrapper/demo usage.

