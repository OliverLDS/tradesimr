# tradesimr News

This changelog follows the repository tags. There is no `v0.8.0` tag in the
current git history; `v0.9.0` follows `v0.7.0`.

## tradesimr 0.14.0

### Durable Portfolio Fill Exports

- Added an append-only `portfolio_fills` ledger with immutable `FILL...`
  identifiers for target-weight portfolio execution.
- Every portfolio fill now records its durable `order_id`, `rebalance_id`,
  agent/asset identity, execution action, quantity, price, fee, realized P&L,
  and applicable target weight.
- `sim_portfolio_export()` now exports the durable ledger in `fills.json`, so
  exports remain complete after subsequent exchange steps and save/load cycles.

## tradesimr 0.13.2

### Fee-Aware Target Execution

- Fixed fee-bearing `tgt_pos` execution at full account exposure. Previously,
  a target of `1` at `lev = 1` could translate to exactly all available
  notional and then fail when the execution fee was applied.
- Target-position and target-weight opening/increase actions now retain their
  decision-boundary sizing intent but are clipped at the actual eligible fill
  price to the largest contract-step quantity satisfying
  `equity - transaction_fee >= initial_margin`.
- This applies to the C++ backtest kernel and incremental target-weight replay
  path. Explicit contract orders intentionally retain their previous
  reject-on-insufficient-margin semantics.
- Added regression tests for fee-aware long, short, fractional, and reversal
  targets, as well as target-weight replay, exact fees/cash/equity, and
  `lev = 1` gross-exposure limits.

## tradesimr 0.13.1

- Restored compatibility with Rcpp 1.0.x, whose `Rcpp::List::create()`
  implementation accepts at most 20 arguments.
- The C++ recorder now returns its final eight state columns through an
  internal nested list; `sim_events()` restores the established flat event
  table before exposing it to callers.
- `sim_portfolio_step()` now derives its public order lifecycle table from
  authoritative C++ before/after states and submitted orders, avoiding an
  unstable portfolio-recorder serialization path on older Rcpp releases.

## tradesimr 0.13.0

### Vox Arena Portfolio Replay

- Added `sim_portfolio_execution()` to define explicit, exchange-locked
  target-weight execution assumptions: next-eligible-bar timing, fees,
  slippage, spread, leverage, maintenance margin, and gross-weight limits.
- Added `sim_portfolio_target_step()`, the public multi-asset target-weight
  replay interface. It translates named registered-symbol weights into
  contract quantities from authoritative account equity and completed-bar or
  carried valuation prices.
- Rebalance plans are accepted atomically under durable `rebalance_id`s. A
  single plan may contain multiple asset orders, including explicit
  close-then-open actions for direction changes.
- Target-weight orders are eligible only on a strictly later bar for their own
  asset. Stale observations create explicit `no_new_bar` outcomes, and assets
  without a genuinely new bar cannot fill or trigger a new strategy reaction.
- `NULL` target weights now record an explicit `no_decision` outcome and keep
  existing positions; rounded unchanged targets record `no_op`; invalid target
  vectors record a rejected rebalance without submitting orders.
- Added `sim_portfolio_export()` for consumer-safe JSON exports of orders,
  fills, positions, valuations, account snapshots, targets, and realized
  weights. The export intentionally excludes credentials and provider payloads.

### Durable State And Validation

- Exchange save/load now preserves portfolio target and rebalance ledgers,
  next-eligible timestamps, target metadata, and the next rebalance sequence.
- Added stable schemas for portfolio targets/rebalances and enriched fill
  records with agent and asset identity.
- Added a runnable five-asset, two-competitor example at
  `inst/examples/vox_arena_portfolio_replay.R`.
- Added tests for target-to-contract conversion, atomic rebalance grouping,
  isolated accounts, next-bar and closed-market behavior, no-op/rejected
  outcomes, fees, save/load resume, and public output schemas.

## tradesimr 0.12.0

- Fixed live exchange order-time filtering so future-dated orders remain
  pending until an eligible bar arrives.
- Symbol-only and asset-id-only orders now resolve the canonical identity from
  the registered asset instead of deriving a conflicting fallback key.
- `sim_exchange_save()` and `sim_exchange_load()` now preserve execution
  configuration while remaining compatible with older save directories.

Released as tag `v0.12.0`.

### Portfolio-Margin Engine

- Added `sim_portfolio_step()`, an exported R wrapper around a new multi-asset
  C++ step kernel.
- Added `portfolio_step_rcpp()`, a C++ kernel that processes one timestamp batch
  across multiple assets for one agent/account.
- Moved portfolio-margin order acceptance into the C++ execution path when
  `portfolio_margin = TRUE`, instead of relying only on post-step R aggregation.
- The portfolio kernel now evaluates explicit order fills against one shared
  cash balance, current multi-asset exposure, a covariance matrix aligned to the
  stepped assets, a sigma multiplier, and a floor margin rate.
- Portfolio-level liquidation now closes the full account state when aggregated
  equity falls below portfolio maintenance.
- Live exchange stepping uses the portfolio kernel automatically for
  `sim_exchange_new(list(portfolio_margin = TRUE, ...))`.

### Order Lifecycle

- Margin-rejected order events now update live exchange order status to
  `failed`.
- Filled order event handling continues to populate price, fee, and realized
  P&L where available.

### Tests And Documentation

- Added tests for hedged cross-asset exposure accepted by the portfolio kernel.
- Added tests for concentrated exposure rejected by the portfolio kernel.
- Added tests for live exchange failed-order status behavior.
- Updated package-level documentation to describe the multi-asset C++
  portfolio-margin step kernel.

## tradesimr 0.11.0

Released as tag `v0.11.0`.

### Market-Model Calibration

- Added `sim_market_model_calibrate()` to estimate market-model settings from
  historical bars.
- Added `sim_market_model_calibrate_exchange()` to calibrate and install a
  market model directly into an exchange.
- Calibration estimates per-asset drift and volatility, AR terms, GARCH-like
  volatility clustering parameters, covariance/correlation matrices, first-PC
  factor loadings, and regime metadata.
- Market-model metadata can be persisted and restored through exchange
  import/export workflows.

### Cross-Asset Covariance And Risk

- Added an editable dashboard correlation matrix grid for live-state feed
  configuration.
- Added frontend matrix validation before POST, including shape, symmetry, and
  positive-semidefinite checks.
- Made covariance mapping safer by aligning matrix rows and columns to displayed
  asset order.
- Expanded cross-asset risk outputs with portfolio allocation,
  asset-class allocation, factor exposure, max drawdown, concentration HHI,
  stress loss, and risk contribution fields.

### Richer Single-Asset Dynamics

- Added dashboard controls for AR coefficients, GARCH alpha/beta, shock
  distribution, jump intensity, jump mean, jump volatility, OHLC model, and
  Brownian bridge step count.
- Added durable per-feed simulation state for AR/GARCH return lags, shock lags,
  sigma state, and regime state.
- Added single-asset regime switching in the feed simulation model.
- Added Brownian-bridge OHLC generation as an alternative to the older stochastic
  wiggle high/low model.
- Added jump and skew shock controls beyond the existing Student-t GARCH
  innovation option.

### Tests

- Added tests for calibration helpers, market-model persistence, correlated
  generation, feed simulation-state persistence, single-asset regime behavior,
  Brownian bridge OHLC behavior, jump shocks, and optional portfolio-margin
  maintenance calculations.

## tradesimr 0.10.0

Released as tag `v0.10.0`.

### Strategy-Backed Agents

- Added strategy-backed AI agent support.
- Added strategy registration helpers:
  `sim_strategy_register()`, `sim_strategy_unregister()`, `sim_strategy_list()`,
  and `sim_strategy_validate_config()`.
- Added strategy adapter contracts for strategy outputs expressed as target
  vectors, target tables by asset, order-intent tables, and action-plan style
  outputs.
- Added diagnostics so strategy-backed agent failures are captured as agent
  strategy events rather than stopping the full agent stepping cycle.
- Added status/error reporting for strategy-backed agents.
- Preserved isolation between broken strategies and other active agents.

### Dashboard And Examples

- Added live-state UI support for displaying strategy configuration and
  strategy-backed agent status.
- Added example strategy-agent contracts under `inst/examples/`.
- Added tests for multi-asset strategyr-style allocation and agent strategy
  output normalization.

## tradesimr 0.9.0

Released as tag `v0.9.0`.

### Asset Registry And Multi-Asset Live Feeds

- Added explicit asset registry support through `sim_asset_add()`,
  `sim_asset_remove()`, and `sim_assets()`.
- Added support for asset metadata including symbol, asset id, status,
  asset class, contract size, tick size, quantity step, base currency, quote
  currency, and creation time.
- Required registered assets for orders and market bars unless auto-registration
  is explicitly enabled.
- Added multi-asset feed configuration so each registered asset can have its own
  symbol, timeframe, timezone, feed mode, start price, drift, volatility, and
  seed.
- Changed live feed stepping so registered assets can advance together rather
  than alternating through one implicit asset stream.

### Coordinated Market Models

- Added coordinated multi-asset market simulation models:
  `multi_asset_random_walk`, `multi_asset_ar_garch`, `factor_random_walk`, and
  `regime_random_walk`.
- Added static correlation/covariance support for multi-asset simulation.
- Added factor-model simulation settings, including loadings, factor
  volatility, and idiosyncratic volatility.
- Added regime-dependent covariance/correlation behavior.
- Added market-model configuration, status, persistence metadata, and dashboard
  visibility.

### Dashboard And Risk

- Updated live-state dashboard asset selection and feed panels for multi-asset
  operation.
- Added cross-asset risk table export and dashboard integration.
- Improved candle-chart asset filtering so the selected candle asset does not
  incorrectly filter unrelated agent decision/ranking panels.
- Added tests around asset registration, multi-asset feed generation, and
  coordinated market-model behavior.

## tradesimr 0.7.0

Released as tag `v0.7.0`.

### Multi-Asset Routing Foundation

- Required symbol or asset id in market bars and explicit orders.
- Added per-agent, per-asset state routing in the live exchange.
- Added shared-cash account aggregation across per-asset C++ states.
- Aggregated positions, unrealized P&L, margin, and equity across assets for
  account views.
- Updated exchange stepping so bars can be accepted in asset batches and orders
  are routed by `agent_id + asset_id`.

### Dashboard Refinement

- Added asset-aware dashboard controls and filters.
- Added asset-specific candle chart behavior.
- Added agent exposure/ranking improvements for multi-asset accounts.
- Improved live-agent and live-state dashboard interactions around order
  submission, account state, and candle visualization.

### Tests And Documentation

- Added tests for market bar asset requirements, order asset requirements,
  multi-asset exchange stepping, and account aggregation.
- Updated package-level documentation for registered tradable assets,
  multi-asset order routing, and shared-cash live accounts.

## tradesimr 0.6.4

Released as tag `v0.6.4`.

### Live-State Dashboard Fixes

- Added `sim_feed_warmup()` for generating historical simulated bars before
  starting a live feed.
- Added UI support for simulating historical bars from the live-state feed
  console.
- Improved candle chart scaling and y-axis behavior.
- Improved live-state auto-refresh and feed-run behavior.
- Removed or simplified several admin-only manual refresh controls that were
  redundant after auto-polling.

### Agent Views And Rankings

- Improved AI agent ranking and decision displays.
- Added detailed exposure modal behavior for per-agent asset exposure.
- Removed no-op/flat decisions from places where they should not appear as real
  orders.
- Reduced dashboard noise by removing panels that were not useful in the
  live-state admin view.

### Engine And Tests

- Added small execution fixes in the C++ exchange path.
- Added tests for feed warmup, dashboard state, and live-agent/live-state
  behavior.

## tradesimr 0.6.3

Released as tag `v0.6.3`.

### Agent Accounts

- Added explicit AI agent registration APIs and service support:
  `sim_agent_add()`, `sim_agent_remove()`, `sim_agent_set_status()`,
  `sim_agent_rankings()`, and `sim_agents_step()`.
- Ensured every AI agent registered by the state admin gets its own account.
- Ensured every human agent registered through the agent-facing UI gets its own
  account.
- Added per-agent ranking outputs and live-state dashboard panels for
  registered AI/human agents.

### AI Agent Competition

- Added simple built-in AI agent behavior for momentum, mean reversion,
  contrarian, and chaos-style agents.
- Added agent decision logging.
- Added service endpoints and dashboard actions to register and step AI agents.
- Added tests for agent registration, agent stepping, account creation, and
  ranking behavior.

### Dashboards

- Updated live-state dashboard for state-admin agent management.
- Updated live-agent dashboard for human registration and trading against other
  agents.
- Added project-level scripts for opening live-agent and live-state dashboards.

## tradesimr 0.6.2

Released as tag `v0.6.2`.

### Dashboard Split

- Split the single dashboard into distinct static applications:
  `inst/dashboard/replay/`, `inst/dashboard/live_state/`, and
  `inst/dashboard/live_agent/`.
- Moved shared dashboard JavaScript and CSS into `inst/dashboard/shared/`.
- Added dashboard-specific R helpers for replay, live-state, and live-agent
  workflows.
- Updated installed scripts and project scripts to open the intended dashboard
  type explicitly.

### Replay Workflow

- Added an inferencer ETH 4h backtest/replay dashboard script under `scripts/`.
- Improved replay dashboard behavior for backtesting and transaction-history
  review.
- Separated read-only replay use cases from live-agent order-entry use cases.

### Service And Script Updates

- Updated live-service script behavior for the new dashboard layout.
- Added tests around dashboard exports and split dashboard assets.

## tradesimr 0.6.1

Released as tag `v0.6.1`.

### Dashboard Export And Visualization

- Improved static dashboard export helpers.
- Added market event export support for candle-chart data.
- Added `sim_market_events()` for accessing market bars associated with a
  simulation/export.
- Added candle chart and dashboard rendering refinements in the original
  dashboard implementation.

### Data Contract

- Improved exported table coverage so dashboard views can consume market events,
  events, orders, fills, account snapshots, and risk snapshots.
- Updated dashboard export documentation and tests.

## tradesimr 0.6.0

Released as tag `v0.6.0`.

### Live Feed Scheduler

- Added feed configuration with symbol, timeframe, timezone, feed mode, start
  time, and random-walk simulation settings.
- Added feed APIs:
  `sim_feed_config()`, `sim_feed_configure()`, `sim_feed_start()`,
  `sim_feed_stop()`, `sim_feed_step()`, and `sim_feed_status()`.
- Added live-service feed controls:
  `/feed/config`, `/feed/start`, `/feed/stop`, `/feed/step`, and
  `/feed/status`.
- Added scheduler behavior to compute completed bar boundaries for timeframes
  such as `4h` or `5m`.
- Added simulation feed mode using deterministic random-walk generation.
- Preserved the adapter boundary so external data packages can later implement
  the feed interface without becoming hard dependencies.

### Service Integration

- Updated the local live service to expose feed controls alongside order and
  state endpoints.
- Updated live-service scripts and tests for scheduled feed stepping.

## tradesimr 0.5.0

Released as tag `v0.5.0`.

### Live Agent Service

- Added append-only command schemas for `agent_commands`, `order_requests`, and
  `order_cancellations`.
- Added R APIs for command submission and processing:
  `sim_agent_command_schema()`, `sim_submit_order()`, `sim_cancel_order()`, and
  `sim_exchange_process_commands()`.
- Added local plumber/jsonlite-based live service helpers:
  `sim_live_service()` and `sim_live_service_run()`.
- Added service endpoints for health, state, order submission, order
  cancellation, and bar submission.
- Added a local shell entrypoint for running the live service.

### Agent Console UI

- Added an Agent Console panel to the dashboard.
- Added UI controls for agent id, side, quantity, quantity semantics, order
  type, limit price, submit, cancel, and status display.
- Connected dashboard buttons to service endpoints rather than direct R/C++
  internals.

### Legacy R6 Cleanup

- Added documentation for legacy `PaperTrader` and `PaperTradingPlatform`
  classes.
- Continued demoting R6 paper trading classes toward teaching/demo wrappers
  rather than authoritative exchange state.

## tradesimr 0.4.0

Released as tag `v0.4.0`.

### Static Dashboard And Scripts

- Added static dashboard assets under `inst/dashboard/`.
- Added R dashboard helpers:
  `sim_dashboard_export()`, `sim_dashboard_open()`, and
  `sim_exchange_dashboard()`.
- Added initial dashboard views for equity curve, account metrics, event
  timeline, orders, fills, and risk snapshots.
- Added installed shell entrypoints under `inst/scripts/` for dashboard export,
  replay export, exchange-step demo, and serving static dashboard output.
- Added `inst/scripts/README.md` documenting installed local scripts.

### Data Contract

- Made the dashboard consume exported CSV files rather than C++ or R6 internals.
- Added manifest/table export behavior for dashboard data.
- Added tests for dashboard export helpers.

## tradesimr 0.3.0

Released as tag `v0.3.0`.

### Incremental C++ Step Kernel

- Added `sim_state()` to represent a reusable incremental execution state.
- Added `sim_step()`, an R wrapper for a C++ one-bar execution step.
- Added `step_rcpp()` as the C++ primitive that accepts prior state, one bar,
  and a batch of explicit order actions.
- Added order normalization for action, direction, order type, contract
  quantity, price, strategy id, and action id.
- Rewired live exchange stepping to use `sim_step()` directly for incremental
  state updates instead of rerunning full history.

### Durable Schemas And Replay

- Added schema version support through `TRADESIMR_SCHEMA_VERSION` and
  `sim_schema_version()`.
- Added import helpers:
  `sim_import()`, `sim_read_manifest()`, `sim_read_table()`,
  `sim_read_events()`, `sim_read_account()`, and `sim_run_from_events()`.
- Added exchange persistence helpers:
  `sim_exchange_save()`, `sim_exchange_load()`, and
  `sim_exchange_export_events()`.
- Added manifest export fields for package version, schema version, table names,
  row counts, and config.

### Explicit Order Semantics

- Added explicit order object fields including side, quantity, quantity type,
  limit price, time in force, agent id, and client order id.
- Made quantity semantics explicit across replay and live stepping paths.
- Added tests for round-trip export/import and incremental stepping behavior.

## tradesimr 0.2.0

Released as tag `v0.2.0`.

### Execution Engine Foundation

- Added the initial stateful simulation engine and C++ execution semantics.
- Added simulated exchange/accounting components for orders, fills, positions,
  cash, fees, funding, margin, liquidation, and event recording.
- Added R wrappers around the C++ backtest engine through `sim_backtest()`.
- Added C++ recording support for execution events, including fills, funding,
  liquidation, cash, equity, position state, notional, realized P&L,
  unrealized P&L, fees, and maintenance margin.

### R Data Interfaces

- Added market data and intent validators:
  `validate_market_data()`, `validate_intents()`, `as_market_bars()`, and
  `as_target_positions()`.
- Added ledger extraction helpers:
  `sim_events()`, `sim_orders()`, `sim_fills()`, `sim_positions()`,
  `sim_cash_ledger()`, `sim_account()`, and `sim_risk()`.
- Added `sim_schemas()` and initial durable table schema definitions.
- Added `sim_export()` for writing simulation outputs to files.

### Exchange Prototype

- Added the first in-memory exchange environment through `sim_exchange_new()`.
- Added exchange helpers for bars, orders, stepping, replay running, account
  views, positions, and new-event access.
- Preserved vectorized `vec_sim_*` helpers as fast approximate baseline tools
  while the stateful engine became the authoritative execution path.
