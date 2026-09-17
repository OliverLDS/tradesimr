# tradesimr CRAN-Core Contract: 0.18.x

## Purpose

`tradesimr` is an R package whose public responsibility is deterministic
trading simulation and paper-execution accounting. Its core turns validated
market observations, explicit orders, and target exposures into durable orders,
fills, positions, cash, account, risk, and performance records.

The package does not provide brokerage connectivity, credentials, live market
data, authentication, multi-user service hosting, or a production web
application. Those concerns belong in adapters and companion services.

## Release Boundary

The `0.18.x` line freezes the current exported symbol inventory and durable
schema signatures. Patch releases may fix defects, improve validation,
documentation, deterministic performance, or cross-platform portability. They
must not remove or rename an export, remove or rename a durable column, change
a durable column type, or change documented execution timing.

Additive APIs and schema changes require the next minor release and a schema
migration. Deprecated compatibility APIs remain available throughout `0.18.x`.
The test suite enforces the export inventory and core schema signatures.

## Stable CRAN Core

The following groups are the supported programmatic interface for ordinary R
users and downstream packages.

| Area | Stable functions |
| --- | --- |
| Input and output | `as_market_bars()`, `as_target_positions()`, `sim_backtest()`, `sim_replay()`, `sim_step()`, `sim_metrics()`, `sim_account()`, `sim_orders()`, `sim_fills()`, `sim_positions()`, `sim_events()`, `sim_state()`, `sim_risk()` |
| Exchange | `sim_exchange_new()`, `sim_exchange_step()`, `sim_exchange_add_bars()`, `sim_exchange_place_order()`, `sim_exchange_cancel_order()`, `sim_exchange_orders()`, `sim_exchange_positions()`, `sim_exchange_account()`, `sim_exchange_save()`, `sim_exchange_load()` |
| Portfolio replay | `sim_portfolio_market_step()`, `sim_portfolio_target_submit()`, `sim_portfolio_target_submit_batch()`, `sim_portfolio_target_step()`, `sim_portfolio_target_replay()`, `sim_portfolio_execution()`, `sim_portfolio_execution_quality()`, `sim_portfolio_export()` |
| Assets and calendars | `sim_asset_add()`, `sim_asset_remove()`, `sim_assets()`, `sim_instrument_profile()`, `sim_instrument_profiles()`, `sim_calendar_spec()`, `sim_calendar_is_open()`, `sim_calendar_expected_bars()`, `sim_exchange_calendarize_bars()` |
| Durable data | `sim_export()`, `sim_import()`, `sim_manifest()`, `sim_read_table()`, `sim_read_events()`, `sim_read_account()`, `sim_schema_version()`, `sim_schemas()`, `sim_schema_migrate()` |
| Risk and typed accounting | `sim_cross_asset_risk()`, `sim_cash_ledger()`, `sim_exchange_cash_balances()`, `sim_exchange_convert_cash()`, `sim_exchange_fx_rate()`, `sim_heterogeneous_account_step()` |

The schema constants `TRADESIMR_SCHEMA_VERSION` and
`TRADESIMR_ACCOUNT_SCHEMA_VERSION` are part of this core contract.

The `synthetic_price_return` instrument profile is also part of the stable
profile contract. It models signed marked price exposure only. It makes no
custody, borrow availability or cost, dividend, funding, carry, settlement,
or corporate-action claims. Target-derived inventory fee scaling preserves
target ratios subject to contract-step rounding and records `fee_scaled` fills
as durable partial execution-quality outcomes. Explicit contract orders remain
all-or-nothing.

## Compatibility and Experimental Surface

The following remain exported in `0.18.x` for compatibility or local tooling,
but are not recommended as new downstream integration points:

- `PaperTrader`, `PaperTradingPlatform`, and `vec_sim_*` are legacy/demo APIs.
- `sim_dashboard_*`, `sim_live_*`, `sim_feed_*`, and `sim_live_service_*` are
  local development tooling. They are not a hosted-service contract.
- `sim_agent_*` and `sim_strategy_*` are experimental agent orchestration.
- Low-level profile lifecycle helpers (`sim_exchange_corporate_action()`,
  `sim_exchange_future_roll()`, bond schedule helpers, and carry/settlement
  helpers) are supported for deterministic accounting examples but may gain
  additive arguments only in a later minor release.

## Durable Schema Rules

`sim_schemas()` is the authoritative typed schema constructor. Durable tables
are versioned by `TRADESIMR_SCHEMA_VERSION`; account-state tables are also
versioned by `TRADESIMR_ACCOUNT_SCHEMA_VERSION`.

Core durable tables are `assets`, `market_events`, `agent_orders`,
`portfolio_targets`, `portfolio_rebalances`, `portfolio_fills`, `events`,
`fills`, `positions`, `account_snapshots`, `risk_snapshots`, `cash_balances`,
`inventory_positions`, `margin_positions`, `account_events`,
`profile_cash_ledger`, and `settlement_ledger`.

Exports are immutable snapshots. `sim_exchange_save()` persists an exchange
state; `sim_exchange_load()` accepts current state and intact legacy serialized
fields. A future schema migration must increment the schema version, preserve
source version metadata, provide `sim_schema_migrate()` coverage, and document
the migration in `NEWS.md`.

## Execution Contract

- A decision produces an accepted, rejected, no-op, or superseded durable
  outcome.
- Accepted orders fill only on a strictly later eligible market observation for
  their asset. Stale or closed-session bars may value accounts but cannot fill
  orders or trigger strategy decisions.
- Target-derived orders may be clipped to contract-step and margin capacity;
  inventory target groups use deterministic fee-aware group scaling, preserving
  target ratios subject to contract-step rounding. `fee_scaled` fills are
  durable partial execution-quality outcomes. Explicit contract orders are not
  silently resized and remain all-or-nothing.
- Orders, fills, rebalances, events, and snapshots retain agent, asset, symbol,
  timestamp, and durable execution identity where applicable.

## Change Process

Before changing this contract: classify the change as a bug fix, additive minor
release, or breaking major release; update this document and `NEWS.md`; add
round-trip/migration and replay-parity tests; then run `R CMD check --as-cran`
on the supported CI matrix.
