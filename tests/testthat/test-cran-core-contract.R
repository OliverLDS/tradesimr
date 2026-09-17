test_that("the 0.18.x public export inventory is frozen", {
  expected <- c(
    "PaperTrader", "PaperTradingPlatform", "TRADESIMR_ACCOUNT_SCHEMA_VERSION",
    "TRADESIMR_SCHEMA_VERSION", "as_market_bars", "as_target_positions",
    "sim_account", "sim_agent_add", "sim_agent_command_schema",
    "sim_agent_dashboard_export", "sim_agent_dashboard_open", "sim_agent_rankings",
    "sim_agent_remove", "sim_agent_set_status", "sim_agents_step", "sim_asset_add",
    "sim_asset_remove", "sim_assets", "sim_backtest", "sim_bond_schedule_add",
    "sim_bond_schedules", "sim_calendar_expected_bars", "sim_calendar_holidays",
    "sim_calendar_is_open", "sim_calendar_settlement_timestamp", "sim_calendar_spec",
    "sim_cancel_order", "sim_cash_ledger", "sim_cross_asset_risk",
    "sim_dashboard_export", "sim_dashboard_open", "sim_events", "sim_exchange_account",
    "sim_exchange_account_state", "sim_exchange_accrue_carry", "sim_exchange_add_bars",
    "sim_exchange_calendar_exception", "sim_exchange_calendarize_bars",
    "sim_exchange_cancel_order", "sim_exchange_cash_adjust",
    "sim_exchange_cash_balances", "sim_exchange_convert_cash",
    "sim_exchange_corporate_action", "sim_exchange_dashboard",
    "sim_exchange_export_events", "sim_exchange_future_roll", "sim_exchange_fx_rate",
    "sim_exchange_load", "sim_exchange_new", "sim_exchange_new_events",
    "sim_exchange_orders", "sim_exchange_place_order", "sim_exchange_positions",
    "sim_exchange_process_commands", "sim_exchange_run", "sim_exchange_save",
    "sim_exchange_set_carry_rates", "sim_exchange_settle", "sim_exchange_step",
    "sim_exchange_validate_cadence", "sim_export", "sim_feed_config",
    "sim_feed_configure", "sim_feed_start", "sim_feed_status", "sim_feed_step",
    "sim_feed_stop", "sim_feed_warmup", "sim_fills", "sim_heterogeneous_account_step",
    "sim_heterogeneous_order_batch_schema", "sim_import", "sim_instrument_profile",
    "sim_instrument_profiles", "sim_live_service", "sim_live_service_run",
    "sim_live_state_dashboard_open", "sim_manifest", "sim_market_events",
    "sim_market_model_calibrate", "sim_market_model_calibrate_exchange",
    "sim_market_model_config", "sim_market_model_configure", "sim_market_model_status",
    "sim_metrics", "sim_orders", "sim_portfolio_decision_policy",
    "sim_portfolio_execution", "sim_portfolio_execution_quality", "sim_portfolio_export",
    "sim_portfolio_market_step", "sim_portfolio_step", "sim_portfolio_target_replay",
    "sim_portfolio_target_step", "sim_portfolio_target_submit",
    "sim_portfolio_target_submit_batch", "sim_positions", "sim_read_account",
    "sim_read_events", "sim_read_manifest", "sim_read_table", "sim_replay",
    "sim_replay_dashboard_export", "sim_risk", "sim_run_from_events",
    "sim_schema_migrate", "sim_schema_version", "sim_schemas", "sim_spot_step",
    "sim_spot_target_submit", "sim_state", "sim_state_dashboard_export", "sim_step",
    "sim_strategy_list", "sim_strategy_register", "sim_strategy_unregister",
    "sim_strategy_validate_config", "sim_submit_order", "sim_trading_calendars",
    "validate_intents", "validate_market_data", "vec_batch_run_simulations",
    "vec_sim_gen_plot", "vec_sim_gen_summary", "vec_sim_gen_summary_table",
    "vec_sim_run_backtest"
  )

  expect_setequal(getNamespaceExports("tradesimr"), expected)
})

test_that("core durable schema signatures are frozen for 0.18.x", {
  expect_identical(TRADESIMR_SCHEMA_VERSION, "0.18.0")
  expect_identical(TRADESIMR_ACCOUNT_SCHEMA_VERSION, "2.0.0")
  schemas <- sim_schemas()
  signature <- function(x) {
    paste(paste(names(x), vapply(x, typeof, character(1L)), sep = ":"), collapse = "|")
  }
  expected <- c(
    assets = "asset_id:integer|symbol:character|status:character|asset_class:character|instrument_profile:character|contract_size:double|tick_size:double|qty_step:double|base_ccy:character|quote_ccy:character|calendar_id:character|timezone:character|settlement_lag_days:integer|margin_model:character|accounting_model:character|bar_cadence_seconds:double|metadata:character|created_at:double",
    market_events = "timestamp:double|observation_timestamp:double|bar_start:double|bar_end:double|valuation_timestamp:double|market_timezone:character|is_completed:logical|is_tradable:logical|symbol:character|asset_id:integer|open:double|high:double|low:double|close:double",
    agent_orders = "order_id:character|client_order_id:character|agent_id:character|symbol:character|asset_id:integer|timestamp:double|eligible_after:double|settlement_timestamp:double|rebalance_id:character|atomic_group_id:character|target_derived:logical|superseded_by_rebalance_id:character|supersedes_rebalance_id:character|target_weight:double|decision_price:double|order_type:character|side:character|intended_action:character|intended_dir:character|qty_type:character|qty:double|limit_price:double|time_in_force:character|tgt_pos:double|tol_pos:double|status:character|price:double|fee:double|realized_pnl:double|reason_code:character|message:character",
    portfolio_targets = "rebalance_id:character|superseded_by_rebalance_id:character|supersedes_rebalance_id:character|timestamp:double|eligible_after:double|agent_id:character|symbol:character|asset_id:integer|target_weight:double|realized_weight_before:double|decision_equity:double|planned_signed_quantity:double|decision_price:double|status:character|message:character",
    portfolio_rebalances = "rebalance_id:character|superseded_by_rebalance_id:character|supersedes_rebalance_id:character|timestamp:double|agent_id:character|status:character|execution_timing:character|fee_rt:double|slippage:double|spread:double|message:character",
    portfolio_fills = "fill_id:character|timestamp:double|agent_id:character|order_id:character|rebalance_id:character|symbol:character|asset_id:integer|event_id:integer|action_id:integer|side:character|action:character|status:character|action_label:character|status_label:character|dir_label:character|qty:double|ctr_qty:double|price:double|fee:double|realized_pnl:double|reason_code:character|target_weight:double",
    events = "timestamp:double|event_id:integer|event_type:integer|event_type_label:character|action_id:integer|status_label:character|action_label:character|dir_label:character|ctr_qty:double|price:double|cash:double|equity:double",
    fills = "timestamp:double|agent_id:character|symbol:character|asset_id:integer|event_id:integer|action_id:integer|action_label:character|dir_label:character|ctr_qty:double|price:double|fee:double|realized_pnl:double",
    positions = "timestamp:double|agent_id:character|symbol:character|asset_id:integer|pos_dir:integer|pos_label:character|ctr_unit:double|avg_price:double|last_px:double|notional:double|unrealized_pnl:double",
    account_snapshots = "timestamp:double|agent_id:character|symbol:character|asset_id:integer|accounting_model:character|equity:double|cash:double|notional:double|abs_notional:double|unrealized_pnl:double|maintenance_margin:double",
    risk_snapshots = "timestamp:double|agent_id:character|symbol:character|asset_id:integer|equity:double|abs_notional:double|leverage:double|maintenance_margin:double|margin_buffer:double",
    cash_balances = "agent_id:character|currency:character|settled:double|unsettled:double|timestamp:double",
    inventory_positions = "agent_id:character|asset_id:integer|symbol:character|currency:character|units:double|average_cost:double|last_price:double|contract_size:double|accrued_interest:double|timestamp:double",
    margin_positions = "agent_id:character|asset_id:integer|symbol:character|currency:character|signed_units:double|settlement_price:double|last_price:double|contract_size:double|maintenance_rate:double|timestamp:double",
    account_events = "account_event_id:character|timestamp:double|agent_id:character|event_type:character|asset_id:integer|symbol:character|currency:character|amount:double|order_id:character|fill_id:character|atomic_group_id:character|message:character",
    profile_cash_ledger = "entry_id:character|timestamp:double|agent_id:character|currency:character|amount:double|balance_after:double|event_type:character|asset_id:integer|symbol:character|order_id:character|settlement_id:character|message:character",
    settlement_ledger = "settlement_id:character|trade_timestamp:double|due_timestamp:double|settled_timestamp:double|agent_id:character|currency:character|amount:double|asset_id:integer|symbol:character|order_id:character|status:character|message:character"
  )

  actual <- vapply(schemas[names(expected)], signature, character(1L))
  expect_identical(actual, expected)
})
