# Vox Arena target-weight portfolio replay example.
#
# This file is runnable after installing/loading tradesimr. Vox owns the
# publication schedule and target-weight decisions; tradesimr owns contract
# planning, next-bar fills, accounting, and persistence.

library(tradesimr)

arena <- sim_exchange_new(list(cash = 100000, ctr_step = 1, lev = 1))
assets <- data.frame(
  symbol = c("SPY", "TLT", "GLD", "EURUSD=X", "BTC-USD"),
  asset_id = 1:5,
  asset_class = c("etf", "etf", "commodity", "fx", "crypto_spot"),
  start_price = c(500, 90, 200, 1.08, 65000)
)
for (i in seq_len(nrow(assets))) {
  sim_asset_add(
    arena,
    symbol = assets$symbol[i],
    asset_id = assets$asset_id[i],
    asset_class = assets$asset_class[i],
    qty_step = 1
  )
}

execution <- sim_portfolio_execution(
  timing = "next_eligible_open",
  fee_rt = 0.0005,
  slippage = 0,
  spread = 0,
  lev = 1,
  mmr = 0.02
)

make_bars <- function(timestamp, prices) {
  data.frame(
    timestamp = timestamp,
    symbol = assets$symbol,
    asset_id = assets$asset_id,
    open = prices,
    high = prices * 1.01,
    low = prices * 0.99,
    close = prices
  )
}

day_1 <- make_bars(as.POSIXct("2026-01-02", tz = "UTC"), assets$start_price)

# Two isolated Arena competitors publish different target weights.
sim_portfolio_target_step(
  arena,
  agent_id = "balanced",
  bars = day_1,
  target_weights = c(SPY = 0.35, TLT = 0.25, GLD = 0.15, `EURUSD=X` = 0.10, `BTC-USD` = 0.15),
  execution = execution,
  decision_label = "Vox publication 2026-01-02"
)
sim_portfolio_target_step(
  arena,
  agent_id = "risk-on",
  bars = day_1,
  target_weights = c(SPY = 0.45, TLT = 0.05, GLD = 0.05, `EURUSD=X` = 0.05, `BTC-USD` = 0.40),
  execution = execution,
  decision_label = "Vox publication 2026-01-02"
)

# Next completed observations fill the day-1 decisions at these opens.
day_2 <- make_bars(as.POSIXct("2026-01-05", tz = "UTC"), c(505, 89, 202, 1.09, 67000))
balanced <- sim_portfolio_target_step(arena, "balanced", day_2, execution = execution)
risk_on <- sim_portfolio_target_step(arena, "risk-on", day_2, execution = execution)

balanced$orders
balanced$fills
balanced$positions
balanced$account
balanced$realized_weights

# Persist after a daily run and resume without replaying older bars.
state_dir <- tempfile("vox-arena-state-")
sim_exchange_save(arena, state_dir)
arena <- sim_exchange_load(state_dir)

# Export immutable consumer-safe records. No credentials or provider payloads
# are included in these files.
export_dir <- tempfile("vox-arena-export-")
sim_portfolio_export(arena, "balanced", export_dir)
