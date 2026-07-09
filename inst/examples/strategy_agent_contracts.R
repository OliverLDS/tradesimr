# Strategy-backed AI agent output contracts for tradesimr.
#
# Strategy functions should generate intentions. tradesimr converts those
# intentions into normal order commands and executes them through the exchange.

library(data.table)
library(tradesimr)

exchange <- sim_exchange_new(list(cash = 10000, ctr_step = 1, lev = 10))
sim_asset_add(exchange, "AAPL", asset_id = 101L, asset_class = "stock")
sim_asset_add(exchange, "ETH-USDT-SWAP", asset_id = 202L, asset_class = "crypto_perp")

bars <- data.table(
  timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
  symbol = c("AAPL", "ETH-USDT-SWAP"),
  asset_id = c(101L, 202L),
  open = c(100, 2000),
  high = c(101, 2020),
  low = c(99, 1980),
  close = c(100, 2000)
)
sim_exchange_add_bars(exchange, bars)

# 1. Target vector: latest finite value is used as target exposure.
sim_strategy_register(exchange, "target-vector-demo", function(DT, ...) {
  rep(1, nrow(DT))
})

# 2. Target table by asset: one row per desired asset target.
sim_strategy_register(exchange, "target-table-demo", function(market, ...) {
  data.table(
    symbol = c("AAPL", "ETH-USDT-SWAP"),
    tgt_pos = c(1, -1)
  )
})

# 3. Order-intent table: compatible with strategyr::build_order_intents().
sim_strategy_register(exchange, "order-intent-demo", function(market, ...) {
  data.table(
    asset = c("AAPL", "ETH-USDT-SWAP"),
    side = c("buy", "sell"),
    units = c(1, 1),
    reference_price = c(100, 2000),
    pricing_method = "market",
    intent_type = "rebalance"
  )
})

# 4. Action plan: compatible with strategyr action-plan outputs.
sim_strategy_register(exchange, "action-plan-demo", function(...) {
  list(
    n = 1L,
    actions = list(list(action_id = 1L, strat = 1L, action = 1L, dir = 1L, type = 0L, ctr_qty = 1, px = NaN))
  )
})

sim_agent_add(exchange, "vector-agent", "strategy", list(strategy_id = "target-vector-demo"))
sim_agent_add(exchange, "table-agent", "strategy", list(strategy_id = "target-table-demo"))
sim_agent_add(exchange, "intent-agent", "strategy", list(strategy_id = "order-intent-demo"))
sim_agent_add(exchange, "plan-agent", "strategy", list(strategy_id = "action-plan-demo"))

decisions <- sim_agents_step(exchange, bars)
print(decisions)
print(exchange$agent_strategy_events)
