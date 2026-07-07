// eng_recorder.h
#pragma once

#include <vector>

#include "eng_exchange.h"

struct Recorder {
  std::vector<double> ts;
  std::vector<int> event_id_vec;
  std::vector<int> event_type_vec;
  std::vector<TRADESIMR::BarStage> bar_stage;
  std::vector<int> action_id_vec;
  std::vector<int> strat_id_vec;
  std::vector<int> asset_id_vec;
  size_t tx_id = 0;
  std::vector<size_t> tx_id_vec;
  std::vector<TRADESIMR::ActionStatus> status;
  std::vector<bool> liquidation;
  std::vector<TRADESIMR::ActionCode> action;
  std::vector<TRADESIMR::Dir> dir;
  std::vector<double> ctr_qty;
  std::vector<double> price;
  std::vector<double> eq_vec;
  std::vector<double> cash_vec;
  std::vector<TRADESIMR::Dir> state_dir_vec;
  std::vector<double> state_ctr_unit_vec;
  std::vector<double> avg_price_vec;
  std::vector<double> last_px_vec;
  std::vector<double> notional_vec;
  std::vector<double> abs_notional_vec;
  std::vector<double> unrealized_pnl_vec;
  std::vector<double> realized_pnl_vec;
  std::vector<double> fee_vec;
  std::vector<double> funding_fee_vec;
  std::vector<double> maintenance_margin_vec;
  int event_id = 0;

  inline void reserve(size_t n) {
    ts.reserve(n);
    event_id_vec.reserve(n);
    event_type_vec.reserve(n);
    bar_stage.reserve(n);
    action_id_vec.reserve(n);
    strat_id_vec.reserve(n);
    asset_id_vec.reserve(n);
    tx_id_vec.reserve(n);
    status.reserve(n);
    liquidation.reserve(n);
    action.reserve(n);
    dir.reserve(n);
    ctr_qty.reserve(n);
    price.reserve(n);
    eq_vec.reserve(n);
    cash_vec.reserve(n);
    state_dir_vec.reserve(n);
    state_ctr_unit_vec.reserve(n);
    avg_price_vec.reserve(n);
    last_px_vec.reserve(n);
    notional_vec.reserve(n);
    abs_notional_vec.reserve(n);
    unrealized_pnl_vec.reserve(n);
    realized_pnl_vec.reserve(n);
    fee_vec.reserve(n);
    funding_fee_vec.reserve(n);
    maintenance_margin_vec.reserve(n);
  }

  inline void append_record(const TradeState& s, const ExchangeMessage_on_trade& m) {
    TradeState after = s;
    if (m.status == TRADESIMR::ActionStatus::FILLED) {
      after.cash = m.cash;
      after.pos_dir = m.pos_dir;
      after.ctr_unit = m.ctr_unit;
      after.avg_price = m.avg_price;
    }

    ts.push_back(m.timestamp);
    event_id_vec.push_back(++event_id);
    event_type_vec.push_back(1);
    bar_stage.push_back(m.bar_stage);
    action_id_vec.push_back(m.action_id);
    strat_id_vec.push_back(m.strat);
    asset_id_vec.push_back(s.asset);

    if (m.action == TRADESIMR::ActionCode::OPEN) ++tx_id;
    tx_id_vec.push_back(tx_id);

    status.push_back(m.status);
    liquidation.push_back(m.liquidate);
    action.push_back(m.action);
    dir.push_back(m.action_pos_dir);
    ctr_qty.push_back(m.action_ctr_unit);
    price.push_back(m.action_px);
    eq_vec.push_back(after.eq());
    cash_vec.push_back(after.cash);
    state_dir_vec.push_back(after.pos_dir);
    state_ctr_unit_vec.push_back(after.ctr_unit);
    avg_price_vec.push_back(after.avg_price);
    last_px_vec.push_back(after.last_px);
    notional_vec.push_back(after.notional());
    abs_notional_vec.push_back(after.abs_notional());
    unrealized_pnl_vec.push_back(after.unrealized_pnl());
    realized_pnl_vec.push_back(m.realized_pnl);
    fee_vec.push_back(m.fee);
    funding_fee_vec.push_back(0.0);
    maintenance_margin_vec.push_back(after.mm());
  }

  inline void append_funding(const TradeState& s, const ExchangeMessage_on_funding& m) {
    ts.push_back(m.timestamp);
    event_id_vec.push_back(++event_id);
    event_type_vec.push_back(2);
    bar_stage.push_back(m.bar_stage);
    action_id_vec.push_back(0);
    strat_id_vec.push_back(s.strat);
    asset_id_vec.push_back(s.asset);
    tx_id_vec.push_back(tx_id);
    status.push_back(m.liquidate ? TRADESIMR::ActionStatus::FAILED : TRADESIMR::ActionStatus::FILLED);
    liquidation.push_back(m.liquidate);
    action.push_back(TRADESIMR::ActionCode::NONE);
    dir.push_back(s.pos_dir);
    ctr_qty.push_back(0.0);
    price.push_back(s.last_px);
    eq_vec.push_back(m.cash + s.unrealized_pnl());
    cash_vec.push_back(m.cash);
    state_dir_vec.push_back(s.pos_dir);
    state_ctr_unit_vec.push_back(s.ctr_unit);
    avg_price_vec.push_back(s.avg_price);
    last_px_vec.push_back(s.last_px);
    notional_vec.push_back(s.notional());
    abs_notional_vec.push_back(s.abs_notional());
    unrealized_pnl_vec.push_back(s.unrealized_pnl());
    realized_pnl_vec.push_back(0.0);
    fee_vec.push_back(0.0);
    funding_fee_vec.push_back(m.funding_fee);
    maintenance_margin_vec.push_back(s.mm());
  }

  inline void append_liquidation(const TradeState& s, double timestamp, TRADESIMR::BarStage stage) {
    ts.push_back(timestamp);
    event_id_vec.push_back(++event_id);
    event_type_vec.push_back(3);
    bar_stage.push_back(stage);
    action_id_vec.push_back(0);
    strat_id_vec.push_back(s.strat);
    asset_id_vec.push_back(s.asset);
    tx_id_vec.push_back(tx_id);
    status.push_back(TRADESIMR::ActionStatus::FAILED);
    liquidation.push_back(true);
    action.push_back(TRADESIMR::ActionCode::NONE);
    dir.push_back(TRADESIMR::Dir::FLAT);
    ctr_qty.push_back(TRADESIMR::kNaReal);
    price.push_back(TRADESIMR::kNaReal);
    eq_vec.push_back(0.0);
    cash_vec.push_back(0.0);
    state_dir_vec.push_back(TRADESIMR::Dir::FLAT);
    state_ctr_unit_vec.push_back(0.0);
    avg_price_vec.push_back(TRADESIMR::kNaReal);
    last_px_vec.push_back(s.last_px);
    notional_vec.push_back(0.0);
    abs_notional_vec.push_back(0.0);
    unrealized_pnl_vec.push_back(0.0);
    realized_pnl_vec.push_back(0.0);
    fee_vec.push_back(0.0);
    funding_fee_vec.push_back(0.0);
    maintenance_margin_vec.push_back(0.0);
  }
};
