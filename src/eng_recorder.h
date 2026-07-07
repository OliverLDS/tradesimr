// eng_recorder.h
#pragma once

#include <vector>

#include "eng_exchange.h"

struct Recorder {
  std::vector<double> ts;
  std::vector<TRADESIMR::BarStage> bar_stage;
  std::vector<int> action_id_vec;
  std::vector<int> strat_id_vec;
  size_t tx_id = 0;
  std::vector<size_t> tx_id_vec;
  std::vector<TRADESIMR::ActionStatus> status;
  std::vector<bool> liquidation;
  std::vector<TRADESIMR::ActionCode> action;
  std::vector<TRADESIMR::Dir> dir;
  std::vector<double> ctr_qty;
  std::vector<double> price;
  std::vector<double> eq_vec;

  inline void reserve(size_t n) {
    ts.reserve(n);
    bar_stage.reserve(n);
    action_id_vec.reserve(n);
    strat_id_vec.reserve(n);
    tx_id_vec.reserve(n);
    status.reserve(n);
    liquidation.reserve(n);
    action.reserve(n);
    dir.reserve(n);
    ctr_qty.reserve(n);
    price.reserve(n);
    eq_vec.reserve(n);
  }

  inline void append_record(const TradeState& s, const ExchangeMessage_on_trade& m) {
    ts.push_back(m.timestamp);
    bar_stage.push_back(m.bar_stage);
    action_id_vec.push_back(m.action_id);
    strat_id_vec.push_back(m.strat);

    if (m.action == TRADESIMR::ActionCode::OPEN) ++tx_id;
    tx_id_vec.push_back(tx_id);

    status.push_back(m.status);
    liquidation.push_back(m.liquidate);
    action.push_back(m.action);
    dir.push_back(m.action_pos_dir);
    ctr_qty.push_back(m.action_ctr_unit);
    price.push_back(m.action_px);
    eq_vec.push_back(s.eq());
  }

  inline void append_liquidation(double timestamp, TRADESIMR::BarStage stage) {
    ts.push_back(timestamp);
    bar_stage.push_back(stage);
    action_id_vec.push_back(0);
    strat_id_vec.push_back(0);
    tx_id_vec.push_back(tx_id);
    status.push_back(TRADESIMR::ActionStatus::FAILED);
    liquidation.push_back(true);
    action.push_back(TRADESIMR::ActionCode::NONE);
    dir.push_back(TRADESIMR::Dir::FLAT);
    ctr_qty.push_back(TRADESIMR::kNaReal);
    price.push_back(TRADESIMR::kNaReal);
    eq_vec.push_back(0.0);
  }
};
