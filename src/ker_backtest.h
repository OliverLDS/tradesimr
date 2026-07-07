// ker_backtest.h
#pragma once

#include "eng_recorder.h"

inline void backtest(double* eq,
                     double* cash,
                     int* pos_dir,
                     double* ctr_unit,
                     double* avg_price,
                     double* last_px,
                     double* notional,
                     double* abs_notional,
                     double* unrealized_pnl,
                     double* maintenance_margin,
                     const double* timestamp,
                     const double* open,
                     const double* high,
                     const double* low,
                     const double* close,
                     const double* tgt_pos,
                     const int* pos_strat,
                     const double* tol_pos,
                     int strat,
                     int asset,
                     double init_cash,
                     double ctr_size,
                     double ctr_step,
                     double lev,
                     double fee_rt,
                     double fund_rt,
                     double mmr,
                     size_t len,
                     bool rec,
                     Recorder* recorder_ptr) {
  TradeState s{};
  s.strat = strat;
  s.asset = asset;
  s.cash = init_cash;
  s.ctr_size = ctr_size;
  s.ctr_step = ctr_step;
  s.lev = lev;
  s.fee_rt = fee_rt;
  s.fund_rt = fund_rt;
  s.mmr = mmr;

  Exchange x{};
  if (rec && recorder_ptr) recorder_ptr->reserve(len);

  auto write_state = [&](size_t i) {
    eq[i] = s.liquidated ? 0.0 : s.eq();
    cash[i] = s.liquidated ? 0.0 : s.cash;
    pos_dir[i] = s.liquidated ? 0 : static_cast<int>(s.pos_dir);
    ctr_unit[i] = s.liquidated ? 0.0 : s.ctr_unit;
    avg_price[i] = s.liquidated ? TRADESIMR::kNaReal : s.avg_price;
    last_px[i] = s.last_px;
    notional[i] = s.liquidated ? 0.0 : s.notional();
    abs_notional[i] = s.liquidated ? 0.0 : s.abs_notional();
    unrealized_pnl[i] = s.liquidated ? 0.0 : s.unrealized_pnl();
    maintenance_margin[i] = s.liquidated ? 0.0 : s.mm();
  };

  for (size_t i = 0; i < len; ++i) {
    if (s.liquidated) {
      write_state(i);
      continue;
    }

    x.update_bar(timestamp[i], open[i], high[i], low[i], close[i]);

    if (s.pending.n > 0) {
      ActionPlan remaining;
      for (size_t j = 0; j < s.pending.n; ++j) {
        const ActionDecision& a = s.pending.a[j];
        if (a.type == TRADESIMR::OrderType::MARKET) {
          ExchangeMessage_on_trade trade_msg = x.update_on_trade(s, a, TRADESIMR::BarStage::OPEN, x.open);
          if (trade_msg.liquidate) {
            if (rec && recorder_ptr) recorder_ptr->append_liquidation(s, trade_msg.timestamp, trade_msg.bar_stage);
            s.liquidated = true;
            eq[i] = 0.0;
            break;
          }

          if (trade_msg.status == TRADESIMR::ActionStatus::FILLED) {
            s.cash = trade_msg.cash;
            s.pos_dir = trade_msg.pos_dir;
            s.ctr_unit = trade_msg.ctr_unit;
            s.avg_price = trade_msg.avg_price;
          }

          if (rec && recorder_ptr) recorder_ptr->append_record(s, trade_msg);
        } else if (x.limit_order_filled(a.px)) {
          ExchangeMessage_on_trade trade_msg = x.update_on_trade(s, a, TRADESIMR::BarStage::INTRA, a.px);
          if (trade_msg.liquidate) {
            if (rec && recorder_ptr) recorder_ptr->append_liquidation(s, trade_msg.timestamp, trade_msg.bar_stage);
            s.liquidated = true;
            eq[i] = 0.0;
            break;
          }

          if (trade_msg.status == TRADESIMR::ActionStatus::FILLED) {
            s.cash = trade_msg.cash;
            s.pos_dir = trade_msg.pos_dir;
            s.ctr_unit = trade_msg.ctr_unit;
            s.avg_price = trade_msg.avg_price;
          }

          if (rec && recorder_ptr) recorder_ptr->append_record(s, trade_msg);
        } else {
          remaining.append_action(a);
        }
      }
      s.pending = remaining;
      if (s.liquidated) {
        write_state(i);
        continue;
      }
    }

    ExchangeMessage_on_funding fund_msg = x.update_on_funding(s);
    if (rec && recorder_ptr && (fund_msg.funding_fee != 0.0 || fund_msg.liquidate)) {
      recorder_ptr->append_funding(s, fund_msg);
    }
    s.cash = fund_msg.cash;
    if (fund_msg.liquidate) {
      if (rec && recorder_ptr) recorder_ptr->append_liquidation(s, fund_msg.timestamp, fund_msg.bar_stage);
      s.liquidated = true;
      write_state(i);
      continue;
    }

    ExchangeMessage_on_mark mark_msg = x.update_on_mark(s);
    s.last_px = mark_msg.last_px;
    if (mark_msg.liquidate) {
      if (rec && recorder_ptr) recorder_ptr->append_liquidation(s, mark_msg.timestamp, mark_msg.bar_stage);
      s.liquidated = true;
      write_state(i);
      continue;
    }

    Intent intent{};
    intent.strat = pos_strat[i];
    intent.tgt_pos = tgt_pos[i];
    intent.tol_pos = tol_pos[i];
    s.pending = s.plan_action_mkt_ord(intent, s.action_id_now);
    s.action_id_now += s.pending.n;

    write_state(i);
  }
}
