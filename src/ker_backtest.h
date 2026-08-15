// ker_backtest.h
#pragma once

#include <algorithm>

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
                     const int* order_type,
                     const double* limit_price,
                     int strat,
                     int asset,
                     double init_cash,
                     double ctr_size,
                     double ctr_step,
                     double lev,
                     double fee_rt,
                     double maker_fee_rt,
                     double taker_fee_rt,
                     double fund_rt,
                     double funding_interval_hours,
                     double mmr,
                     int fill_model,
                     double slippage,
                     double spread,
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
  s.funding_interval_hours = funding_interval_hours;
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

  auto fill_price_with_costs = [&](const ActionDecision& a, double base_price) {
    double side = 0.0;
    if (a.action == TRADESIMR::ActionCode::OPEN || a.action == TRADESIMR::ActionCode::INCREASE) {
      side = static_cast<double>(a.dir);
    } else if (a.action == TRADESIMR::ActionCode::CLOSE || a.action == TRADESIMR::ActionCode::REDUCE) {
      side = -static_cast<double>(s.pos_dir);
    }
    if (side == 0.0 || is_na(base_price)) return base_price;
    return base_price + side * (spread / 2.0 + slippage);
  };

  auto trade_fee_rate = [&](const ActionDecision& a) {
    if (a.type == TRADESIMR::OrderType::LIMIT) return maker_fee_rt;
    return taker_fee_rt;
  };

  auto process_trade = [&](const ActionDecision& a, TRADESIMR::BarStage stage, double base_price) {
    TradeState priced_state = s;
    priced_state.fee_rt = trade_fee_rate(a);
    double filled_price = fill_price_with_costs(a, base_price);
    ActionDecision executable = a;
    if (executable.fee_aware_target &&
        (executable.action == TRADESIMR::ActionCode::OPEN || executable.action == TRADESIMR::ActionCode::INCREASE)) {
      executable.ctr_qty = std::min(executable.ctr_qty, priced_state.fee_aware_target_qty(filled_price));
    }
    return x.update_on_trade(priced_state, executable, stage, filled_price);
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
          ExchangeMessage_on_trade trade_msg = process_trade(a, TRADESIMR::BarStage::OPEN, x.open);
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
          ExchangeMessage_on_trade trade_msg = process_trade(a, TRADESIMR::BarStage::INTRA, a.px);
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
    intent.type = (order_type[i] == 1) ? TRADESIMR::OrderType::LIMIT : TRADESIMR::OrderType::MARKET;
    intent.px = limit_price[i];
    s.pending = s.plan_action_mkt_ord(intent, s.action_id_now);
    s.action_id_now += s.pending.n;

    if (fill_model == 1 && s.pending.n > 0) {
      ActionPlan remaining;
      for (size_t j = 0; j < s.pending.n; ++j) {
        const ActionDecision& a = s.pending.a[j];
        bool can_fill = a.type == TRADESIMR::OrderType::MARKET ||
          (a.type == TRADESIMR::OrderType::LIMIT && x.limit_order_filled(a.px));
        if (!can_fill) {
          remaining.append_action(a);
          continue;
        }
        double base_price = a.type == TRADESIMR::OrderType::LIMIT ? a.px : x.close;
        ExchangeMessage_on_trade trade_msg = process_trade(
          a,
          a.type == TRADESIMR::OrderType::LIMIT ? TRADESIMR::BarStage::INTRA : TRADESIMR::BarStage::CLOSE,
          base_price
        );
        if (trade_msg.liquidate) {
          if (rec && recorder_ptr) recorder_ptr->append_liquidation(s, trade_msg.timestamp, trade_msg.bar_stage);
          s.liquidated = true;
          break;
        }
        if (trade_msg.status == TRADESIMR::ActionStatus::FILLED) {
          s.cash = trade_msg.cash;
          s.pos_dir = trade_msg.pos_dir;
          s.ctr_unit = trade_msg.ctr_unit;
          s.avg_price = trade_msg.avg_price;
        }
        if (rec && recorder_ptr) recorder_ptr->append_record(s, trade_msg);
      }
      s.pending = remaining;
    }

    write_state(i);
  }
}
