// eng_exchange.h
#pragma once

#include <cmath>

#include "eng_tradestate.h"

struct ExchangeMessage_on_funding {
  double timestamp = TRADESIMR::kNaReal;
  TRADESIMR::BarStage bar_stage = TRADESIMR::BarStage::CLOSE;
  double cash = TRADESIMR::kNaReal;
  bool liquidate = false;
};

struct ExchangeMessage_on_mark {
  double timestamp = TRADESIMR::kNaReal;
  TRADESIMR::BarStage bar_stage = TRADESIMR::BarStage::CLOSE;
  double last_px = TRADESIMR::kNaReal;
  bool liquidate = false;
};

struct ExchangeMessage_on_trade {
  int action_id = 0;
  double timestamp = TRADESIMR::kNaReal;
  TRADESIMR::BarStage bar_stage = TRADESIMR::BarStage::OPEN;
  int strat = 0;
  TRADESIMR::ActionCode action = TRADESIMR::ActionCode::NONE;
  TRADESIMR::Dir action_pos_dir = TRADESIMR::Dir::FLAT;
  double action_ctr_unit = TRADESIMR::kNaReal;
  double action_px = TRADESIMR::kNaReal;
  TRADESIMR::ActionStatus status = TRADESIMR::ActionStatus::PENDING;

  bool liquidate = false;
  double cash = TRADESIMR::kNaReal;
  TRADESIMR::Dir pos_dir = TRADESIMR::Dir::FLAT;
  double ctr_unit = TRADESIMR::kNaReal;
  double avg_price = TRADESIMR::kNaReal;
};

struct Exchange {
  double old_timestamp = TRADESIMR::kNaReal;
  double timestamp = TRADESIMR::kNaReal;
  double open = TRADESIMR::kNaReal;
  double high = TRADESIMR::kNaReal;
  double low = TRADESIMR::kNaReal;
  double close = TRADESIMR::kNaReal;

  void update_bar(double ts, double o, double h, double l, double c) {
    old_timestamp = timestamp;
    timestamp = ts;
    open = o;
    high = h;
    low = l;
    close = c;
  }

  bool limit_order_filled(double px) const noexcept {
    return px > low && px < high;
  }

  bool is_liquidated(const TradeState& s, double px) const noexcept;
  bool is_liquidated_intra_bar(const TradeState& s) const noexcept;
  ExchangeMessage_on_funding update_on_funding(const TradeState& s) const noexcept;
  ExchangeMessage_on_mark update_on_mark(const TradeState& s) const noexcept;
  ExchangeMessage_on_trade update_on_trade(
    const TradeState& s,
    const ActionDecision& a,
    TRADESIMR::BarStage stage,
    double filled_price
  ) const noexcept;
};

inline bool Exchange::is_liquidated(const TradeState& s, double px) const noexcept {
  double floating_pnl = s.unrealized_pnl(px);
  return s.cash + floating_pnl < s.mm();
}

inline bool Exchange::is_liquidated_intra_bar(const TradeState& s) const noexcept {
  return is_liquidated(s, high) || is_liquidated(s, low);
}

inline ExchangeMessage_on_funding Exchange::update_on_funding(const TradeState& s) const noexcept {
  ExchangeMessage_on_funding out;
  out.timestamp = timestamp;

  if (is_na(old_timestamp)) {
    out.cash = s.cash;
    return out;
  }

  const double dt = timestamp - old_timestamp;
  out.cash = s.cash - s.fund_rt * dt * s.abs_notional() / (60.0 * 60.0 * 8.0);

  if (out.cash + s.unrealized_pnl() < s.mm()) {
    out.cash = 0.0;
    out.liquidate = true;
  }
  return out;
}

inline ExchangeMessage_on_mark Exchange::update_on_mark(const TradeState& s) const noexcept {
  ExchangeMessage_on_mark out;
  out.timestamp = timestamp;
  out.last_px = close;
  if (is_liquidated(s, close)) out.liquidate = true;
  return out;
}

inline ExchangeMessage_on_trade Exchange::update_on_trade(
    const TradeState& s,
    const ActionDecision& a,
    TRADESIMR::BarStage stage,
    double filled_price) const noexcept {
  ExchangeMessage_on_trade out;
  out.timestamp = timestamp;
  out.bar_stage = stage;
  out.action_id = a.action_id;
  out.strat = a.strat;
  out.action = a.action;
  out.action_pos_dir = a.dir;
  out.action_ctr_unit = a.ctr_qty;
  out.action_px = filled_price;

  if (is_liquidated(s, filled_price)) {
    out.status = TRADESIMR::ActionStatus::FAILED;
    out.liquidate = true;
    return out;
  }

  const double trade_units = a.ctr_qty * s.ctr_size;
  const double trade_notional = trade_units * filled_price;
  const double fee = s.fee_rt * trade_notional;

  if (a.action == TRADESIMR::ActionCode::OPEN || a.action == TRADESIMR::ActionCode::INCREASE) {
    const double floating_pnl = s.unrealized_pnl(filled_price);
    const double equity_after_fee = s.cash + floating_pnl - fee;
    const double signed_trade_notional = trade_notional * static_cast<double>(a.dir);
    const double new_abs_notional = std::abs(s.notional() + signed_trade_notional);
    const double required_margin = new_abs_notional * s.imr();

    if (equity_after_fee < required_margin) {
      out.status = TRADESIMR::ActionStatus::FAILED;
      return out;
    }

    out.status = TRADESIMR::ActionStatus::FILLED;
    out.cash = s.cash - fee;
    out.pos_dir = a.dir;
    if (a.action == TRADESIMR::ActionCode::OPEN) {
      out.ctr_unit = a.ctr_qty;
      out.avg_price = filled_price;
    } else {
      out.ctr_unit = s.ctr_unit + a.ctr_qty;
      out.avg_price = (s.ctr_unit * s.avg_price + a.ctr_qty * filled_price) / out.ctr_unit;
    }
    return out;
  }

  if (a.action == TRADESIMR::ActionCode::CLOSE) {
    const double fee_close = s.fee_rt * trade_notional;
    const double dir_sign = static_cast<double>(s.pos_dir);
    const double realized_pnl = trade_units * (filled_price - s.avg_price) * dir_sign;

    out.status = TRADESIMR::ActionStatus::FILLED;
    out.cash = s.cash - fee_close + realized_pnl;
    out.pos_dir = TRADESIMR::Dir::FLAT;
    out.ctr_unit = 0.0;
    out.avg_price = TRADESIMR::kNaReal;
    return out;
  }

  if (a.action == TRADESIMR::ActionCode::REDUCE) {
    const double fee_reduce = s.fee_rt * trade_notional;
    const double dir_sign = static_cast<double>(s.pos_dir);
    const double realized_pnl = trade_units * (filled_price - s.avg_price) * dir_sign;

    out.status = TRADESIMR::ActionStatus::FILLED;
    out.cash = s.cash - fee_reduce + realized_pnl;
    out.pos_dir = a.dir;
    out.ctr_unit = s.ctr_unit - a.ctr_qty;
    out.avg_price = s.avg_price;
    return out;
  }

  out.status = TRADESIMR::ActionStatus::FAILED;
  return out;
}
