// eng_tradestate.h
#pragma once

#include <algorithm>
#include <cmath>

#include "base_types.h"

struct TradeState {
  int strat = 0;
  int asset = 0;

  double ctr_size = 1.0;
  double ctr_step = 0.01;
  double lev = 1.0;
  double mmr = 0.02;

  double fee_rt = 0.0005;
  double fund_rt = 0.0004;
  double funding_interval_hours = 8.0;

  double ctr_unit = 0.0;
  TRADESIMR::Dir pos_dir = TRADESIMR::Dir::FLAT;
  double avg_price = TRADESIMR::kNaReal;
  double cash = 10000.0;
  bool liquidated = false;

  ActionPlan pending;
  size_t action_id_now = 1;

  double last_px = 0.0;

  inline double pos_units() const noexcept {
    return ctr_size * ctr_unit * static_cast<double>(pos_dir);
  }

  inline double notional() const noexcept {
    return pos_units() * last_px;
  }

  inline double abs_notional() const noexcept {
    return std::abs(notional());
  }

  inline double unrealized_pnl(double px) const noexcept {
    if (!has_pos() || std::isnan(avg_price)) return 0.0;
    return (px - avg_price) * pos_units();
  }

  inline double unrealized_pnl() const noexcept {
    return unrealized_pnl(last_px);
  }

  inline double eq() const noexcept {
    return cash + unrealized_pnl();
  }

  inline double cur_pos() const noexcept {
    double e = eq();
    if (e <= 0.0) return TRADESIMR::kInfReal;
    return notional() / e;
  }

  inline double abs_cur_pos() const noexcept {
    return std::abs(cur_pos());
  }

  inline bool has_pos() const noexcept {
    double pu = pos_units();
    return (pu != 0.0 && !is_na(pu));
  }

  inline double imr() const noexcept {
    return 1.0 / lev;
  }

  inline double mm() const noexcept {
    return abs_notional() * mmr;
  }

  // Maximum opening/increasing quantity that remains valid after its fee and
  // initial-margin requirement are both charged at the actual fill price.
  inline double fee_aware_target_qty(double px) const noexcept {
    if (!std::isfinite(px) || px <= 0.0 || !std::isfinite(ctr_size) || ctr_size <= 0.0 ||
        !std::isfinite(ctr_step) || ctr_step <= 0.0 || !std::isfinite(lev) || lev <= 0.0) {
      return 0.0;
    }
    const double equity = cash + unrealized_pnl(px);
    const double current_abs_notional = std::abs(pos_units() * px);
    const double headroom = equity - current_abs_notional * imr();
    const double unit_cost = imr() + std::max(0.0, fee_rt);
    if (!std::isfinite(headroom) || !std::isfinite(unit_cost) || headroom <= 0.0 || unit_cost <= 0.0) {
      return 0.0;
    }
    const double raw_qty = headroom / (unit_cost * px * ctr_size);
    if (!std::isfinite(raw_qty) || raw_qty <= 0.0) return 0.0;
    return std::floor(raw_qty / ctr_step + 1e-10) * ctr_step;
  }

  double delta_pos_to_ctr(double delta_pos) const noexcept;
  ActionPlan plan_action_mkt_ord(const Intent& intent, size_t action_id) const noexcept;
};

inline double TradeState::delta_pos_to_ctr(double delta_pos) const noexcept {
  double e = eq();
  if (e <= 0.0 || is_na(last_px)) return 0.0;

  double gap_notional = delta_pos * e;
  double raw_ctr = gap_notional / (ctr_size * last_px);
  return std::round(raw_ctr / ctr_step) * ctr_step;
}

inline ActionPlan TradeState::plan_action_mkt_ord(const Intent& intent, size_t action_id) const noexcept {
  ActionPlan plan{};
  if (is_na(intent.tgt_pos)) return plan;

  const double gap_pos = intent.tgt_pos - cur_pos();
  if (std::abs(gap_pos) < intent.tol_pos) return plan;

  double ctr_delta = delta_pos_to_ctr(gap_pos);
  if (ctr_delta == 0.0) return plan;

  TRADESIMR::Dir cur_dir = pos_dir;
  TRADESIMR::Dir tgt_dir = intent.tgt_pos_dir();

  if (!has_pos()) {
    ActionDecision o;
    o.action_id = static_cast<int>(action_id);
    o.strat = intent.strat;
    o.action = TRADESIMR::ActionCode::OPEN;
    o.dir = tgt_dir;
    o.type = intent.type;
    o.px = intent.px;
    o.ctr_qty = std::abs(ctr_delta);
    o.fee_aware_target = true;
    plan.a[0] = o;
    plan.n = 1;
    return plan;
  }

  if (!intent.has_tgt_pos()) {
    ActionDecision c;
    c.action_id = static_cast<int>(action_id);
    c.strat = intent.strat;
    c.action = TRADESIMR::ActionCode::CLOSE;
    c.dir = TRADESIMR::Dir::FLAT;
    c.ctr_qty = std::abs(ctr_unit);
    c.fee_aware_target = true;
    plan.a[0] = c;
    plan.n = 1;
    return plan;
  }

  if (tgt_dir != TRADESIMR::Dir::FLAT && cur_dir != tgt_dir) {
    ActionDecision c;
    c.action_id = static_cast<int>(action_id);
    c.strat = intent.strat;
    c.action = TRADESIMR::ActionCode::CLOSE;
    c.dir = TRADESIMR::Dir::FLAT;
    c.ctr_qty = std::abs(ctr_unit);
    c.fee_aware_target = true;
    plan.a[0] = c;
    plan.n = 1;

    double ctr_for_tgt = delta_pos_to_ctr(intent.tgt_pos);
    if (ctr_for_tgt == 0.0) return plan;

    ActionDecision o;
    o.action_id = static_cast<int>(action_id + 1);
    o.strat = intent.strat;
    o.action = TRADESIMR::ActionCode::OPEN;
    o.dir = tgt_dir;
    o.type = intent.type;
    o.px = intent.px;
    o.ctr_qty = std::abs(ctr_for_tgt);
    o.fee_aware_target = true;
    plan.a[1] = o;
    plan.n = 2;
    return plan;
  }

  ActionDecision d;
  d.action_id = static_cast<int>(action_id);
  d.strat = intent.strat;
  d.action = (intent.tgt_pos_size() > abs_cur_pos()) ?
    TRADESIMR::ActionCode::INCREASE : TRADESIMR::ActionCode::REDUCE;
  d.dir = tgt_dir;
  d.type = intent.type;
  d.px = intent.px;
  d.ctr_qty = std::abs(ctr_delta);
  d.fee_aware_target = true;
  plan.a[0] = d;
  plan.n = 1;
  return plan;
}
