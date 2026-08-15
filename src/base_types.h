// base_types.h
#pragma once

#include <array>
#include <cmath>

#include "base_utils.h"

struct ActionDecision {
  int action_id = 0;
  int strat = 0;

  TRADESIMR::ActionCode action = TRADESIMR::ActionCode::NONE;
  TRADESIMR::Dir dir = TRADESIMR::Dir::FLAT;
  TRADESIMR::OrderType type = TRADESIMR::OrderType::MARKET;
  double ctr_qty = TRADESIMR::kNaReal;
  double px = TRADESIMR::kNaReal;
  // Target-derived actions may be clipped at the executable price to reserve fees.
  bool fee_aware_target = false;
};

static constexpr size_t MaxActions = 2;

struct ActionPlan {
  std::array<ActionDecision, MaxActions> a;
  size_t n = 0;

  bool append_action(const ActionDecision& action) {
    if (n >= a.size()) return false;
    a[n++] = action;
    return true;
  }
};

struct Intent {
  int strat = 0;
  double tgt_pos = TRADESIMR::kNaReal;
  double tol_pos = 0.0;
  TRADESIMR::OrderType type = TRADESIMR::OrderType::MARKET;
  double px = TRADESIMR::kNaReal;

  bool has_tgt_pos() const noexcept {
    return (tgt_pos != 0.0 && !is_na(tgt_pos));
  }

  TRADESIMR::Dir tgt_pos_dir() const noexcept {
    return (tgt_pos > 0) ? TRADESIMR::Dir::LONG :
      ((tgt_pos < 0) ? TRADESIMR::Dir::SHORT : TRADESIMR::Dir::FLAT);
  }

  double tgt_pos_size() const noexcept {
    return std::abs(tgt_pos);
  }
};

static_assert(ActionPlan{}.n == 0, "ActionPlan must start empty");
