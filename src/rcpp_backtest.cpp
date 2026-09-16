// rcpp_backtest.cpp
#include <Rcpp.h>
#include <algorithm>
#include <string>
#include <vector>

#include "base_ids.h"
#include "ker_backtest.h"
#include "eng_account.h"

template <typename EnumT>
Rcpp::IntegerVector enum_vec_to_int(const std::vector<EnumT>& x) {
  Rcpp::IntegerVector out(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) {
    out[static_cast<R_xlen_t>(i)] = static_cast<int>(x[i]);
  }
  return out;
}

inline Rcpp::IntegerVector size_t_vec_to_int(const std::vector<std::size_t>& x) {
  Rcpp::IntegerVector out(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) {
    out[static_cast<R_xlen_t>(i)] = static_cast<int>(x[i]);
  }
  return out;
}

inline Rcpp::LogicalVector bool_vec_to_logical(const std::vector<bool>& x) {
  Rcpp::LogicalVector out(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) {
    out[static_cast<R_xlen_t>(i)] = x[i] ? TRUE : FALSE;
  }
  return out;
}

inline Rcpp::List recorder_to_list(const Recorder& recorder) {
  // Rcpp versions supported by R 4.2 limit List::create() to 20 arguments.
  // The R boundary flattens this internal tail list before exposing events.
  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("timestamp") = recorder.ts,
    Rcpp::Named("event_id") = recorder.event_id_vec,
    Rcpp::Named("event_type") = recorder.event_type_vec,
    Rcpp::Named("bar_stage") = enum_vec_to_int(recorder.bar_stage),
    Rcpp::Named("action_id") = recorder.action_id_vec,
    Rcpp::Named("strat_id") = recorder.strat_id_vec,
    Rcpp::Named("asset_id") = recorder.asset_id_vec,
    Rcpp::Named("tx_id") = size_t_vec_to_int(recorder.tx_id_vec),
    Rcpp::Named("status") = enum_vec_to_int(recorder.status),
    Rcpp::Named("liquidation") = bool_vec_to_logical(recorder.liquidation),
    Rcpp::Named("action") = enum_vec_to_int(recorder.action),
    Rcpp::Named("dir") = enum_vec_to_int(recorder.dir),
    Rcpp::Named("ctr_qty") = recorder.ctr_qty,
    Rcpp::Named("price") = recorder.price,
    Rcpp::Named("equity") = recorder.eq_vec,
    Rcpp::Named("cash") = recorder.cash_vec,
    Rcpp::Named("state_dir") = enum_vec_to_int(recorder.state_dir_vec),
    Rcpp::Named("state_ctr_unit") = recorder.state_ctr_unit_vec,
    Rcpp::Named("avg_price") = recorder.avg_price_vec,
    Rcpp::Named("tail") = Rcpp::List::create(
      Rcpp::Named("last_px") = recorder.last_px_vec,
      Rcpp::Named("notional") = recorder.notional_vec,
      Rcpp::Named("abs_notional") = recorder.abs_notional_vec,
      Rcpp::Named("unrealized_pnl") = recorder.unrealized_pnl_vec,
      Rcpp::Named("realized_pnl") = recorder.realized_pnl_vec,
      Rcpp::Named("fee") = recorder.fee_vec,
      Rcpp::Named("funding_fee") = recorder.funding_fee_vec,
      Rcpp::Named("maintenance_margin") = recorder.maintenance_margin_vec
    )
  );
  return out;
}

inline TRADESIMR::Dir int_to_dir(int x) {
  if (x > 0) return TRADESIMR::Dir::LONG;
  if (x < 0) return TRADESIMR::Dir::SHORT;
  return TRADESIMR::Dir::FLAT;
}

inline TRADESIMR::ActionCode int_to_action(int x) {
  if (x == 1) return TRADESIMR::ActionCode::OPEN;
  if (x == 2) return TRADESIMR::ActionCode::INCREASE;
  if (x == -1) return TRADESIMR::ActionCode::CLOSE;
  if (x == -2) return TRADESIMR::ActionCode::REDUCE;
  return TRADESIMR::ActionCode::NONE;
}

inline TRADESIMR::OrderType int_to_order_type(int x) {
  return x == 1 ? TRADESIMR::OrderType::LIMIT : TRADESIMR::OrderType::MARKET;
}

inline TradeState list_to_trade_state(const Rcpp::List& state,
                                      int asset,
                                      double close,
                                      double cash,
                                      double ctr_size,
                                      double ctr_step,
                                      double lev,
                                      double fee_rt,
                                      double fund_rt,
                                      double funding_interval_hours,
                                      double mmr) {
  TradeState s{};
  s.strat = state.containsElementNamed("strat") ? Rcpp::as<int>(state["strat"]) : 0;
  s.asset = state.containsElementNamed("asset") ? Rcpp::as<int>(state["asset"]) : asset;
  s.cash = cash;
  s.pos_dir = state.containsElementNamed("pos_dir") ? int_to_dir(Rcpp::as<int>(state["pos_dir"])) : TRADESIMR::Dir::FLAT;
  s.ctr_unit = state.containsElementNamed("ctr_unit") ? Rcpp::as<double>(state["ctr_unit"]) : 0.0;
  s.avg_price = state.containsElementNamed("avg_price") ? Rcpp::as<double>(state["avg_price"]) : TRADESIMR::kNaReal;
  s.last_px = state.containsElementNamed("last_px") ? Rcpp::as<double>(state["last_px"]) : close;
  s.liquidated = state.containsElementNamed("liquidated") ? Rcpp::as<bool>(state["liquidated"]) : false;
  s.action_id_now = state.containsElementNamed("action_id_now") ? Rcpp::as<size_t>(state["action_id_now"]) : 1;
  s.ctr_size = ctr_size;
  s.ctr_step = ctr_step;
  s.lev = lev;
  s.fee_rt = fee_rt;
  s.fund_rt = fund_rt;
  s.funding_interval_hours = funding_interval_hours;
  s.mmr = mmr;
  if (!s.has_pos() && (s.last_px == 0.0 || is_na(s.last_px))) s.last_px = close;
  return s;
}

inline Rcpp::List trade_state_to_list(const TradeState& s, double cash, double timestamp, bool liquidated) {
  return Rcpp::List::create(
    Rcpp::Named("strat") = s.strat,
    Rcpp::Named("asset") = s.asset,
    Rcpp::Named("cash") = liquidated ? 0.0 : cash,
    Rcpp::Named("pos_dir") = liquidated ? 0 : static_cast<int>(s.pos_dir),
    Rcpp::Named("ctr_unit") = liquidated ? 0.0 : s.ctr_unit,
    Rcpp::Named("avg_price") = liquidated ? TRADESIMR::kNaReal : s.avg_price,
    Rcpp::Named("last_px") = s.last_px,
    Rcpp::Named("equity") = liquidated ? 0.0 : cash + s.unrealized_pnl(),
    Rcpp::Named("notional") = liquidated ? 0.0 : s.notional(),
    Rcpp::Named("abs_notional") = liquidated ? 0.0 : s.abs_notional(),
    Rcpp::Named("unrealized_pnl") = liquidated ? 0.0 : s.unrealized_pnl(),
    Rcpp::Named("maintenance_margin") = liquidated ? 0.0 : s.mm(),
    Rcpp::Named("liquidated") = liquidated,
    Rcpp::Named("action_id_now") = static_cast<int>(s.action_id_now),
    Rcpp::Named("old_timestamp") = timestamp
  );
}

inline double portfolio_equity_cpp(const std::vector<TradeState>& states, double cash) {
  double pnl = 0.0;
  for (const auto& s : states) {
    if (!s.liquidated) pnl += s.unrealized_pnl();
  }
  return cash + pnl;
}

inline double portfolio_margin_required_cpp(const std::vector<TradeState>& states,
                                            const Rcpp::NumericMatrix& cov,
                                            double sigma,
                                            double floor_rate) {
  const std::size_t n = states.size();
  std::vector<double> exposure(n, 0.0);
  double abs_exposure = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    if (states[i].liquidated) continue;
    exposure[i] = states[i].notional();
    abs_exposure += std::abs(exposure[i]);
  }
  double variance = 0.0;
  if (cov.nrow() == static_cast<int>(n) && cov.ncol() == static_cast<int>(n)) {
    for (std::size_t i = 0; i < n; ++i) {
      for (std::size_t j = 0; j < n; ++j) {
        variance += exposure[i] * cov(i, j) * exposure[j];
      }
    }
  } else {
    for (std::size_t i = 0; i < n; ++i) {
      variance += exposure[i] * exposure[i] * states[i].mmr * states[i].mmr;
    }
  }
  const double covariance_margin = sigma * std::sqrt(std::max(0.0, variance));
  const double floor_margin = floor_rate * abs_exposure;
  return std::max(covariance_margin, floor_margin);
}

inline double fill_price_with_costs_cpp(const ActionDecision& a,
                                        const TradeState& s,
                                        double base_price,
                                        double spread,
                                        double slippage) {
  double side = 0.0;
  if (a.action == TRADESIMR::ActionCode::OPEN || a.action == TRADESIMR::ActionCode::INCREASE) {
    side = static_cast<double>(a.dir);
  } else if (a.action == TRADESIMR::ActionCode::CLOSE || a.action == TRADESIMR::ActionCode::REDUCE) {
    side = -static_cast<double>(s.pos_dir);
  }
  if (side == 0.0 || is_na(base_price)) return base_price;
  return base_price + side * (spread / 2.0 + slippage);
}

// [[Rcpp::export]]
Rcpp::List backtest_rcpp(const Rcpp::NumericVector& timestamp,
                         const Rcpp::NumericVector& open,
                         const Rcpp::NumericVector& high,
                         const Rcpp::NumericVector& low,
                         const Rcpp::NumericVector& close,
                         const Rcpp::NumericVector& tgt_pos,
                         const Rcpp::IntegerVector& pos_strat,
                         const Rcpp::NumericVector& tol_pos,
                         const Rcpp::IntegerVector& order_type,
                         const Rcpp::NumericVector& limit_price,
                         int strat = 0,
                         int asset = 0,
                         double init_cash = 10000.0,
                         double ctr_size = 1.0,
                         double ctr_step = 1.0,
                         double lev = 10.0,
                         double fee_rt = 0.0,
                         double maker_fee_rt = NA_REAL,
                         double taker_fee_rt = NA_REAL,
                         double fund_rt = 0.0,
                         double funding_interval_hours = 8.0,
                         double mmr = 0.02,
                         int fill_model = 0,
                         double slippage = 0.0,
                         double spread = 0.0,
                         bool rec = false) {
  R_xlen_t n = timestamp.size();

  if (open.size() != n ||
      high.size() != n ||
      low.size() != n ||
      close.size() != n ||
      tgt_pos.size() != n ||
      pos_strat.size() != n ||
      tol_pos.size() != n ||
      order_type.size() != n ||
      limit_price.size() != n) {
    Rcpp::stop("All input vectors must have the same length.");
  }
  if (Rcpp::NumericVector::is_na(maker_fee_rt)) maker_fee_rt = fee_rt;
  if (Rcpp::NumericVector::is_na(taker_fee_rt)) taker_fee_rt = fee_rt;

  Rcpp::NumericVector eq(n);
  Rcpp::NumericVector cash(n);
  Rcpp::IntegerVector pos_dir(n);
  Rcpp::NumericVector ctr_unit(n);
  Rcpp::NumericVector avg_price(n);
  Rcpp::NumericVector last_px(n);
  Rcpp::NumericVector notional(n);
  Rcpp::NumericVector abs_notional(n);
  Rcpp::NumericVector unrealized_pnl(n);
  Rcpp::NumericVector maintenance_margin(n);
  Rcpp::List rec_out = R_NilValue;
  Recorder* recorder_ptr = nullptr;
  if (rec) recorder_ptr = new Recorder();

  backtest(
    eq.begin(),
    cash.begin(),
    pos_dir.begin(),
    ctr_unit.begin(),
    avg_price.begin(),
    last_px.begin(),
    notional.begin(),
    abs_notional.begin(),
    unrealized_pnl.begin(),
    maintenance_margin.begin(),
    timestamp.begin(),
    open.begin(),
    high.begin(),
    low.begin(),
    close.begin(),
    tgt_pos.begin(),
    pos_strat.begin(),
    tol_pos.begin(),
    order_type.begin(),
    limit_price.begin(),
    strat,
    asset,
    init_cash,
    ctr_size,
    ctr_step,
    lev,
    fee_rt,
    maker_fee_rt,
    taker_fee_rt,
    fund_rt,
    funding_interval_hours,
    mmr,
    fill_model,
    slippage,
    spread,
    static_cast<std::size_t>(n),
    rec,
    recorder_ptr
  );

  if (rec && recorder_ptr != nullptr) {
    rec_out = recorder_to_list(*recorder_ptr);
  }

  if (recorder_ptr) delete recorder_ptr;
  return Rcpp::List::create(
    Rcpp::Named("equity") = eq,
    Rcpp::Named("cash") = cash,
    Rcpp::Named("pos_dir") = pos_dir,
    Rcpp::Named("ctr_unit") = ctr_unit,
    Rcpp::Named("avg_price") = avg_price,
    Rcpp::Named("last_px") = last_px,
    Rcpp::Named("notional") = notional,
    Rcpp::Named("abs_notional") = abs_notional,
    Rcpp::Named("unrealized_pnl") = unrealized_pnl,
    Rcpp::Named("maintenance_margin") = maintenance_margin,
    Rcpp::Named("recorder") = rec_out
  );
}

// [[Rcpp::export]]
Rcpp::List step_rcpp(const Rcpp::List& state,
                     double timestamp,
                     double open,
                     double high,
                     double low,
                     double close,
                     const Rcpp::IntegerVector& action,
                     const Rcpp::IntegerVector& dir,
                     const Rcpp::IntegerVector& order_type,
                     const Rcpp::NumericVector& ctr_qty,
                     const Rcpp::NumericVector& price,
                     const Rcpp::IntegerVector& strat_id,
                     const Rcpp::IntegerVector& action_id,
                     const Rcpp::LogicalVector& fee_aware_target,
                     int asset = 0,
                     double ctr_size = 1.0,
                     double ctr_step = 1.0,
                     double lev = 10.0,
                     double fee_rt = 0.0,
                     double maker_fee_rt = NA_REAL,
                     double taker_fee_rt = NA_REAL,
                     double fund_rt = 0.0,
                     double funding_interval_hours = 8.0,
                     double mmr = 0.02,
                     double old_timestamp = NA_REAL,
                     double slippage = 0.0,
                     double spread = 0.0,
                     bool rec = true) {
  R_xlen_t n = action.size();
  if (dir.size() != n ||
      order_type.size() != n ||
      ctr_qty.size() != n ||
      price.size() != n ||
      strat_id.size() != n ||
      action_id.size() != n ||
      fee_aware_target.size() != n) {
    Rcpp::stop("All order vectors must have the same length.");
  }
  if (Rcpp::NumericVector::is_na(maker_fee_rt)) maker_fee_rt = fee_rt;
  if (Rcpp::NumericVector::is_na(taker_fee_rt)) taker_fee_rt = fee_rt;

  TradeState s{};
  s.strat = state.containsElementNamed("strat") ? Rcpp::as<int>(state["strat"]) : 0;
  s.asset = state.containsElementNamed("asset") ? Rcpp::as<int>(state["asset"]) : asset;
  s.cash = state.containsElementNamed("cash") ? Rcpp::as<double>(state["cash"]) : 10000.0;
  s.pos_dir = state.containsElementNamed("pos_dir") ? int_to_dir(Rcpp::as<int>(state["pos_dir"])) : TRADESIMR::Dir::FLAT;
  s.ctr_unit = state.containsElementNamed("ctr_unit") ? Rcpp::as<double>(state["ctr_unit"]) : 0.0;
  s.avg_price = state.containsElementNamed("avg_price") ? Rcpp::as<double>(state["avg_price"]) : TRADESIMR::kNaReal;
  s.last_px = state.containsElementNamed("last_px") ? Rcpp::as<double>(state["last_px"]) : close;
  s.liquidated = state.containsElementNamed("liquidated") ? Rcpp::as<bool>(state["liquidated"]) : false;
  s.action_id_now = state.containsElementNamed("action_id_now") ? Rcpp::as<size_t>(state["action_id_now"]) : 1;
  s.ctr_size = ctr_size;
  s.ctr_step = ctr_step;
  s.lev = lev;
  s.fee_rt = fee_rt;
  s.fund_rt = fund_rt;
  s.funding_interval_hours = funding_interval_hours;
  s.mmr = mmr;

  Exchange x{};
  x.timestamp = Rcpp::NumericVector::is_na(old_timestamp) ? TRADESIMR::kNaReal : old_timestamp;
  x.update_bar(timestamp, open, high, low, close);
  if (!s.has_pos() && (s.last_px == 0.0 || is_na(s.last_px))) {
    s.last_px = open;
  }

  Recorder recorder{};
  if (rec) recorder.reserve(static_cast<size_t>(n) + 2);

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

  auto fee_rate_for = [&](const ActionDecision& a) {
    return a.type == TRADESIMR::OrderType::LIMIT ? maker_fee_rt : taker_fee_rt;
  };

  if (!s.liquidated) {
    for (R_xlen_t i = 0; i < n; ++i) {
      ActionDecision a{};
      a.action_id = action_id[i];
      a.strat = strat_id[i];
      a.action = int_to_action(action[i]);
      a.dir = int_to_dir(dir[i]);
      a.type = int_to_order_type(order_type[i]);
      a.ctr_qty = ctr_qty[i];
      a.px = price[i];
      a.fee_aware_target = fee_aware_target[i] == TRUE;
      if (a.action == TRADESIMR::ActionCode::NONE || a.ctr_qty <= 0.0 || Rcpp::NumericVector::is_na(a.ctr_qty)) {
        continue;
      }
      if (a.type == TRADESIMR::OrderType::LIMIT && !x.limit_order_filled(a.px)) {
        continue;
      }
      double base_price = a.type == TRADESIMR::OrderType::LIMIT ? a.px : x.open;
      TradeState priced_state = s;
      priced_state.fee_rt = fee_rate_for(a);
      ActionDecision executable = a;
      const double filled_price = fill_price_with_costs(executable, base_price);
      if (executable.fee_aware_target &&
          (executable.action == TRADESIMR::ActionCode::OPEN || executable.action == TRADESIMR::ActionCode::INCREASE)) {
        executable.ctr_qty = std::min(executable.ctr_qty, priced_state.fee_aware_target_qty(filled_price));
      }
      ExchangeMessage_on_trade trade_msg = x.update_on_trade(
        priced_state,
        executable,
        a.type == TRADESIMR::OrderType::LIMIT ? TRADESIMR::BarStage::INTRA : TRADESIMR::BarStage::OPEN,
        filled_price
      );
      if (rec) recorder.append_record(s, trade_msg);
      if (trade_msg.liquidate) {
        if (rec) recorder.append_liquidation(s, trade_msg.timestamp, trade_msg.bar_stage);
        s.liquidated = true;
        break;
      }
      if (trade_msg.status == TRADESIMR::ActionStatus::FILLED) {
        s.cash = trade_msg.cash;
        s.pos_dir = trade_msg.pos_dir;
        s.ctr_unit = trade_msg.ctr_unit;
        s.avg_price = trade_msg.avg_price;
      }
      s.action_id_now = std::max(s.action_id_now, static_cast<size_t>(a.action_id + 1));
    }
  }

  if (!s.liquidated) {
    ExchangeMessage_on_funding fund_msg = x.update_on_funding(s);
    if (rec && (fund_msg.funding_fee != 0.0 || fund_msg.liquidate)) recorder.append_funding(s, fund_msg);
    s.cash = fund_msg.cash;
    if (fund_msg.liquidate) {
      if (rec) recorder.append_liquidation(s, fund_msg.timestamp, fund_msg.bar_stage);
      s.liquidated = true;
    }
  }

  if (!s.liquidated) {
    ExchangeMessage_on_mark mark_msg = x.update_on_mark(s);
    s.last_px = mark_msg.last_px;
    if (mark_msg.liquidate) {
      if (rec) recorder.append_liquidation(s, mark_msg.timestamp, mark_msg.bar_stage);
      s.liquidated = true;
    }
  }

  Rcpp::List next_state = Rcpp::List::create(
    Rcpp::Named("strat") = s.strat,
    Rcpp::Named("asset") = s.asset,
    Rcpp::Named("cash") = s.liquidated ? 0.0 : s.cash,
    Rcpp::Named("pos_dir") = s.liquidated ? 0 : static_cast<int>(s.pos_dir),
    Rcpp::Named("ctr_unit") = s.liquidated ? 0.0 : s.ctr_unit,
    Rcpp::Named("avg_price") = s.liquidated ? TRADESIMR::kNaReal : s.avg_price,
    Rcpp::Named("last_px") = s.last_px,
    Rcpp::Named("equity") = s.liquidated ? 0.0 : s.eq(),
    Rcpp::Named("notional") = s.liquidated ? 0.0 : s.notional(),
    Rcpp::Named("abs_notional") = s.liquidated ? 0.0 : s.abs_notional(),
    Rcpp::Named("unrealized_pnl") = s.liquidated ? 0.0 : s.unrealized_pnl(),
    Rcpp::Named("maintenance_margin") = s.liquidated ? 0.0 : s.mm(),
    Rcpp::Named("liquidated") = s.liquidated,
    Rcpp::Named("action_id_now") = static_cast<int>(s.action_id_now),
    Rcpp::Named("old_timestamp") = timestamp
  );

  Rcpp::List event_out = rec ? recorder_to_list(recorder) : Rcpp::List::create();

  return Rcpp::List::create(
    Rcpp::Named("state") = next_state,
    Rcpp::Named("events") = event_out
  );
}

// Shared derivatives portfolio kernel. Both the legacy exported entry point
// and the heterogeneous account bridge use this implementation so that
// admission, fee clipping, funding, covariance margin, and liquidation have
// one native source of truth.
// The derivative algorithm operates on typed margin rows.  The deprecated
// list-based endpoint adapts into this kernel; heterogeneous account stepping
// passes its authoritative margin positions directly.
static Rcpp::List typed_derivative_step_kernel(const Rcpp::DataFrame& margin_positions,
                               const Rcpp::DataFrame& bars,
                               const Rcpp::DataFrame& orders,
                               const Rcpp::NumericMatrix& cov,
                               double shared_cash,
                               const Rcpp::NumericVector& ctr_size,
                               const Rcpp::NumericVector& ctr_step,
                               double lev,
                               double fee_rt,
                               double maker_fee_rt,
                               double taker_fee_rt,
                               double fund_rt,
                               double funding_interval_hours,
                               double mmr,
                               double portfolio_margin_sigma,
                               double portfolio_margin_floor,
                               double old_timestamp,
                               double slippage,
                               double spread,
                               bool rec) {
  if (Rcpp::NumericVector::is_na(maker_fee_rt)) maker_fee_rt = fee_rt;
  if (Rcpp::NumericVector::is_na(taker_fee_rt)) taker_fee_rt = fee_rt;

  Rcpp::IntegerVector bar_asset = bars["asset_id"];
  Rcpp::NumericVector timestamp = bars["timestamp"];
  Rcpp::NumericVector open = bars["open"];
  Rcpp::NumericVector high = bars["high"];
  Rcpp::NumericVector low = bars["low"];
  Rcpp::NumericVector close = bars["close"];
  const R_xlen_t n_assets = bar_asset.size();
  if (timestamp.size() != n_assets || open.size() != n_assets || high.size() != n_assets ||
      low.size() != n_assets || close.size() != n_assets) {
    Rcpp::stop("Portfolio bars must have asset_id, timestamp, open, high, low, and close columns of equal length.");
  }
  if (ctr_size.size() != n_assets || ctr_step.size() != n_assets) {
    Rcpp::stop("Portfolio ctr_size and ctr_step must be aligned to the market-bar asset batch.");
  }

  std::vector<TradeState> state_vec;
  std::vector<Exchange> exchange_vec;
  std::vector<int> asset_ids;
  state_vec.reserve(static_cast<std::size_t>(n_assets));
  exchange_vec.reserve(static_cast<std::size_t>(n_assets));
  asset_ids.reserve(static_cast<std::size_t>(n_assets));

  Rcpp::IntegerVector margin_asset = margin_positions["asset_id"];
  Rcpp::IntegerVector margin_strat = margin_positions.containsElementNamed("strat") ?
    Rcpp::as<Rcpp::IntegerVector>(margin_positions["strat"]) : Rcpp::IntegerVector(margin_asset.size(), 0);
  Rcpp::NumericVector margin_signed = margin_positions["signed_units"];
  Rcpp::NumericVector margin_settlement = margin_positions["settlement_price"];
  Rcpp::NumericVector margin_last = margin_positions["last_price"];
  Rcpp::IntegerVector margin_action_id = margin_positions.containsElementNamed("action_id_now") ?
    Rcpp::as<Rcpp::IntegerVector>(margin_positions["action_id_now"]) : Rcpp::IntegerVector(margin_asset.size(), 1);
  Rcpp::LogicalVector margin_liquidated = margin_positions.containsElementNamed("liquidated") ?
    Rcpp::as<Rcpp::LogicalVector>(margin_positions["liquidated"]) : Rcpp::LogicalVector(margin_asset.size(), false);
  for (R_xlen_t i = 0; i < n_assets; ++i) {
    const int asset = bar_asset[i];
    asset_ids.push_back(asset);
    R_xlen_t mi = -1;
    for (R_xlen_t j = 0; j < margin_asset.size(); ++j) {
      if (margin_asset[j] == asset) { mi = j; break; }
    }
    TradeState s{};
    s.strat = mi >= 0 ? margin_strat[mi] : 0;
    s.asset = asset;
    s.cash = shared_cash;
    const double signed_units = mi >= 0 ? margin_signed[mi] : 0.0;
    s.pos_dir = signed_units > 0.0 ? TRADESIMR::Dir::LONG : (signed_units < 0.0 ? TRADESIMR::Dir::SHORT : TRADESIMR::Dir::FLAT);
    s.ctr_unit = std::abs(signed_units);
    s.avg_price = mi >= 0 ? margin_settlement[mi] : TRADESIMR::kNaReal;
    s.last_px = mi >= 0 ? margin_last[mi] : close[i];
    s.liquidated = mi >= 0 && margin_liquidated[mi] == TRUE;
    s.action_id_now = static_cast<size_t>(mi >= 0 ? margin_action_id[mi] : 1);
    s.ctr_size = ctr_size[i];
    s.ctr_step = ctr_step[i];
    s.lev = lev;
    s.fee_rt = fee_rt;
    s.fund_rt = fund_rt;
    s.funding_interval_hours = funding_interval_hours;
    s.mmr = mmr;
    if (!s.has_pos() && (s.last_px == 0.0 || is_na(s.last_px))) s.last_px = close[i];
    s.last_px = close[i];
    state_vec.push_back(s);

    Exchange x{};
    x.timestamp = Rcpp::NumericVector::is_na(old_timestamp) ? TRADESIMR::kNaReal : old_timestamp;
    x.update_bar(timestamp[i], open[i], high[i], low[i], close[i]);
    exchange_vec.push_back(x);
  }

  Rcpp::CharacterVector order_id;
  Rcpp::IntegerVector order_asset;
  Rcpp::IntegerVector action;
  Rcpp::IntegerVector dir;
  Rcpp::IntegerVector order_type;
  Rcpp::NumericVector ctr_qty;
  Rcpp::NumericVector price;
  Rcpp::IntegerVector strat_id;
  Rcpp::IntegerVector action_id;
  Rcpp::LogicalVector fee_aware_target;
  const bool has_orders = orders.nrows() > 0;
  if (has_orders) {
    order_asset = orders["asset_id"];
    action = orders["action"];
    dir = orders["dir"];
    order_type = orders["order_type"];
    ctr_qty = orders["ctr_qty"];
    price = orders["price"];
    strat_id = orders["strat_id"];
    action_id = orders["action_id"];
    if (orders.containsElementNamed("fee_aware_target")) fee_aware_target = orders["fee_aware_target"];
    if (orders.containsElementNamed("order_id")) order_id = orders["order_id"];
  }

  std::vector<double> event_timestamp;
  std::vector<int> event_id;
  std::vector<int> event_type;
  std::vector<int> event_bar_stage;
  std::vector<int> event_action_id;
  std::vector<int> event_strat_id;
  std::vector<int> event_asset_id;
  std::vector<int> event_tx_id;
  std::vector<int> event_status;
  std::vector<int> event_liquidation;
  std::vector<int> event_action;
  std::vector<int> event_dir;
  std::vector<double> event_ctr_qty;
  std::vector<double> event_price;
  std::vector<double> event_equity;
  std::vector<double> event_cash;
  std::vector<double> event_realized_pnl;
  std::vector<double> event_fee;
  std::vector<double> event_funding_fee;
  std::vector<double> event_maintenance_margin;
  std::vector<int> event_target_clipped;
  int next_event_id = 0;
  int next_tx_id = 0;

  auto fee_rate_for = [&](const ActionDecision& a) {
    return a.type == TRADESIMR::OrderType::LIMIT ? maker_fee_rt : taker_fee_rt;
  };

  for (R_xlen_t oi = 0; oi < (has_orders ? orders.nrows() : 0); ++oi) {
    R_xlen_t si = -1;
    for (R_xlen_t j = 0; j < n_assets; ++j) {
      if (bar_asset[j] == order_asset[oi]) {
        si = j;
        break;
      }
    }
    if (si < 0) continue;
    TradeState s = state_vec[static_cast<std::size_t>(si)];
    if (s.liquidated) continue;

    ActionDecision a{};
    a.action_id = action_id[oi];
    a.strat = strat_id[oi];
    a.action = int_to_action(action[oi]);
    a.dir = int_to_dir(dir[oi]);
    a.type = int_to_order_type(order_type[oi]);
    a.ctr_qty = ctr_qty[oi];
    a.px = price[oi];
    a.fee_aware_target = fee_aware_target.size() == orders.nrows() && fee_aware_target[oi] == TRUE;
    if (a.action == TRADESIMR::ActionCode::NONE || a.ctr_qty <= 0.0 || Rcpp::NumericVector::is_na(a.ctr_qty)) {
      continue;
    }
    Exchange& x = exchange_vec[static_cast<std::size_t>(si)];
    if (a.type == TRADESIMR::OrderType::LIMIT && !x.limit_order_filled(a.px)) continue;

    double base_price = a.type == TRADESIMR::OrderType::LIMIT ? a.px : x.open;
    TradeState priced_state = s;
    priced_state.cash = shared_cash;
    priced_state.fee_rt = fee_rate_for(a);
    // Keep the configured per-asset initial-margin ceiling before applying
    // the portfolio-level covariance margin check below.
    priced_state.lev = lev;
    priced_state.mmr = 0.0;
    ActionDecision executable = a;
    const double filled_price = fill_price_with_costs_cpp(executable, s, base_price, spread, slippage);
    if (executable.fee_aware_target &&
        (executable.action == TRADESIMR::ActionCode::OPEN || executable.action == TRADESIMR::ActionCode::INCREASE)) {
      executable.ctr_qty = std::min(executable.ctr_qty, priced_state.fee_aware_target_qty(filled_price));
    }
    ExchangeMessage_on_trade trade_msg = x.update_on_trade(
      priced_state,
      executable,
      a.type == TRADESIMR::OrderType::LIMIT ? TRADESIMR::BarStage::INTRA : TRADESIMR::BarStage::OPEN,
      filled_price
    );
    const bool can_clip = executable.fee_aware_target &&
      (executable.action == TRADESIMR::ActionCode::OPEN ||
       executable.action == TRADESIMR::ActionCode::INCREASE) &&
      s.ctr_step > 0.0;
    auto commit_trade = [&](const ExchangeMessage_on_trade& message) {
      std::vector<TradeState> candidate = state_vec;
      TradeState candidate_state = s;
      candidate_state.cash = message.cash;
      candidate_state.pos_dir = message.pos_dir;
      candidate_state.ctr_unit = message.ctr_unit;
      candidate_state.avg_price = message.avg_price;
      candidate_state.last_px = message.action_px;
      candidate[static_cast<std::size_t>(si)] = candidate_state;
      const double equity = portfolio_equity_cpp(candidate, message.cash);
      const double required = portfolio_margin_required_cpp(candidate, cov, portfolio_margin_sigma, portfolio_margin_floor);
      if (!std::isfinite(equity) || equity < required) return false;
      shared_cash = message.cash;
      state_vec = candidate;
      return true;
    };
    auto clip_to_portfolio_margin = [&]() {
      const long long max_steps = static_cast<long long>(std::floor(executable.ctr_qty / s.ctr_step + 1e-10));
      long long low = 1;
      long long high = max_steps;
      long long feasible_steps = 0;
      ExchangeMessage_on_trade feasible_msg{};
      while (low <= high) {
        const long long mid = low + (high - low) / 2;
        ActionDecision trial = executable;
        trial.ctr_qty = static_cast<double>(mid) * s.ctr_step;
        ExchangeMessage_on_trade trial_msg = x.update_on_trade(
          priced_state,
          trial,
          a.type == TRADESIMR::OrderType::LIMIT ? TRADESIMR::BarStage::INTRA : TRADESIMR::BarStage::OPEN,
          filled_price
        );
        if (trial_msg.status == TRADESIMR::ActionStatus::FILLED && commit_trade(trial_msg)) {
          // commit_trade mutates state, so restore it until the largest trial is known.
          state_vec[static_cast<std::size_t>(si)] = s;
          shared_cash = s.cash;
          feasible_steps = mid;
          feasible_msg = trial_msg;
          low = mid + 1;
        } else {
          high = mid - 1;
        }
      }
      if (feasible_steps == 0) return false;
      // Commit the selected trial exactly once after restoring the pre-trade state.
      if (!commit_trade(feasible_msg)) return false;
      trade_msg = feasible_msg;
      return true;
    };

    bool committed = false;
    if (trade_msg.status == TRADESIMR::ActionStatus::FILLED) {
      committed = commit_trade(trade_msg);
    }
    if (!committed && can_clip) {
      // This covers both a per-asset pre-check rejection and a shared
      // portfolio-margin rejection of the requested target quantity.
      committed = clip_to_portfolio_margin();
    }
    if (!committed) {
      trade_msg.status = TRADESIMR::ActionStatus::FAILED;
      trade_msg.cash = shared_cash;
      trade_msg.pos_dir = s.pos_dir;
      trade_msg.ctr_unit = s.ctr_unit;
      trade_msg.avg_price = s.avg_price;
      trade_msg.fee = 0.0;
      trade_msg.realized_pnl = 0.0;
    }

    if (rec) {
      TradeState event_state = state_vec[static_cast<std::size_t>(si)];
      event_state.cash = shared_cash;
      if (trade_msg.action == TRADESIMR::ActionCode::OPEN) ++next_tx_id;
      event_timestamp.push_back(trade_msg.timestamp);
      event_id.push_back(++next_event_id);
      event_type.push_back(1);
      event_bar_stage.push_back(static_cast<int>(trade_msg.bar_stage));
      event_action_id.push_back(static_cast<int>(trade_msg.action_id));
      event_strat_id.push_back(static_cast<int>(trade_msg.strat));
      event_asset_id.push_back(s.asset);
      event_tx_id.push_back(next_tx_id);
      event_status.push_back(static_cast<int>(trade_msg.status));
      event_liquidation.push_back(trade_msg.liquidate ? 1 : 0);
      event_action.push_back(static_cast<int>(trade_msg.action));
      event_dir.push_back(static_cast<int>(trade_msg.action_pos_dir));
      event_ctr_qty.push_back(trade_msg.action_ctr_unit);
      event_price.push_back(trade_msg.action_px);
      event_equity.push_back(event_state.eq());
      event_cash.push_back(event_state.cash);
      event_realized_pnl.push_back(trade_msg.realized_pnl);
      event_fee.push_back(trade_msg.fee);
      event_funding_fee.push_back(0.0);
      event_maintenance_margin.push_back(event_state.mm());
      event_target_clipped.push_back(
        executable.fee_aware_target &&
        trade_msg.status == TRADESIMR::ActionStatus::FILLED &&
        trade_msg.action_ctr_unit + s.ctr_step / 2.0 < executable.ctr_qty ? 1 : 0
      );
    }
    state_vec[static_cast<std::size_t>(si)].action_id_now = std::max(
      state_vec[static_cast<std::size_t>(si)].action_id_now,
      static_cast<size_t>(a.action_id + 1)
    );
  }

  for (R_xlen_t i = 0; i < n_assets; ++i) {
    TradeState s = state_vec[static_cast<std::size_t>(i)];
    if (s.liquidated) continue;
    s.cash = shared_cash;
    s.mmr = 0.0;
    ExchangeMessage_on_funding fund_msg = exchange_vec[static_cast<std::size_t>(i)].update_on_funding(s);
    shared_cash = fund_msg.cash;
    ExchangeMessage_on_mark mark_msg = exchange_vec[static_cast<std::size_t>(i)].update_on_mark(s);
    s.last_px = mark_msg.last_px;
    state_vec[static_cast<std::size_t>(i)] = s;
  }

  const double equity = portfolio_equity_cpp(state_vec, shared_cash);
  const double required = portfolio_margin_required_cpp(state_vec, cov, portfolio_margin_sigma, portfolio_margin_floor);
  bool liquidated = (!std::isfinite(equity) || equity < required);
  if (liquidated) {
    shared_cash = 0.0;
    for (auto& s : state_vec) {
      s.cash = 0.0;
      s.pos_dir = TRADESIMR::Dir::FLAT;
      s.ctr_unit = 0.0;
      s.avg_price = TRADESIMR::kNaReal;
      s.liquidated = true;
    }
  }

  Rcpp::List out_states;
  for (R_xlen_t i = 0; i < n_assets; ++i) {
    out_states.push_back(
      trade_state_to_list(state_vec[static_cast<std::size_t>(i)], shared_cash, timestamp[i], liquidated),
      std::to_string(asset_ids[static_cast<std::size_t>(i)])
    );
  }

  Rcpp::List event_out = Rcpp::List::create();
  if (rec && !event_id.empty()) {
    event_out = Rcpp::List::create(
      Rcpp::Named("timestamp") = event_timestamp,
      Rcpp::Named("event_id") = event_id,
      Rcpp::Named("event_type") = event_type,
      Rcpp::Named("bar_stage") = event_bar_stage,
      Rcpp::Named("action_id") = event_action_id,
      Rcpp::Named("strat_id") = event_strat_id,
      Rcpp::Named("asset_id") = event_asset_id,
      Rcpp::Named("tx_id") = event_tx_id,
      Rcpp::Named("status") = event_status,
      Rcpp::Named("liquidation") = event_liquidation,
      Rcpp::Named("action") = event_action,
      Rcpp::Named("dir") = event_dir,
      Rcpp::Named("ctr_qty") = event_ctr_qty,
      Rcpp::Named("price") = event_price,
      Rcpp::Named("equity") = event_equity,
      Rcpp::Named("cash") = event_cash,
      Rcpp::Named("realized_pnl") = event_realized_pnl,
      Rcpp::Named("fee") = event_fee,
      Rcpp::Named("funding_fee") = event_funding_fee,
      Rcpp::Named("maintenance_margin") = event_maintenance_margin
    );
    // Rcpp 1.0.x supports at most 20 arguments in List::create().
    event_out["target_clipped"] = event_target_clipped;
  }
  return Rcpp::List::create(
    Rcpp::Named("states") = out_states,
    Rcpp::Named("cash") = shared_cash,
    Rcpp::Named("equity") = liquidated ? 0.0 : portfolio_equity_cpp(state_vec, shared_cash),
    Rcpp::Named("maintenance_margin") = liquidated ? 0.0 : required,
    Rcpp::Named("liquidated") = liquidated,
    Rcpp::Named("events") = event_out
  );
}

// Deprecated compatibility endpoint. New portfolio stepping reaches the same
// native kernel through heterogeneous_account_step_rcpp().
// [[Rcpp::export]]
Rcpp::List portfolio_step_rcpp(const Rcpp::List& states,
                               const Rcpp::DataFrame& bars,
                               const Rcpp::DataFrame& orders,
                               const Rcpp::NumericMatrix& cov,
                               double shared_cash,
                               const Rcpp::NumericVector& ctr_size,
                               const Rcpp::NumericVector& ctr_step,
                               double lev,
                               double fee_rt,
                               double maker_fee_rt,
                               double taker_fee_rt,
                               double fund_rt,
                               double funding_interval_hours,
                               double mmr,
                               double portfolio_margin_sigma,
                               double portfolio_margin_floor,
                               double old_timestamp,
                               double slippage,
                               double spread,
                               bool rec) {
  Rcpp::IntegerVector bar_asset = bars["asset_id"];
  Rcpp::CharacterVector state_names = states.names();
  Rcpp::IntegerVector asset_id(bar_asset.size()), strat(bar_asset.size()), action_id_now(bar_asset.size(), 1);
  Rcpp::NumericVector signed_units(bar_asset.size()), settlement_price(bar_asset.size(), NA_REAL),
    last_price(bar_asset.size(), NA_REAL), contract_size(bar_asset.size());
  Rcpp::LogicalVector liquidated(bar_asset.size(), false);
  for (R_xlen_t i = 0; i < bar_asset.size(); ++i) {
    asset_id[i] = bar_asset[i];
    contract_size[i] = ctr_size[i];
    for (R_xlen_t j = 0; j < states.size(); ++j) {
      if (state_names.size() <= j || Rcpp::as<std::string>(state_names[j]) != std::to_string(bar_asset[i])) continue;
      Rcpp::List state = states[j];
      strat[i] = state.containsElementNamed("strat") ? Rcpp::as<int>(state["strat"]) : 0;
      const int dir = state.containsElementNamed("pos_dir") ? Rcpp::as<int>(state["pos_dir"]) : 0;
      const double units = state.containsElementNamed("ctr_unit") ? Rcpp::as<double>(state["ctr_unit"]) : 0.0;
      signed_units[i] = dir * units;
      settlement_price[i] = state.containsElementNamed("avg_price") ? Rcpp::as<double>(state["avg_price"]) : NA_REAL;
      last_price[i] = state.containsElementNamed("last_px") ? Rcpp::as<double>(state["last_px"]) : NA_REAL;
      action_id_now[i] = state.containsElementNamed("action_id_now") ? Rcpp::as<int>(state["action_id_now"]) : 1;
      liquidated[i] = state.containsElementNamed("liquidated") && Rcpp::as<bool>(state["liquidated"]);
      break;
    }
  }
  Rcpp::DataFrame margin_positions = Rcpp::DataFrame::create(
    Rcpp::Named("asset_id") = asset_id, Rcpp::Named("strat") = strat,
    Rcpp::Named("signed_units") = signed_units, Rcpp::Named("settlement_price") = settlement_price,
    Rcpp::Named("last_price") = last_price, Rcpp::Named("contract_size") = contract_size,
    Rcpp::Named("action_id_now") = action_id_now, Rcpp::Named("liquidated") = liquidated
  );
  return typed_derivative_step_kernel(
    margin_positions, bars, orders, cov, shared_cash, ctr_size, ctr_step, lev, fee_rt,
    maker_fee_rt, taker_fee_rt, fund_rt, funding_interval_hours, mmr,
    portfolio_margin_sigma, portfolio_margin_floor, old_timestamp, slippage,
    spread, rec
  );
}

// [[Rcpp::export]]
Rcpp::List spot_step_rcpp(const Rcpp::List& state,
                          double close,
                          double signed_qty = 0.0,
                          double execution_price = NA_REAL,
                          double contract_size = 1.0,
                          double fee_rt = 0.0,
                          double dividend_per_unit = 0.0,
                          double split_ratio = 1.0) {
  if (!R_finite(close) || close <= 0.0 || !R_finite(contract_size) || contract_size <= 0.0 ||
      !R_finite(fee_rt) || fee_rt < 0.0 || !R_finite(split_ratio) || split_ratio <= 0.0) {
    Rcpp::stop("Spot prices, contract size, and split ratio must be positive; fees must be non-negative.");
  }
  double cash = state.containsElementNamed("cash") ? Rcpp::as<double>(state["cash"]) : 0.0;
  double units = state.containsElementNamed("units") ? Rcpp::as<double>(state["units"]) : 0.0;
  double avg_cost = state.containsElementNamed("avg_cost") ? Rcpp::as<double>(state["avg_cost"]) : NA_REAL;
  if (!R_finite(cash) || !R_finite(units) || units < 0.0) Rcpp::stop("Spot state requires finite non-negative cash and units.");
  if (units == 0.0) avg_cost = NA_REAL;

  const double px = R_finite(execution_price) ? execution_price : close;
  if (!R_finite(px) || px <= 0.0) Rcpp::stop("`execution_price` must be positive when supplied.");
  const double units_before = units;
  double fee = 0.0;
  double realized_pnl = 0.0;
  double dividend_cash = 0.0;
  bool filled = true;
  std::string status = "no_op";

  if (split_ratio != 1.0 && units > 0.0) {
    units *= split_ratio;
    avg_cost /= split_ratio;
  }
  if (dividend_per_unit != 0.0 && units > 0.0) {
    dividend_cash = units * dividend_per_unit * contract_size;
    cash += dividend_cash;
  }
  if (signed_qty > 0.0) {
    const double notional = signed_qty * px * contract_size;
    fee = notional * fee_rt;
    if (cash + 1e-10 < notional + fee) {
      filled = false;
      fee = 0.0;
      status = "rejected_insufficient_cash";
    } else {
      const double prior_cost = units * (R_finite(avg_cost) ? avg_cost : 0.0);
      cash -= notional + fee;
      units += signed_qty;
      avg_cost = (prior_cost + signed_qty * px) / units;
      status = "filled_buy";
    }
  } else if (signed_qty < 0.0) {
    const double sell_units = -signed_qty;
    if (sell_units > units + 1e-10) {
      filled = false;
      status = "rejected_insufficient_inventory";
    } else {
      const double notional = sell_units * px * contract_size;
      fee = notional * fee_rt;
      realized_pnl = (px - avg_cost) * sell_units * contract_size - fee;
      cash += notional - fee;
      units -= sell_units;
      if (units <= 1e-10) {
        units = 0.0;
        avg_cost = NA_REAL;
      }
      status = "filled_sell";
    }
  }
  const double market_value = units * close * contract_size;
  const double unrealized_pnl = units > 0.0 ? (close - avg_cost) * units * contract_size : 0.0;
  return Rcpp::List::create(
    Rcpp::Named("cash") = cash,
    Rcpp::Named("units") = units,
    Rcpp::Named("avg_cost") = avg_cost,
    Rcpp::Named("last_price") = close,
    Rcpp::Named("market_value") = market_value,
    Rcpp::Named("equity") = cash + market_value,
    Rcpp::Named("unrealized_pnl") = unrealized_pnl,
    Rcpp::Named("realized_pnl") = realized_pnl,
    Rcpp::Named("fee") = fee,
    Rcpp::Named("dividend_cash") = dividend_cash,
    Rcpp::Named("units_before") = units_before,
    Rcpp::Named("filled") = filled,
    Rcpp::Named("status") = status
  );
}

// [[Rcpp::export]]
Rcpp::List account_variation_margin_rcpp(const Rcpp::DataFrame& margin_positions,
                                         const Rcpp::DataFrame& fx_rates,
                                         const std::string& base_currency) {
  Rcpp::IntegerVector asset_id = margin_positions["asset_id"];
  Rcpp::CharacterVector currency = margin_positions["currency"];
  Rcpp::NumericVector signed_units = margin_positions["signed_units"];
  Rcpp::NumericVector settlement_price = margin_positions["settlement_price"];
  Rcpp::NumericVector last_price = margin_positions["last_price"];
  Rcpp::NumericVector contract_size = margin_positions["contract_size"];
  Rcpp::NumericVector maintenance_rate = margin_positions["maintenance_rate"];
  Rcpp::CharacterVector fx_currency = fx_rates["currency"];
  Rcpp::NumericVector fx_rate = fx_rates["rate_to_base"];
  const R_xlen_t n = asset_id.size();
  Rcpp::NumericVector variation(n), variation_base(n), maintenance_base(n), next_settlement_price(n);
  double total_variation_base = 0.0;
  double total_maintenance_base = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    MarginPosition p;
    p.asset_id = asset_id[i];
    p.currency = Rcpp::as<std::string>(currency[i]);
    p.signed_units = signed_units[i];
    p.settlement_price = settlement_price[i];
    p.last_price = last_price[i];
    p.contract_size = contract_size[i];
    p.maintenance_rate = maintenance_rate[i];
    double rate = p.currency == base_currency ? 1.0 : NA_REAL;
    for (R_xlen_t j = 0; j < fx_currency.size(); ++j) {
      if (Rcpp::as<std::string>(fx_currency[j]) == p.currency) { rate = fx_rate[j]; break; }
    }
    if (!std::isfinite(rate) || rate <= 0.0) Rcpp::stop("Missing positive FX rate for a margin position currency.");
    variation[i] = variation_margin(p);
    variation_base[i] = variation[i] * rate;
    maintenance_base[i] = std::abs(margin_notional(p)) * p.maintenance_rate * rate;
    next_settlement_price[i] = p.last_price;
    total_variation_base += variation_base[i];
    total_maintenance_base += maintenance_base[i];
  }
  return Rcpp::List::create(
    Rcpp::Named("asset_id") = asset_id,
    Rcpp::Named("variation_margin") = variation,
    Rcpp::Named("variation_margin_base") = variation_base,
    Rcpp::Named("maintenance_margin_base") = maintenance_base,
    Rcpp::Named("next_settlement_price") = next_settlement_price,
    Rcpp::Named("total_variation_margin_base") = total_variation_base,
    Rcpp::Named("total_maintenance_margin_base") = total_maintenance_base
  );
}

// [[Rcpp::export]]
Rcpp::List heterogeneous_account_step_rcpp(const std::string& base_currency,
                                           const Rcpp::DataFrame& cash_balances,
                                           const Rcpp::DataFrame& inventory_positions,
                                           const Rcpp::DataFrame& margin_positions,
                                           const Rcpp::DataFrame& bars,
                                           const Rcpp::DataFrame& fx_rates,
                                           const Rcpp::DataFrame& settlements,
                                           const Rcpp::DataFrame& corporate_actions,
                                           const Rcpp::DataFrame& orders,
                                           double timestamp) {
  // Derivatives-only portfolio mode uses the native shared kernel below. The
  // typed heterogeneous batch carries the legacy action semantics until the
  // public portfolio API is retired.
  if (settlements.containsElementNamed("execution_mode") && settlements.nrows() > 0 &&
      Rcpp::as<std::string>(Rcpp::as<Rcpp::CharacterVector>(settlements["execution_mode"])[0]) == "derivatives_native") {
    auto setting = [&](const char* name, double fallback) {
      if (!settlements.containsElementNamed(name)) return fallback;
      Rcpp::NumericVector values = settlements[name];
      return values.size() ? values[0] : fallback;
    };
    auto setting_bool = [&](const char* name, bool fallback) {
      if (!settlements.containsElementNamed(name)) return fallback;
      Rcpp::LogicalVector values = settlements[name];
      return values.size() && values[0] != NA_LOGICAL ? values[0] == TRUE : fallback;
    };
    Rcpp::IntegerVector bar_asset = bars["asset_id"];
    Rcpp::NumericVector bar_timestamp = bars["timestamp"];
    Rcpp::NumericVector bar_open = bars["open"];
    Rcpp::NumericVector bar_high = bars["high"];
    Rcpp::NumericVector bar_low = bars["low"];
    Rcpp::NumericVector bar_close = bars["close"];
    Rcpp::NumericVector bar_step = bars.containsElementNamed("ctr_step") ? Rcpp::as<Rcpp::NumericVector>(bars["ctr_step"]) : Rcpp::NumericVector(bar_asset.size(), 1.0);
    Rcpp::NumericVector margin_size = margin_positions["contract_size"];
    Rcpp::IntegerVector margin_asset = margin_positions["asset_id"];
    Rcpp::CharacterVector order_id = orders.containsElementNamed("order_id") ? Rcpp::as<Rcpp::CharacterVector>(orders["order_id"]) : Rcpp::CharacterVector();
    Rcpp::IntegerVector order_asset = orders.containsElementNamed("asset_id") ? Rcpp::as<Rcpp::IntegerVector>(orders["asset_id"]) : Rcpp::IntegerVector();
    Rcpp::IntegerVector action = orders.containsElementNamed("action_code") ? Rcpp::as<Rcpp::IntegerVector>(orders["action_code"]) : Rcpp::IntegerVector(order_asset.size(), 0);
    Rcpp::IntegerVector dir = orders.containsElementNamed("dir_code") ? Rcpp::as<Rcpp::IntegerVector>(orders["dir_code"]) : Rcpp::IntegerVector(order_asset.size(), 0);
    Rcpp::IntegerVector order_type = orders.containsElementNamed("order_type_code") ? Rcpp::as<Rcpp::IntegerVector>(orders["order_type_code"]) : Rcpp::IntegerVector(order_asset.size(), 0);
    Rcpp::NumericVector qty = orders.containsElementNamed("qty") ? Rcpp::as<Rcpp::NumericVector>(orders["qty"]) : Rcpp::NumericVector(order_asset.size(), 0.0);
    Rcpp::NumericVector price = orders.containsElementNamed("execution_price") ? Rcpp::as<Rcpp::NumericVector>(orders["execution_price"]) : Rcpp::NumericVector(order_asset.size(), NA_REAL);
    Rcpp::IntegerVector strat_id = orders.containsElementNamed("strat_id") ? Rcpp::as<Rcpp::IntegerVector>(orders["strat_id"]) : Rcpp::IntegerVector(order_asset.size(), 0);
    Rcpp::IntegerVector action_id = orders.containsElementNamed("action_id") ? Rcpp::as<Rcpp::IntegerVector>(orders["action_id"]) : Rcpp::IntegerVector(order_asset.size(), 0);
    Rcpp::LogicalVector fee_aware = orders.containsElementNamed("target_derived") ? Rcpp::as<Rcpp::LogicalVector>(orders["target_derived"]) : Rcpp::LogicalVector(order_asset.size(), false);
    Rcpp::DataFrame legacy_orders = Rcpp::DataFrame::create(
      Rcpp::Named("order_id") = order_id, Rcpp::Named("asset_id") = order_asset,
      Rcpp::Named("action") = action, Rcpp::Named("dir") = dir,
      Rcpp::Named("order_type") = order_type, Rcpp::Named("ctr_qty") = qty,
      Rcpp::Named("price") = price, Rcpp::Named("strat_id") = strat_id,
      Rcpp::Named("action_id") = action_id, Rcpp::Named("fee_aware_target") = fee_aware
    );
    Rcpp::NumericMatrix covariance(bar_asset.size(), bar_asset.size());
    if (corporate_actions.containsElementNamed("asset_i") && corporate_actions.containsElementNamed("asset_j") && corporate_actions.containsElementNamed("covariance")) {
      Rcpp::IntegerVector asset_i = corporate_actions["asset_i"];
      Rcpp::IntegerVector asset_j = corporate_actions["asset_j"];
      Rcpp::NumericVector covariance_value = corporate_actions["covariance"];
      for (R_xlen_t k = 0; k < asset_i.size(); ++k) for (R_xlen_t i = 0; i < bar_asset.size(); ++i) for (R_xlen_t j = 0; j < bar_asset.size(); ++j) {
        if (asset_i[k] == bar_asset[i] && asset_j[k] == bar_asset[j]) covariance(i, j) = covariance_value[k];
      }
    }
    Rcpp::NumericVector bar_size(bar_asset.size(), 1.0);
    for (R_xlen_t i = 0; i < bar_asset.size(); ++i) for (R_xlen_t j = 0; j < margin_asset.size(); ++j) {
      if (margin_asset[j] == bar_asset[i]) { bar_size[i] = margin_size[j]; break; }
    }
    Rcpp::List result = typed_derivative_step_kernel(
      margin_positions,
      Rcpp::DataFrame::create(Rcpp::Named("asset_id") = bar_asset, Rcpp::Named("timestamp") = bar_timestamp, Rcpp::Named("open") = bar_open, Rcpp::Named("high") = bar_high, Rcpp::Named("low") = bar_low, Rcpp::Named("close") = bar_close),
      legacy_orders, covariance, setting("shared_cash", 0.0), bar_size, bar_step,
      setting("lev", 10.0), setting("fee_rt", 0.0), setting("maker_fee_rt", NA_REAL), setting("taker_fee_rt", NA_REAL), setting("fund_rt", 0.0), setting("funding_interval_hours", 8.0), setting("mmr", 0.02), setting("portfolio_margin_sigma", 3.0), setting("portfolio_margin_floor", 0.02), setting("old_timestamp", NA_REAL), setting("slippage", 0.0), setting("spread", 0.0), setting_bool("rec", true)
    );
    // Project the shared derivative kernel result into the heterogeneous
    // account contract. The legacy-shaped fields remain present for public
    // sim_portfolio_step() compatibility; exchange adapters consume these
    // typed balances and margin positions instead.
    Rcpp::List result_states = result["states"];
    Rcpp::IntegerVector output_asset(bar_asset.size());
    Rcpp::CharacterVector output_currency(bar_asset.size(), base_currency);
    Rcpp::NumericVector output_units(bar_asset.size());
    Rcpp::NumericVector output_settlement(bar_asset.size());
    Rcpp::NumericVector output_last(bar_asset.size());
    Rcpp::NumericVector output_size(bar_asset.size());
    Rcpp::NumericVector output_mmr(bar_asset.size(), setting("mmr", 0.02));
    for (R_xlen_t i = 0; i < bar_asset.size(); ++i) {
      Rcpp::List state = result_states[i];
      output_asset[i] = bar_asset[i];
      output_units[i] = Rcpp::as<int>(state["pos_dir"]) * Rcpp::as<double>(state["ctr_unit"]);
      output_settlement[i] = Rcpp::as<double>(state["avg_price"]);
      output_last[i] = Rcpp::as<double>(state["last_px"]);
      output_size[i] = bar_size[i];
    }
    // In the v2 exchange route variation margin is an account-kernel
    // settlement, not an R-side adjustment.  Keep the compatibility endpoint
    // unchanged unless the exchange explicitly requests this settlement.
    std::vector<std::string> account_event_type, account_event_currency;
    std::vector<int> account_event_asset;
    std::vector<double> account_event_amount, account_event_price;
    double settled_cash = Rcpp::as<double>(result["cash"]);
    if (setting_bool("settle_variation_margin", false)) {
      for (R_xlen_t i = 0; i < output_asset.size(); ++i) {
        if (!std::isfinite(output_units[i]) || std::abs(output_units[i]) <= 1e-12 ||
            !std::isfinite(output_settlement[i]) || !std::isfinite(output_last[i])) continue;
        const double variation = output_units[i] * (output_last[i] - output_settlement[i]) * output_size[i];
        settled_cash += variation;
        output_settlement[i] = output_last[i];
        if (std::abs(variation) > 1e-12) {
          account_event_type.push_back("variation_margin");
          account_event_currency.push_back(base_currency);
          account_event_asset.push_back(output_asset[i]);
          account_event_amount.push_back(variation);
          account_event_price.push_back(output_last[i]);
        }
      }
      result["cash"] = settled_cash;
    }
    result["cash_balances"] = Rcpp::DataFrame::create(
      Rcpp::Named("currency") = Rcpp::CharacterVector::create(base_currency),
      Rcpp::Named("settled") = Rcpp::NumericVector::create(settled_cash),
      Rcpp::Named("unsettled") = Rcpp::NumericVector::create(0.0)
    );
    result["margin_positions"] = Rcpp::DataFrame::create(
      Rcpp::Named("asset_id") = output_asset,
      Rcpp::Named("currency") = output_currency,
      Rcpp::Named("signed_units") = output_units,
      Rcpp::Named("settlement_price") = output_settlement,
      Rcpp::Named("last_price") = output_last,
      Rcpp::Named("contract_size") = output_size,
      Rcpp::Named("maintenance_rate") = output_mmr
    );
    Rcpp::List legacy_events = result["events"];
    Rcpp::IntegerVector event_action = legacy_events.containsElementNamed("action_id") ? Rcpp::as<Rcpp::IntegerVector>(legacy_events["action_id"]) : Rcpp::IntegerVector();
    Rcpp::IntegerVector event_asset_id = legacy_events.containsElementNamed("asset_id") ? Rcpp::as<Rcpp::IntegerVector>(legacy_events["asset_id"]) : Rcpp::IntegerVector();
    Rcpp::IntegerVector event_status = legacy_events.containsElementNamed("status") ? Rcpp::as<Rcpp::IntegerVector>(legacy_events["status"]) : Rcpp::IntegerVector();
    Rcpp::NumericVector event_time = legacy_events.containsElementNamed("timestamp") ? Rcpp::as<Rcpp::NumericVector>(legacy_events["timestamp"]) : Rcpp::NumericVector();
    Rcpp::NumericVector event_qty = legacy_events.containsElementNamed("ctr_qty") ? Rcpp::as<Rcpp::NumericVector>(legacy_events["ctr_qty"]) : Rcpp::NumericVector();
    Rcpp::NumericVector event_price = legacy_events.containsElementNamed("price") ? Rcpp::as<Rcpp::NumericVector>(legacy_events["price"]) : Rcpp::NumericVector();
    Rcpp::NumericVector event_fee = legacy_events.containsElementNamed("fee") ? Rcpp::as<Rcpp::NumericVector>(legacy_events["fee"]) : Rcpp::NumericVector();
    Rcpp::NumericVector event_realized = legacy_events.containsElementNamed("realized_pnl") ? Rcpp::as<Rcpp::NumericVector>(legacy_events["realized_pnl"]) : Rcpp::NumericVector();
    Rcpp::CharacterVector group_id = orders.containsElementNamed("atomic_group_id") ? Rcpp::as<Rcpp::CharacterVector>(orders["atomic_group_id"]) : order_id;
    std::vector<std::string> fill_id, fill_order_id, fill_group_id, fill_status, fill_reason;
    std::vector<int> fill_asset;
    std::vector<double> fill_timestamp, fill_quantity, fill_price, fill_fee, fill_realized;
    std::vector<bool> event_used(event_action.size(), false);
    for (R_xlen_t oi = 0; oi < order_asset.size(); ++oi) {
      R_xlen_t match = -1;
      for (R_xlen_t ei = 0; ei < event_action.size(); ++ei) {
        if (!event_used[ei] && event_action[ei] == action_id[oi] && event_asset_id[ei] == order_asset[oi]) { match = ei; event_used[ei] = true; break; }
      }
      const bool filled = match >= 0 && event_status[match] == static_cast<int>(TRADESIMR::ActionStatus::FILLED);
      const bool rejected = match >= 0 && !filled;
      fill_id.push_back("HDFILL" + std::to_string(oi + 1));
      fill_order_id.push_back(Rcpp::as<std::string>(order_id[oi]));
      fill_group_id.push_back(Rcpp::as<std::string>(group_id[oi]));
      fill_asset.push_back(order_asset[oi]);
      fill_status.push_back(filled ? "filled" : (rejected ? "rejected" : "pending"));
      fill_reason.push_back(filled ? "filled" : (rejected ? "execution_rejected" : "next_bar_not_eligible"));
      fill_timestamp.push_back(match >= 0 ? event_time[match] : timestamp);
      fill_quantity.push_back(match >= 0 ? event_qty[match] : 0.0);
      fill_price.push_back(match >= 0 ? event_price[match] : price[oi]);
      fill_fee.push_back(match >= 0 ? event_fee[match] : 0.0);
      fill_realized.push_back(match >= 0 ? event_realized[match] : 0.0);
    }
    std::vector<bool> fill_committed_bool;
    fill_committed_bool.reserve(fill_status.size());
    for (const auto& status : fill_status) fill_committed_bool.push_back(status == "filled");
    result["fills"] = Rcpp::DataFrame::create(
      Rcpp::Named("fill_id") = Rcpp::wrap(fill_id), Rcpp::Named("event_timestamp") = Rcpp::wrap(fill_timestamp),
      Rcpp::Named("order_id") = Rcpp::wrap(fill_order_id), Rcpp::Named("atomic_group_id") = Rcpp::wrap(fill_group_id),
      Rcpp::Named("asset_id") = Rcpp::wrap(fill_asset), Rcpp::Named("status") = Rcpp::wrap(fill_status),
      Rcpp::Named("reason_code") = Rcpp::wrap(fill_reason), Rcpp::Named("committed") = bool_vec_to_logical(fill_committed_bool),
      Rcpp::Named("qty") = Rcpp::wrap(fill_quantity), Rcpp::Named("price") = Rcpp::wrap(fill_price),
      Rcpp::Named("fee") = Rcpp::wrap(fill_fee), Rcpp::Named("realized_pnl") = Rcpp::wrap(fill_realized)
    );
    std::vector<std::string> group_ids, group_status, group_reason;
    std::vector<bool> group_committed;
    std::vector<double> group_timestamp;
    for (std::size_t i = 0; i < fill_group_id.size(); ++i) {
      const std::string& id = fill_group_id[i];
      if (std::find(group_ids.begin(), group_ids.end(), id) != group_ids.end()) continue;
      bool pending = false, rejected = false;
      std::string reason = "filled";
      for (std::size_t j = i; j < fill_group_id.size(); ++j) if (fill_group_id[j] == id) {
        if (fill_status[j] == "rejected") { rejected = true; reason = fill_reason[j]; }
        else if (fill_status[j] == "pending" && !rejected) { pending = true; reason = fill_reason[j]; }
      }
      group_ids.push_back(id);
      group_committed.push_back(!pending && !rejected);
      group_status.push_back(rejected ? "rejected" : (pending ? "pending" : "committed"));
      group_reason.push_back(reason);
      group_timestamp.push_back(fill_timestamp[i]);
    }
    result["groups"] = Rcpp::DataFrame::create(
      Rcpp::Named("atomic_group_id") = Rcpp::wrap(group_ids), Rcpp::Named("group_status") = Rcpp::wrap(group_status),
      Rcpp::Named("group_reason_code") = Rcpp::wrap(group_reason), Rcpp::Named("committed") = bool_vec_to_logical(group_committed),
      Rcpp::Named("event_timestamp") = Rcpp::wrap(group_timestamp), Rcpp::Named("equity") = Rcpp::NumericVector(group_ids.size(), Rcpp::as<double>(result["equity"])),
      Rcpp::Named("maintenance_margin") = Rcpp::NumericVector(group_ids.size(), Rcpp::as<double>(result["maintenance_margin"])),
      Rcpp::Named("liquidated") = Rcpp::LogicalVector(group_ids.size(), Rcpp::as<bool>(result["liquidated"]))
    );
    result["account_events"] = Rcpp::DataFrame::create(
      Rcpp::Named("timestamp") = Rcpp::NumericVector(account_event_amount.size(), timestamp),
      Rcpp::Named("event_type") = Rcpp::wrap(account_event_type),
      Rcpp::Named("asset_id") = Rcpp::wrap(account_event_asset),
      Rcpp::Named("currency") = Rcpp::wrap(account_event_currency),
      Rcpp::Named("amount") = Rcpp::wrap(account_event_amount),
      Rcpp::Named("settlement_price") = Rcpp::wrap(account_event_price),
      Rcpp::Named("cash_effect") = Rcpp::LogicalVector(account_event_amount.size(), true)
    );
    return result;
  }
  Rcpp::CharacterVector cash_ccy = Rcpp::clone(Rcpp::as<Rcpp::CharacterVector>(cash_balances["currency"]));
  Rcpp::NumericVector cash_settled = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(cash_balances["settled"]));
  Rcpp::NumericVector cash_unsettled = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(cash_balances["unsettled"]));
  const bool has_inventory = inventory_positions.containsElementNamed("asset_id");
  Rcpp::IntegerVector inventory_asset = has_inventory ? Rcpp::clone(Rcpp::as<Rcpp::IntegerVector>(inventory_positions["asset_id"])) : Rcpp::IntegerVector();
  Rcpp::CharacterVector inventory_ccy = has_inventory ? Rcpp::clone(Rcpp::as<Rcpp::CharacterVector>(inventory_positions["currency"])) : Rcpp::CharacterVector();
  Rcpp::NumericVector inventory_units = has_inventory ? Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(inventory_positions["units"])) : Rcpp::NumericVector();
  Rcpp::NumericVector inventory_cost = has_inventory ? Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(inventory_positions["average_cost"])) : Rcpp::NumericVector();
  Rcpp::NumericVector inventory_last = has_inventory ? Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(inventory_positions["last_price"])) : Rcpp::NumericVector();
  Rcpp::NumericVector inventory_size = has_inventory ? Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(inventory_positions["contract_size"])) : Rcpp::NumericVector();
  Rcpp::NumericVector inventory_accrued = has_inventory && inventory_positions.containsElementNamed("accrued_interest") ? Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(inventory_positions["accrued_interest"])) : Rcpp::NumericVector(inventory_asset.size(), 0.0);
  Rcpp::IntegerVector margin_asset = Rcpp::clone(Rcpp::as<Rcpp::IntegerVector>(margin_positions["asset_id"]));
  Rcpp::CharacterVector margin_ccy = Rcpp::clone(Rcpp::as<Rcpp::CharacterVector>(margin_positions["currency"]));
  Rcpp::NumericVector margin_units = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(margin_positions["signed_units"]));
  Rcpp::NumericVector margin_settle = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(margin_positions["settlement_price"]));
  Rcpp::NumericVector margin_last = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(margin_positions["last_price"]));
  Rcpp::NumericVector margin_size = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(margin_positions["contract_size"]));
  Rcpp::NumericVector margin_mmr = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(margin_positions["maintenance_rate"]));
  Rcpp::NumericVector margin_old_timestamp = margin_positions.containsElementNamed("old_timestamp") ? Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(margin_positions["old_timestamp"])) : Rcpp::NumericVector(margin_asset.size(), NA_REAL);
  Rcpp::IntegerVector bar_asset = bars["asset_id"];
  Rcpp::NumericVector bar_close = bars["close"];
  Rcpp::NumericVector bar_high = bars.containsElementNamed("high") ? bars["high"] : Rcpp::NumericVector(bar_asset.size(), NA_REAL);
  Rcpp::NumericVector bar_low = bars.containsElementNamed("low") ? bars["low"] : Rcpp::NumericVector(bar_asset.size(), NA_REAL);
  Rcpp::CharacterVector bar_profile = bars.containsElementNamed("instrument_profile") ? bars["instrument_profile"] : Rcpp::CharacterVector(bar_asset.size(), "future");
  Rcpp::CharacterVector fx_ccy = fx_rates["currency"];
  Rcpp::NumericVector fx_to_base = fx_rates["rate_to_base"];

  auto rate_for = [&](const std::string& ccy) {
    if (ccy == base_currency) return 1.0;
    for (R_xlen_t i = 0; i < fx_ccy.size(); ++i) if (Rcpp::as<std::string>(fx_ccy[i]) == ccy) return fx_to_base[i];
    Rcpp::stop("Missing positive FX rate for account currency.");
    return 0.0;
  };
  auto cash_index = [&](const std::string& ccy) {
    for (R_xlen_t i = 0; i < cash_ccy.size(); ++i) if (Rcpp::as<std::string>(cash_ccy[i]) == ccy) return i;
    return static_cast<R_xlen_t>(-1);
  };
  const bool mixed_portfolio_native = settlements.containsElementNamed("execution_mode") && settlements.nrows() > 0 &&
    Rcpp::as<std::string>(Rcpp::as<Rcpp::CharacterVector>(settlements["execution_mode"])[0]) == "mixed_portfolio_native";
  auto numeric_setting = [&](const char* name, double fallback) {
    if (!settlements.containsElementNamed(name)) return fallback;
    Rcpp::NumericVector values = settlements[name];
    return values.size() && std::isfinite(values[0]) ? values[0] : fallback;
  };
  const double mixed_lev = numeric_setting("lev", 10.0);
  const double mixed_sigma = numeric_setting("portfolio_margin_sigma", 3.0);
  const double mixed_floor = numeric_setting("portfolio_margin_floor", 0.02);
  Rcpp::IntegerVector covariance_i = corporate_actions.containsElementNamed("asset_i") ? Rcpp::as<Rcpp::IntegerVector>(corporate_actions["asset_i"]) : Rcpp::IntegerVector();
  Rcpp::IntegerVector covariance_j = corporate_actions.containsElementNamed("asset_j") ? Rcpp::as<Rcpp::IntegerVector>(corporate_actions["asset_j"]) : Rcpp::IntegerVector();
  Rcpp::NumericVector covariance_value = corporate_actions.containsElementNamed("covariance") ? Rcpp::as<Rcpp::NumericVector>(corporate_actions["covariance"]) : Rcpp::NumericVector();
  auto unified_equity = [&]() {
    double value = 0.0;
    for (R_xlen_t i = 0; i < cash_ccy.size(); ++i) value += (cash_settled[i] + cash_unsettled[i]) * rate_for(Rcpp::as<std::string>(cash_ccy[i]));
    for (R_xlen_t i = 0; i < inventory_asset.size(); ++i) value += (inventory_units[i] * inventory_last[i] * inventory_size[i] + inventory_accrued[i]) * rate_for(Rcpp::as<std::string>(inventory_ccy[i]));
    for (R_xlen_t i = 0; i < margin_asset.size(); ++i) {
      const double settle = std::isfinite(margin_settle[i]) ? margin_settle[i] : margin_last[i];
      value += margin_units[i] * (margin_last[i] - settle) * margin_size[i] * rate_for(Rcpp::as<std::string>(margin_ccy[i]));
    }
    return value;
  };
  auto unified_margin = [&]() {
    std::vector<int> assets;
    std::vector<double> exposures;
    auto add_exposure = [&](int asset, double exposure) {
      for (std::size_t i = 0; i < assets.size(); ++i) if (assets[i] == asset) { exposures[i] += exposure; return; }
      assets.push_back(asset); exposures.push_back(exposure);
    };
    for (R_xlen_t i = 0; i < inventory_asset.size(); ++i) add_exposure(inventory_asset[i], inventory_units[i] * inventory_last[i] * inventory_size[i] * rate_for(Rcpp::as<std::string>(inventory_ccy[i])));
    for (R_xlen_t i = 0; i < margin_asset.size(); ++i) add_exposure(margin_asset[i], margin_units[i] * margin_last[i] * margin_size[i] * rate_for(Rcpp::as<std::string>(margin_ccy[i])));
    double gross = 0.0, variance = 0.0;
    for (std::size_t i = 0; i < exposures.size(); ++i) {
      gross += std::abs(exposures[i]);
      for (std::size_t j = 0; j < exposures.size(); ++j) {
        double covariance = i == j ? 1.0 : 0.0;
        for (R_xlen_t k = 0; k < covariance_value.size(); ++k) {
          if (covariance_i[k] == assets[i] && covariance_j[k] == assets[j]) { covariance = covariance_value[k]; break; }
        }
        variance += exposures[i] * covariance * exposures[j];
      }
    }
    return std::max(mixed_sigma * std::sqrt(std::max(0.0, variance)), mixed_floor * gross);
  };
  std::vector<double> event_amount;
  std::vector<int> event_asset;
  std::vector<std::string> event_ccy;
  std::vector<double> event_settlement;
  std::vector<std::string> event_type_label;
  std::vector<int> event_cash_effect;
  std::vector<std::string> fill_order_id;
  std::vector<int> fill_asset_id;
  std::vector<std::string> fill_status;
  std::vector<std::string> fill_reason;
  std::vector<double> fill_qty;
  std::vector<double> fill_price;
  std::vector<double> fill_fee;
  std::vector<double> fill_realized;
  std::vector<std::string> fill_group_id;
  std::vector<int> fill_committed;
  std::vector<double> fill_resulting_qty;
  std::vector<double> fill_resulting_cash;

  // Admission is evaluated at the completed boundary.  Seed previously
  // unvalued inventory with that boundary close before testing a mixed group;
  // otherwise an initial spot leg produces NA equity and rejects the entire
  // atomic rebalance.
  if (mixed_portfolio_native) {
    for (R_xlen_t i = 0; i < inventory_asset.size(); ++i) {
      if (std::isfinite(inventory_last[i])) continue;
      for (R_xlen_t j = 0; j < bar_asset.size(); ++j) {
        if (bar_asset[j] == inventory_asset[i]) { inventory_last[i] = bar_close[j]; break; }
      }
    }
  }

  // Corporate actions share the durable input table with covariance rows for
  // backwards compatibility.  Rows with an action_type are independent of
  // covariance rows and are applied before order admission at this boundary.
  // The first vertical slice is deliberately explicit: R owns calendars while
  // C++ owns the accounting mutation and the typed event it produces.
  if (corporate_actions.containsElementNamed("action_type") &&
      corporate_actions.containsElementNamed("asset_id") &&
      corporate_actions.containsElementNamed("amount") &&
      corporate_actions.containsElementNamed("currency")) {
    Rcpp::CharacterVector action_type = corporate_actions["action_type"];
    Rcpp::IntegerVector action_asset = corporate_actions["asset_id"];
    Rcpp::NumericVector action_amount = corporate_actions["amount"];
    Rcpp::CharacterVector action_currency = corporate_actions["currency"];
    Rcpp::NumericVector action_timestamp = corporate_actions.containsElementNamed("effective_timestamp") ?
      Rcpp::as<Rcpp::NumericVector>(corporate_actions["effective_timestamp"]) :
      Rcpp::NumericVector(action_asset.size(), timestamp);
    for (R_xlen_t ai = 0; ai < action_asset.size(); ++ai) {
      if (!std::isfinite(action_amount[ai]) ||
          (std::isfinite(action_timestamp[ai]) && action_timestamp[ai] > timestamp)) continue;
      const std::string type = Rcpp::as<std::string>(action_type[ai]);
      if (type != "coupon" && type != "bond_accrual" && type != "redemption") continue;
      R_xlen_t pi = static_cast<R_xlen_t>(-1);
      for (R_xlen_t i = 0; i < inventory_asset.size(); ++i) {
        if (inventory_asset[i] == action_asset[ai]) { pi = i; break; }
      }
      if (pi == static_cast<R_xlen_t>(-1)) continue;
      const std::string ccy = Rcpp::as<std::string>(action_currency[ai]);
      const R_xlen_t ci = cash_index(ccy);
      if (ci == static_cast<R_xlen_t>(-1)) {
        Rcpp::stop("Every corporate-action currency requires a cash balance row.");
      }
      const double units = inventory_units[pi];
      double amount = units * action_amount[ai] * inventory_size[pi];
      if (type == "bond_accrual") {
        inventory_accrued[pi] += amount;
      } else {
        if (type == "redemption") amount += inventory_accrued[pi];
        cash_settled[ci] += amount;
        if (type == "redemption") {
          inventory_units[pi] = 0.0;
          inventory_cost[pi] = NA_REAL;
          inventory_accrued[pi] = 0.0;
        }
      }
      if (std::abs(amount) > 1e-12 || (type == "redemption" && std::abs(units) > 1e-12)) {
        event_amount.push_back(amount);
        event_asset.push_back(action_asset[ai]);
        event_ccy.push_back(ccy);
        event_settlement.push_back(std::isfinite(inventory_last[pi]) ? inventory_last[pi] : NA_REAL);
        event_type_label.push_back(type == "coupon" ? "bond_coupon" : type);
        event_cash_effect.push_back(type == "bond_accrual" ? 0 : 1);
      }
    }
  }

  // Calendar-driven bond schedules are passed as typed rows. They retain no
  // hidden C++ state: R persists the boundary cursors, while C++ performs the
  // day-count, coupon, and redemption accounting deterministically.
  if (corporate_actions.containsElementNamed("schedule_type") &&
      corporate_actions.containsElementNamed("asset_id") &&
      corporate_actions.containsElementNamed("coupon_rate") &&
      corporate_actions.containsElementNamed("coupon_frequency") &&
      corporate_actions.containsElementNamed("face_value") &&
      corporate_actions.containsElementNamed("currency")) {
    Rcpp::CharacterVector schedule_type = corporate_actions["schedule_type"];
    Rcpp::IntegerVector schedule_asset = corporate_actions["asset_id"];
    Rcpp::NumericVector coupon_rate = corporate_actions["coupon_rate"];
    Rcpp::NumericVector coupon_frequency = corporate_actions["coupon_frequency"];
    Rcpp::NumericVector face_value = corporate_actions["face_value"];
    Rcpp::CharacterVector schedule_currency = corporate_actions["currency"];
    Rcpp::NumericVector day_count = corporate_actions.containsElementNamed("accrual_day_count") ? Rcpp::as<Rcpp::NumericVector>(corporate_actions["accrual_day_count"]) : Rcpp::NumericVector(schedule_asset.size(), 365.0);
    Rcpp::NumericVector last_accrual = corporate_actions.containsElementNamed("last_accrual_timestamp") ? Rcpp::as<Rcpp::NumericVector>(corporate_actions["last_accrual_timestamp"]) : Rcpp::NumericVector(schedule_asset.size(), NA_REAL);
    Rcpp::NumericVector next_coupon = corporate_actions.containsElementNamed("next_coupon_timestamp") ? Rcpp::as<Rcpp::NumericVector>(corporate_actions["next_coupon_timestamp"]) : Rcpp::NumericVector(schedule_asset.size(), NA_REAL);
    Rcpp::NumericVector maturity = corporate_actions.containsElementNamed("maturity_timestamp") ? Rcpp::as<Rcpp::NumericVector>(corporate_actions["maturity_timestamp"]) : Rcpp::NumericVector(schedule_asset.size(), NA_REAL);
    for (R_xlen_t si = 0; si < schedule_asset.size(); ++si) {
      if (schedule_type[si] == NA_STRING) continue;
      if (Rcpp::as<std::string>(schedule_type[si]) != "bond") continue;
      if (!std::isfinite(coupon_rate[si]) || !std::isfinite(coupon_frequency[si]) || coupon_frequency[si] <= 0.0 ||
          !std::isfinite(face_value[si]) || face_value[si] <= 0.0) continue;
      R_xlen_t pi = static_cast<R_xlen_t>(-1);
      for (R_xlen_t i = 0; i < inventory_asset.size(); ++i) if (inventory_asset[i] == schedule_asset[si]) { pi = i; break; }
      if (pi == static_cast<R_xlen_t>(-1) || std::abs(inventory_units[pi]) <= 1e-12) continue;
      const std::string ccy = Rcpp::as<std::string>(schedule_currency[si]);
      const R_xlen_t ci = cash_index(ccy);
      if (ci == static_cast<R_xlen_t>(-1)) Rcpp::stop("Every bond-schedule currency requires a cash balance row.");
      const double cutoff = std::isfinite(maturity[si]) ? std::min(timestamp, maturity[si]) : timestamp;
      const double denominator = std::isfinite(day_count[si]) && day_count[si] > 0.0 ? day_count[si] : 365.0;
      const auto accrue_to = [&](const double boundary) {
        if (!std::isfinite(last_accrual[si]) || boundary <= last_accrual[si]) return;
        const double accrued = inventory_units[pi] * inventory_size[pi] * face_value[si] * coupon_rate[si] *
          ((boundary - last_accrual[si]) / (86400.0 * denominator));
        if (std::abs(accrued) > 1e-12) {
          inventory_accrued[pi] += accrued;
          event_amount.push_back(accrued); event_asset.push_back(schedule_asset[si]); event_ccy.push_back(ccy);
          event_settlement.push_back(std::isfinite(inventory_last[pi]) ? inventory_last[pi] : NA_REAL);
          event_type_label.push_back("bond_accrual"); event_cash_effect.push_back(0);
        }
        last_accrual[si] = boundary;
      };
      const double period = 365.0 * 86400.0 / coupon_frequency[si];
      double due = next_coupon[si];
      while (std::isfinite(due) && due <= cutoff + 1e-8) {
        accrue_to(due);
        const double coupon = inventory_units[pi] * inventory_size[pi] * face_value[si] * coupon_rate[si] / coupon_frequency[si];
        cash_settled[ci] += coupon;
        inventory_accrued[pi] = 0.0;
        event_amount.push_back(coupon); event_asset.push_back(schedule_asset[si]); event_ccy.push_back(ccy);
        event_settlement.push_back(std::isfinite(inventory_last[pi]) ? inventory_last[pi] : NA_REAL);
        event_type_label.push_back("bond_coupon"); event_cash_effect.push_back(1);
        due += period;
      }
      accrue_to(cutoff);
      if (std::isfinite(maturity[si]) && timestamp >= maturity[si] && std::abs(inventory_units[pi]) > 1e-12) {
        const double redemption = inventory_units[pi] * inventory_size[pi] * face_value[si] + inventory_accrued[pi];
        cash_settled[ci] += redemption;
        inventory_units[pi] = 0.0; inventory_cost[pi] = NA_REAL; inventory_accrued[pi] = 0.0;
        event_amount.push_back(redemption); event_asset.push_back(schedule_asset[si]); event_ccy.push_back(ccy);
        event_settlement.push_back(std::isfinite(inventory_last[pi]) ? inventory_last[pi] : NA_REAL);
        event_type_label.push_back("redemption"); event_cash_effect.push_back(1);
      }
    }
  }

  // Orders are normalized by R before crossing the C++ boundary. The kernel
  // owns profile-specific cash/inventory mutations and returns a typed outcome
  // for every supplied order; R owns durable order-id bookkeeping.
  if (orders.nrows() > 0) {
    const std::vector<std::string> required_order = {
      "order_id", "asset_id", "instrument_profile", "side", "qty",
      "execution_price", "fee_rt", "atomic_group_id", "order_type",
      "limit_price", "time_in_force"
    };
    for (const auto& name : required_order) {
      if (!orders.containsElementNamed(name.c_str())) Rcpp::stop("Normalized heterogeneous orders are missing required columns.");
    }
    Rcpp::CharacterVector order_id = orders["order_id"];
    Rcpp::IntegerVector order_asset = orders["asset_id"];
    Rcpp::CharacterVector order_profile = orders["instrument_profile"];
    Rcpp::CharacterVector order_side = orders["side"];
    Rcpp::NumericVector order_qty = orders["qty"];
    Rcpp::NumericVector order_px = orders["execution_price"];
    Rcpp::NumericVector order_fee_rt = orders["fee_rt"];
    Rcpp::CharacterVector order_group = orders["atomic_group_id"];
    Rcpp::CharacterVector order_type = orders["order_type"];
    Rcpp::NumericVector order_limit = orders["limit_price"];
    Rcpp::CharacterVector order_tif = orders["time_in_force"];
    Rcpp::IntegerVector order_action = orders.containsElementNamed("action_code") ? Rcpp::as<Rcpp::IntegerVector>(orders["action_code"]) : Rcpp::IntegerVector(order_asset.size(), 0);
    Rcpp::IntegerVector order_dir = orders.containsElementNamed("dir_code") ? Rcpp::as<Rcpp::IntegerVector>(orders["dir_code"]) : Rcpp::IntegerVector(order_asset.size(), 0);
    Rcpp::LogicalVector order_target_derived = orders.containsElementNamed("target_derived") ? Rcpp::as<Rcpp::LogicalVector>(orders["target_derived"]) : Rcpp::LogicalVector(order_asset.size(), false);
    Rcpp::NumericVector order_step = orders.containsElementNamed("ctr_step") ? Rcpp::as<Rcpp::NumericVector>(orders["ctr_step"]) : Rcpp::NumericVector(order_asset.size(), 1.0);
    std::string active_group;
    R_xlen_t group_fill_start = 0;
    Rcpp::NumericVector group_cash;
    Rcpp::NumericVector group_inventory_units;
    Rcpp::NumericVector group_inventory_cost;
    Rcpp::NumericVector group_inventory_last;
    Rcpp::NumericVector group_inventory_accrued;
    Rcpp::NumericVector group_margin_units;
    Rcpp::NumericVector group_margin_settle;
    Rcpp::NumericVector group_margin_last;
    bool group_rejected = false;
    for (R_xlen_t oi = 0; oi < order_asset.size(); ++oi) {
      const std::string group = Rcpp::as<std::string>(order_group[oi]);
      if (oi == 0 || group != active_group) {
        active_group = group;
        group_fill_start = fill_order_id.size();
        group_cash = Rcpp::clone(cash_settled);
        group_inventory_units = Rcpp::clone(inventory_units);
        group_inventory_cost = Rcpp::clone(inventory_cost);
        group_inventory_last = Rcpp::clone(inventory_last);
        group_inventory_accrued = Rcpp::clone(inventory_accrued);
        group_margin_units = Rcpp::clone(margin_units);
        group_margin_settle = Rcpp::clone(margin_settle);
        group_margin_last = Rcpp::clone(margin_last);
        group_rejected = false;
        // A group is a transaction boundary, not an execution ordering hint.
        // Check limit eligibility for every member before allowing the first
        // leg to mutate shared cash, inventory, or margin state.
        R_xlen_t group_end = oi;
        bool any_not_eligible = false;
        bool any_ioc = false;
        bool any_fok = false;
        for (; group_end < order_asset.size() && Rcpp::as<std::string>(order_group[group_end]) == group; ++group_end) {
          const std::string type0 = Rcpp::as<std::string>(order_type[group_end]);
          const std::string side0 = Rcpp::as<std::string>(order_side[group_end]);
          if (type0 != "limit" || side0 == "flat") continue;
          bool eligible0 = false;
          for (R_xlen_t bi = 0; bi < bar_asset.size(); ++bi) {
            if (bar_asset[bi] != order_asset[group_end]) continue;
            eligible0 = side0 == "buy" ? (std::isfinite(bar_low[bi]) && bar_low[bi] <= order_limit[group_end]) :
              (std::isfinite(bar_high[bi]) && bar_high[bi] >= order_limit[group_end]);
            break;
          }
          if (!eligible0) {
            any_not_eligible = true;
            const std::string tif0 = Rcpp::as<std::string>(order_tif[group_end]);
            any_ioc = any_ioc || tif0 == "ioc";
            any_fok = any_fok || tif0 == "fok";
          }
        }
        if (any_not_eligible) {
          const std::string status0 = any_fok ? "rejected" : (any_ioc ? "cancelled" : "pending");
          const std::string reason0 = any_fok ? "atomic_group_rejected" : "limit_not_eligible";
          for (R_xlen_t gj = oi; gj < group_end; ++gj) {
            fill_order_id.push_back(Rcpp::as<std::string>(order_id[gj]));
            fill_asset_id.push_back(order_asset[gj]); fill_status.push_back(status0); fill_reason.push_back(reason0);
            fill_qty.push_back(0.0); fill_price.push_back(order_limit[gj]); fill_fee.push_back(0.0); fill_realized.push_back(0.0);
            fill_group_id.push_back(group); fill_committed.push_back(0); fill_resulting_qty.push_back(0.0); fill_resulting_cash.push_back(NA_REAL);
          }
          oi = group_end - 1;
          continue;
        }
      }
      const std::string oid = Rcpp::as<std::string>(order_id[oi]);
      const std::string profile = Rcpp::as<std::string>(order_profile[oi]);
      const std::string side = Rcpp::as<std::string>(order_side[oi]);
      const double qty = order_qty[oi];
      const double px = order_px[oi];
      const double fee_rate = order_fee_rt[oi];
      const std::string type = Rcpp::as<std::string>(order_type[oi]);
      const std::string tif = Rcpp::as<std::string>(order_tif[oi]);
      std::string status = "rejected";
      std::string reason = "invalid_order";
      double fee = 0.0, realized = 0.0, executed_qty = 0.0;
      if (group_rejected) {
        fill_order_id.push_back(oid); fill_asset_id.push_back(order_asset[oi]);
        fill_status.push_back("rejected"); fill_reason.push_back("atomic_group_rejected");
        fill_qty.push_back(0.0); fill_price.push_back(px); fill_fee.push_back(0.0); fill_realized.push_back(0.0);
        fill_group_id.push_back(group); fill_committed.push_back(0); fill_resulting_qty.push_back(0.0); fill_resulting_cash.push_back(NA_REAL);
        continue;
      }
      if (type == "limit" && side != "flat") {
        bool eligible = false;
        for (R_xlen_t bi = 0; bi < bar_asset.size(); ++bi) {
          if (bar_asset[bi] != order_asset[oi]) continue;
          eligible = side == "buy" ? (std::isfinite(bar_low[bi]) && bar_low[bi] <= order_limit[oi]) :
            (std::isfinite(bar_high[bi]) && bar_high[bi] >= order_limit[oi]);
          break;
        }
        if (!eligible) {
          if (tif == "fok") {
            cash_settled = group_cash; inventory_units = group_inventory_units;
            inventory_cost = group_inventory_cost; inventory_last = group_inventory_last;
            inventory_accrued = group_inventory_accrued;
            margin_units = group_margin_units; margin_settle = group_margin_settle; margin_last = group_margin_last;
            for (R_xlen_t fi = group_fill_start; fi < static_cast<R_xlen_t>(fill_status.size()); ++fi) {
              fill_status[fi] = "rejected"; fill_reason[fi] = "atomic_group_rejected";
              fill_qty[fi] = 0.0; fill_fee[fi] = 0.0; fill_realized[fi] = 0.0;
              fill_committed[fi] = 0; fill_resulting_qty[fi] = 0.0; fill_resulting_cash[fi] = NA_REAL;
            }
            group_rejected = true;
            fill_order_id.push_back(oid); fill_asset_id.push_back(order_asset[oi]);
            fill_status.push_back("rejected"); fill_reason.push_back("atomic_group_rejected");
            fill_qty.push_back(0.0); fill_price.push_back(order_limit[oi]); fill_fee.push_back(0.0); fill_realized.push_back(0.0);
            fill_group_id.push_back(group); fill_committed.push_back(0); fill_resulting_qty.push_back(0.0); fill_resulting_cash.push_back(NA_REAL);
            continue;
          }
          fill_order_id.push_back(oid); fill_asset_id.push_back(order_asset[oi]);
          fill_status.push_back(tif == "ioc" ? "cancelled" : "pending");
          fill_reason.push_back("limit_not_eligible"); fill_qty.push_back(0.0);
          fill_price.push_back(order_limit[oi]); fill_fee.push_back(0.0); fill_realized.push_back(0.0);
          fill_group_id.push_back(group); fill_committed.push_back(0); fill_resulting_qty.push_back(0.0); fill_resulting_cash.push_back(NA_REAL);
          continue;
        }
      }
      if (std::isfinite(qty) && qty >= 0.0 && std::isfinite(px) && px > 0.0 && std::isfinite(fee_rate) && fee_rate >= 0.0 &&
          (side == "buy" || side == "sell" || side == "flat")) {
        if (profile == "equity" || profile == "etf" || profile == "crypto_spot" || profile == "fx_spot" || profile == "bond") {
          R_xlen_t pi = static_cast<R_xlen_t>(-1);
          for (R_xlen_t i = 0; i < inventory_asset.size(); ++i) if (inventory_asset[i] == order_asset[oi]) { pi = i; break; }
          if (pi == static_cast<R_xlen_t>(-1)) {
            reason = "missing_inventory_position";
          } else {
            const double signed_qty = side == "buy" ? qty : (side == "sell" ? -qty : -inventory_units[pi]);
            const double units = std::abs(signed_qty);
            const double notional = units * px * inventory_size[pi];
            fee = notional * fee_rate;
            const R_xlen_t ci = cash_index(Rcpp::as<std::string>(inventory_ccy[pi]));
            if (ci == static_cast<R_xlen_t>(-1)) {
              reason = "missing_cash_balance";
            } else if (signed_qty > 0.0 && cash_settled[ci] + 1e-10 < notional + fee) {
              reason = "insufficient_cash";
            } else if (signed_qty < 0.0 && inventory_units[pi] + 1e-10 < units) {
              reason = "insufficient_inventory";
            } else if (std::abs(signed_qty) < 1e-12) {
              status = "no_op"; reason = "no_position_change"; fee = 0.0;
            } else if (signed_qty > 0.0) {
              const double prior_cost = inventory_units[pi] * (std::isfinite(inventory_cost[pi]) ? inventory_cost[pi] : 0.0);
              cash_settled[ci] -= notional + fee;
              inventory_units[pi] += units;
              inventory_cost[pi] = (prior_cost + units * px) / inventory_units[pi];
              status = "filled"; reason = "filled"; executed_qty = units;
            } else {
              realized = (px - inventory_cost[pi]) * units * inventory_size[pi] - fee;
              cash_settled[ci] += notional - fee;
              inventory_units[pi] -= units;
              if (inventory_units[pi] <= 1e-12) { inventory_units[pi] = 0.0; inventory_cost[pi] = NA_REAL; inventory_accrued[pi] = 0.0; }
              status = "filled"; reason = "filled"; executed_qty = units;
            }
          }
        } else if (profile == "future" || profile == "crypto_perp") {
          R_xlen_t pi = static_cast<R_xlen_t>(-1);
          for (R_xlen_t i = 0; i < margin_asset.size(); ++i) if (margin_asset[i] == order_asset[oi]) { pi = i; break; }
          if (pi == static_cast<R_xlen_t>(-1)) {
            reason = "missing_margin_position";
          } else {
            const double prior_units = margin_units[pi];
            double target_units = side == "buy" ? prior_units + qty : (side == "sell" ? prior_units - qty : 0.0);
            // Target-weight orders carry the authoritative action plan from R.
            // Explicit contract orders retain their historical signed-delta path.
            if (mixed_portfolio_native && order_action[oi] != 0) {
              const int action = order_action[oi];
              const int direction = order_dir[oi] > 0 ? 1 : (order_dir[oi] < 0 ? -1 : 0);
              if (action == 1) target_units = direction * qty;
              else if (action == 2) target_units = prior_units + direction * qty;
              else if (action == -2) target_units = prior_units - (prior_units >= 0.0 ? 1.0 : -1.0) * qty;
              else if (action == -1) target_units = 0.0;
            }
            double signed_qty = target_units - prior_units;
            double units = std::abs(signed_qty);
            const double notional = units * px * margin_size[pi];
            fee = notional * fee_rate;
            const R_xlen_t ci = cash_index(Rcpp::as<std::string>(margin_ccy[pi]));
            if (ci == static_cast<R_xlen_t>(-1)) {
              reason = "missing_cash_balance";
            } else if (cash_settled[ci] + 1e-10 < fee) {
              reason = "insufficient_cash_for_fee";
            } else if (std::abs(signed_qty) < 1e-12) {
              status = "no_op"; reason = "no_position_change"; fee = 0.0;
            } else {
              auto feasible = [&](double delta) {
                if (!mixed_portfolio_native) return true;
                const double trial_fee = std::abs(delta) * px * margin_size[pi] * fee_rate;
                if (cash_settled[ci] + 1e-10 < trial_fee) return false;
                const double saved_cash = cash_settled[ci];
                const double saved_units = margin_units[pi];
                cash_settled[ci] -= trial_fee;
                margin_units[pi] += delta;
                const bool accepted = unified_equity() + 1e-10 >= unified_margin() &&
                  std::abs(margin_units[pi] * px * margin_size[pi]) <= (unified_equity() * mixed_lev + 1e-10);
                cash_settled[ci] = saved_cash;
                margin_units[pi] = saved_units;
                return accepted;
              };
              if (mixed_portfolio_native && !feasible(signed_qty) && order_target_derived[oi] == TRUE && std::abs(order_step[oi]) > 0.0) {
                const double step = std::abs(order_step[oi]);
                const long long max_steps = static_cast<long long>(std::floor(std::abs(signed_qty) / step + 1e-10));
                long long low = 0, high = max_steps, best = 0;
                while (low <= high) {
                  const long long mid = low + (high - low) / 2;
                  const double trial = (signed_qty >= 0.0 ? 1.0 : -1.0) * static_cast<double>(mid) * step;
                  if (feasible(trial)) { best = mid; low = mid + 1; } else high = mid - 1;
                }
                signed_qty = (signed_qty >= 0.0 ? 1.0 : -1.0) * static_cast<double>(best) * step;
                units = std::abs(signed_qty);
                fee = units * px * margin_size[pi] * fee_rate;
                if (best == 0) reason = "insufficient_portfolio_margin";
              }
              if (units > 1e-12 && feasible(signed_qty)) {
                cash_settled[ci] -= fee;
                margin_units[pi] += signed_qty;
                margin_last[pi] = px;
                if (!std::isfinite(margin_settle[pi])) margin_settle[pi] = px;
                status = "filled"; reason = (std::abs(signed_qty - (target_units - prior_units)) > 1e-12) ? "margin_clipped" : "filled";
                executed_qty = units;
              } else if (units <= 1e-12 && std::abs(target_units - prior_units) > 1e-12) {
                reason = "insufficient_portfolio_margin";
              }
            }
          }
        } else {
          reason = "unsupported_instrument_profile";
        }
      }
      fill_order_id.push_back(oid); fill_asset_id.push_back(order_asset[oi]);
      fill_status.push_back(status); fill_reason.push_back(reason); fill_qty.push_back(executed_qty);
      fill_price.push_back(px); fill_fee.push_back(fee); fill_realized.push_back(realized);
      fill_group_id.push_back(group); fill_committed.push_back(status == "filled" || status == "no_op" ? 1 : 0);
      double resulting_qty = 0.0, resulting_cash = NA_REAL;
      for (R_xlen_t i = 0; i < inventory_asset.size(); ++i) if (inventory_asset[i] == order_asset[oi]) {
        resulting_qty = inventory_units[i]; const R_xlen_t ci = cash_index(Rcpp::as<std::string>(inventory_ccy[i])); if (ci >= 0) resulting_cash = cash_settled[ci]; break;
      }
      for (R_xlen_t i = 0; i < margin_asset.size(); ++i) if (margin_asset[i] == order_asset[oi]) {
        resulting_qty = margin_units[i]; const R_xlen_t ci = cash_index(Rcpp::as<std::string>(margin_ccy[i])); if (ci >= 0) resulting_cash = cash_settled[ci]; break;
      }
      fill_resulting_qty.push_back(resulting_qty); fill_resulting_cash.push_back(resulting_cash);
      if (status == "rejected") {
        // No member of an atomic group is factual execution unless every leg
        // preflights successfully. Restore the group boundary and publish
        // terminal rollback outcomes for all provisional legs.
        cash_settled = group_cash;
        inventory_units = group_inventory_units;
        inventory_cost = group_inventory_cost;
        inventory_last = group_inventory_last;
        inventory_accrued = group_inventory_accrued;
        margin_units = group_margin_units;
        margin_settle = group_margin_settle;
        margin_last = group_margin_last;
        for (R_xlen_t fi = group_fill_start; fi < static_cast<R_xlen_t>(fill_status.size()); ++fi) {
          fill_status[fi] = "rejected";
          fill_reason[fi] = "atomic_group_rejected";
          fill_qty[fi] = 0.0;
          fill_fee[fi] = 0.0;
          fill_realized[fi] = 0.0;
          fill_committed[fi] = 0;
          fill_resulting_qty[fi] = 0.0;
          fill_resulting_cash[fi] = NA_REAL;
        }
        group_rejected = true;
      }
    }
  }
  // Mark inventory before valuing the unified account. Inventory execution and
  // corporate actions remain explicit inputs to a later kernel iteration.
  for (R_xlen_t i = 0; i < inventory_asset.size(); ++i) {
    for (R_xlen_t j = 0; j < bar_asset.size(); ++j) {
      if (bar_asset[j] == inventory_asset[i]) {
        inventory_last[i] = bar_close[j];
        break;
      }
    }
  }
  for (R_xlen_t i = 0; i < margin_asset.size(); ++i) {
    for (R_xlen_t j = 0; j < bar_asset.size(); ++j) {
      if (bar_asset[j] != margin_asset[i]) continue;
      const std::string profile = Rcpp::as<std::string>(bar_profile[j]);
      if (profile != "future" && profile != "crypto_perp") continue;
      if (mixed_portfolio_native && std::isfinite(margin_old_timestamp[i])) {
        const double funding_rate = numeric_setting("fund_rt", 0.0);
        const double interval_hours = numeric_setting("funding_interval_hours", 8.0);
        const double dt = timestamp - margin_old_timestamp[i];
        const double funding = funding_rate * dt * std::abs(margin_units[i] * margin_last[i] * margin_size[i]) /
          (60.0 * 60.0 * interval_hours);
        if (std::abs(funding) > 1e-12) {
          const R_xlen_t ci = cash_index(Rcpp::as<std::string>(margin_ccy[i]));
          if (ci < 0) Rcpp::stop("Every margin currency requires a cash balance row.");
          cash_settled[ci] -= funding;
          event_amount.push_back(-funding); event_asset.push_back(margin_asset[i]);
          event_ccy.push_back(Rcpp::as<std::string>(margin_ccy[i])); event_settlement.push_back(margin_last[i]);
          event_type_label.push_back("funding");
          event_cash_effect.push_back(1);
        }
      }
      margin_last[i] = bar_close[j];
      MarginPosition position;
      position.asset_id = margin_asset[i]; position.currency = Rcpp::as<std::string>(margin_ccy[i]);
      position.signed_units = margin_units[i]; position.settlement_price = margin_settle[i];
      position.last_price = margin_last[i]; position.contract_size = margin_size[i]; position.maintenance_rate = margin_mmr[i];
      const double vm = variation_margin(position);
      const R_xlen_t ci = cash_index(position.currency);
      if (ci < 0) Rcpp::stop("Every margin currency requires a cash balance row.");
      cash_settled[ci] += vm;
      margin_settle[i] = position.last_price;
      if (std::abs(vm) > 1e-12) {
        event_amount.push_back(vm); event_asset.push_back(position.asset_id);
        event_ccy.push_back(position.currency); event_settlement.push_back(position.last_price);
        event_type_label.push_back("variation_margin");
        event_cash_effect.push_back(1);
      }
      break;
    }
  }
  double equity = 0.0, maintenance = 0.0;
  for (R_xlen_t i = 0; i < cash_ccy.size(); ++i) equity += (cash_settled[i] + cash_unsettled[i]) * rate_for(Rcpp::as<std::string>(cash_ccy[i]));
  for (R_xlen_t i = 0; i < inventory_asset.size(); ++i) {
    InventoryPosition position;
    position.asset_id = inventory_asset[i]; position.currency = Rcpp::as<std::string>(inventory_ccy[i]);
    position.units = inventory_units[i]; position.average_cost = inventory_cost[i];
    position.last_price = inventory_last[i]; position.contract_size = inventory_size[i];
    position.accrued_interest = inventory_accrued[i];
    equity += inventory_value(position) * rate_for(position.currency);
  }
  for (R_xlen_t i = 0; i < margin_asset.size(); ++i) {
    maintenance += std::abs(margin_units[i] * margin_last[i] * margin_size[i]) * margin_mmr[i] * rate_for(Rcpp::as<std::string>(margin_ccy[i]));
  }
  const bool liquidated = !std::isfinite(equity) || equity < maintenance;
  Rcpp::DataFrame cash_out = Rcpp::DataFrame::create(Rcpp::Named("currency") = cash_ccy, Rcpp::Named("settled") = cash_settled, Rcpp::Named("unsettled") = cash_unsettled);
  Rcpp::DataFrame inventory_out = Rcpp::DataFrame::create(Rcpp::Named("asset_id") = inventory_asset, Rcpp::Named("currency") = inventory_ccy, Rcpp::Named("units") = inventory_units, Rcpp::Named("average_cost") = inventory_cost, Rcpp::Named("last_price") = inventory_last, Rcpp::Named("contract_size") = inventory_size, Rcpp::Named("accrued_interest") = inventory_accrued);
  Rcpp::DataFrame margin_out = Rcpp::DataFrame::create(Rcpp::Named("asset_id") = margin_asset, Rcpp::Named("currency") = margin_ccy, Rcpp::Named("signed_units") = margin_units, Rcpp::Named("settlement_price") = margin_settle, Rcpp::Named("last_price") = margin_last, Rcpp::Named("contract_size") = margin_size, Rcpp::Named("maintenance_rate") = margin_mmr);
  Rcpp::LogicalVector event_cash_effect_out(event_cash_effect.size());
  for (R_xlen_t i = 0; i < event_cash_effect_out.size(); ++i) event_cash_effect_out[i] = event_cash_effect[i] == 1;
  Rcpp::DataFrame events = Rcpp::DataFrame::create(Rcpp::Named("timestamp") = Rcpp::NumericVector(event_amount.size(), timestamp), Rcpp::Named("event_type") = Rcpp::wrap(event_type_label), Rcpp::Named("asset_id") = Rcpp::wrap(event_asset), Rcpp::Named("currency") = Rcpp::wrap(event_ccy), Rcpp::Named("amount") = Rcpp::wrap(event_amount), Rcpp::Named("settlement_price") = Rcpp::wrap(event_settlement), Rcpp::Named("cash_effect") = event_cash_effect_out);
  std::vector<std::string> fill_id; for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(fill_order_id.size()); ++i) fill_id.push_back("HFILL" + std::to_string(i + 1));
  Rcpp::DataFrame fills = Rcpp::DataFrame::create(Rcpp::Named("fill_id") = Rcpp::wrap(fill_id), Rcpp::Named("event_timestamp") = Rcpp::NumericVector(fill_order_id.size(), timestamp), Rcpp::Named("order_id") = Rcpp::wrap(fill_order_id), Rcpp::Named("atomic_group_id") = Rcpp::wrap(fill_group_id), Rcpp::Named("asset_id") = Rcpp::wrap(fill_asset_id), Rcpp::Named("status") = Rcpp::wrap(fill_status), Rcpp::Named("reason_code") = Rcpp::wrap(fill_reason), Rcpp::Named("committed") = Rcpp::wrap(fill_committed), Rcpp::Named("qty") = Rcpp::wrap(fill_qty), Rcpp::Named("price") = Rcpp::wrap(fill_price), Rcpp::Named("fee") = Rcpp::wrap(fill_fee), Rcpp::Named("realized_pnl") = Rcpp::wrap(fill_realized), Rcpp::Named("resulting_signed_quantity") = Rcpp::wrap(fill_resulting_qty), Rcpp::Named("resulting_currency_cash") = Rcpp::wrap(fill_resulting_cash));
  std::vector<std::string> group_ids, group_status, group_reason;
  std::vector<int> group_committed;
  for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(fill_group_id.size()); ++i) {
    if (i > 0 && fill_group_id[i] == fill_group_id[i - 1]) continue;
    bool committed = true, pending = false;
    std::string reason = "filled";
    for (R_xlen_t j = i; j < static_cast<R_xlen_t>(fill_group_id.size()) && fill_group_id[j] == fill_group_id[i]; ++j) {
      if (fill_status[j] == "pending") { committed = false; pending = true; reason = fill_reason[j]; }
      if (fill_status[j] == "cancelled" || fill_status[j] == "rejected") { committed = false; pending = false; reason = fill_reason[j]; }
    }
    group_ids.push_back(fill_group_id[i]); group_committed.push_back(committed ? 1 : 0);
    group_status.push_back(committed ? "committed" : (pending ? "pending" : "rejected")); group_reason.push_back(reason);
  }
  Rcpp::LogicalVector group_committed_out(group_committed.size());
  for (R_xlen_t i = 0; i < group_committed_out.size(); ++i) group_committed_out[i] = group_committed[i] == 1;
  Rcpp::DataFrame groups = Rcpp::DataFrame::create(Rcpp::Named("atomic_group_id") = Rcpp::wrap(group_ids), Rcpp::Named("group_status") = Rcpp::wrap(group_status), Rcpp::Named("group_reason_code") = Rcpp::wrap(group_reason), Rcpp::Named("committed") = group_committed_out, Rcpp::Named("event_timestamp") = Rcpp::NumericVector(group_ids.size(), timestamp), Rcpp::Named("equity") = Rcpp::NumericVector(group_ids.size(), equity), Rcpp::Named("maintenance_margin") = Rcpp::NumericVector(group_ids.size(), maintenance), Rcpp::Named("liquidated") = Rcpp::LogicalVector(group_ids.size(), liquidated));
  return Rcpp::List::create(Rcpp::Named("cash_balances") = cash_out, Rcpp::Named("inventory_positions") = inventory_out, Rcpp::Named("margin_positions") = margin_out, Rcpp::Named("equity") = equity, Rcpp::Named("maintenance_margin") = maintenance, Rcpp::Named("liquidated") = liquidated, Rcpp::Named("events") = events, Rcpp::Named("fills") = fills, Rcpp::Named("groups") = groups);
}

// [[Rcpp::export]]
Rcpp::List heterogeneous_order_preflight_rcpp(const std::string& base_currency,
                                               const Rcpp::DataFrame& cash_balances,
                                               const Rcpp::DataFrame& inventory_positions,
                                               const Rcpp::DataFrame& margin_positions,
                                               const Rcpp::DataFrame& bars,
                                               const Rcpp::DataFrame& fx_rates,
                                               const Rcpp::DataFrame& orders,
                                               double timestamp) {
  // The account step clones every mutated vector. This endpoint therefore
  // returns a proposal and never mutates the caller's R data frames.
  return heterogeneous_account_step_rcpp(
    base_currency, cash_balances, inventory_positions, margin_positions,
    bars, fx_rates, Rcpp::DataFrame::create(), Rcpp::DataFrame::create(),
    orders, timestamp
  );
}
