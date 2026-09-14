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

  Rcpp::CharacterVector state_names = states.names();
  for (R_xlen_t i = 0; i < n_assets; ++i) {
    const int asset = bar_asset[i];
    asset_ids.push_back(asset);
    Rcpp::List state_i;
    const std::string key = std::to_string(asset);
    bool found = false;
    for (R_xlen_t j = 0; j < states.size(); ++j) {
      if (state_names.size() > j && Rcpp::as<std::string>(state_names[j]) == key) {
        state_i = Rcpp::as<Rcpp::List>(states[j]);
        found = true;
        break;
      }
    }
    if (!found) state_i = Rcpp::List::create();
    TradeState s = list_to_trade_state(
      state_i,
      asset,
      close[i],
      shared_cash,
      ctr_size[i],
      ctr_step[i],
      lev,
      fee_rt,
      fund_rt,
      funding_interval_hours,
      mmr
    );
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
  Rcpp::CharacterVector cash_ccy = cash_balances["currency"];
  Rcpp::NumericVector cash_settled = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(cash_balances["settled"]));
  Rcpp::NumericVector cash_unsettled = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(cash_balances["unsettled"]));
  const bool has_inventory = inventory_positions.containsElementNamed("asset_id");
  Rcpp::IntegerVector inventory_asset = has_inventory ? Rcpp::as<Rcpp::IntegerVector>(inventory_positions["asset_id"]) : Rcpp::IntegerVector();
  Rcpp::CharacterVector inventory_ccy = has_inventory ? Rcpp::as<Rcpp::CharacterVector>(inventory_positions["currency"]) : Rcpp::CharacterVector();
  Rcpp::NumericVector inventory_units = has_inventory ? Rcpp::as<Rcpp::NumericVector>(inventory_positions["units"]) : Rcpp::NumericVector();
  Rcpp::NumericVector inventory_cost = has_inventory ? Rcpp::as<Rcpp::NumericVector>(inventory_positions["average_cost"]) : Rcpp::NumericVector();
  Rcpp::NumericVector inventory_last = has_inventory ? Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(inventory_positions["last_price"])) : Rcpp::NumericVector();
  Rcpp::NumericVector inventory_size = has_inventory ? Rcpp::as<Rcpp::NumericVector>(inventory_positions["contract_size"]) : Rcpp::NumericVector();
  Rcpp::IntegerVector margin_asset = margin_positions["asset_id"];
  Rcpp::CharacterVector margin_ccy = margin_positions["currency"];
  Rcpp::NumericVector margin_units = margin_positions["signed_units"];
  Rcpp::NumericVector margin_settle = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(margin_positions["settlement_price"]));
  Rcpp::NumericVector margin_last = Rcpp::clone(Rcpp::as<Rcpp::NumericVector>(margin_positions["last_price"]));
  Rcpp::NumericVector margin_size = margin_positions["contract_size"];
  Rcpp::NumericVector margin_mmr = margin_positions["maintenance_rate"];
  Rcpp::IntegerVector bar_asset = bars["asset_id"];
  Rcpp::NumericVector bar_close = bars["close"];
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
  std::vector<double> event_amount;
  std::vector<int> event_asset;
  std::vector<std::string> event_ccy;
  std::vector<double> event_settlement;
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
    equity += inventory_value(position) * rate_for(position.currency);
  }
  for (R_xlen_t i = 0; i < margin_asset.size(); ++i) {
    maintenance += std::abs(margin_units[i] * margin_last[i] * margin_size[i]) * margin_mmr[i] * rate_for(Rcpp::as<std::string>(margin_ccy[i]));
  }
  const bool liquidated = !std::isfinite(equity) || equity < maintenance;
  Rcpp::DataFrame cash_out = Rcpp::DataFrame::create(Rcpp::Named("currency") = cash_ccy, Rcpp::Named("settled") = cash_settled, Rcpp::Named("unsettled") = cash_unsettled);
  Rcpp::DataFrame inventory_out = Rcpp::DataFrame::create(Rcpp::Named("asset_id") = inventory_asset, Rcpp::Named("currency") = inventory_ccy, Rcpp::Named("units") = inventory_units, Rcpp::Named("average_cost") = inventory_cost, Rcpp::Named("last_price") = inventory_last, Rcpp::Named("contract_size") = inventory_size);
  Rcpp::DataFrame margin_out = Rcpp::DataFrame::create(Rcpp::Named("asset_id") = margin_asset, Rcpp::Named("currency") = margin_ccy, Rcpp::Named("signed_units") = margin_units, Rcpp::Named("settlement_price") = margin_settle, Rcpp::Named("last_price") = margin_last, Rcpp::Named("contract_size") = margin_size, Rcpp::Named("maintenance_rate") = margin_mmr);
  Rcpp::DataFrame events = Rcpp::DataFrame::create(Rcpp::Named("timestamp") = Rcpp::NumericVector(event_amount.size(), timestamp), Rcpp::Named("event_type") = Rcpp::CharacterVector(event_amount.size(), "variation_margin"), Rcpp::Named("asset_id") = Rcpp::wrap(event_asset), Rcpp::Named("currency") = Rcpp::wrap(event_ccy), Rcpp::Named("amount") = Rcpp::wrap(event_amount), Rcpp::Named("settlement_price") = Rcpp::wrap(event_settlement));
  return Rcpp::List::create(Rcpp::Named("cash_balances") = cash_out, Rcpp::Named("inventory_positions") = inventory_out, Rcpp::Named("margin_positions") = margin_out, Rcpp::Named("equity") = equity, Rcpp::Named("maintenance_margin") = maintenance, Rcpp::Named("liquidated") = liquidated, Rcpp::Named("events") = events);
}
