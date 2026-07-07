// rcpp_backtest.cpp
#include <Rcpp.h>
#include <algorithm>

#include "base_ids.h"
#include "ker_backtest.h"

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
  return Rcpp::List::create(
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
    Rcpp::Named("last_px") = recorder.last_px_vec,
    Rcpp::Named("notional") = recorder.notional_vec,
    Rcpp::Named("abs_notional") = recorder.abs_notional_vec,
    Rcpp::Named("unrealized_pnl") = recorder.unrealized_pnl_vec,
    Rcpp::Named("realized_pnl") = recorder.realized_pnl_vec,
    Rcpp::Named("fee") = recorder.fee_vec,
    Rcpp::Named("funding_fee") = recorder.funding_fee_vec,
    Rcpp::Named("maintenance_margin") = recorder.maintenance_margin_vec
  );
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
                         int strat = StratID::UNKNOWN,
                         int asset = AssetID::UNKNOWN,
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
                     int asset = AssetID::UNKNOWN,
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
      action_id.size() != n) {
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
      if (a.action == TRADESIMR::ActionCode::NONE || a.ctr_qty <= 0.0 || Rcpp::NumericVector::is_na(a.ctr_qty)) {
        continue;
      }
      if (a.type == TRADESIMR::OrderType::LIMIT && !x.limit_order_filled(a.px)) {
        continue;
      }
      double base_price = a.type == TRADESIMR::OrderType::LIMIT ? a.px : x.open;
      TradeState priced_state = s;
      priced_state.fee_rt = fee_rate_for(a);
      ExchangeMessage_on_trade trade_msg = x.update_on_trade(
        priced_state,
        a,
        a.type == TRADESIMR::OrderType::LIMIT ? TRADESIMR::BarStage::INTRA : TRADESIMR::BarStage::OPEN,
        fill_price_with_costs(a, base_price)
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
