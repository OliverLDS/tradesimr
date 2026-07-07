// rcpp_backtest.cpp
#include <Rcpp.h>

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

// [[Rcpp::export]]
Rcpp::List backtest_rcpp(const Rcpp::NumericVector& timestamp,
                         const Rcpp::NumericVector& open,
                         const Rcpp::NumericVector& high,
                         const Rcpp::NumericVector& low,
                         const Rcpp::NumericVector& close,
                         const Rcpp::NumericVector& tgt_pos,
                         const Rcpp::IntegerVector& pos_strat,
                         const Rcpp::NumericVector& tol_pos,
                         int strat = StratID::UNKNOWN,
                         int asset = AssetID::UNKNOWN,
                         double init_cash = 10000.0,
                         double ctr_size = 1.0,
                         double ctr_step = 1.0,
                         double lev = 10.0,
                         double fee_rt = 0.0,
                         double fund_rt = 0.0,
                         double mmr = 0.02,
                         bool rec = false) {
  R_xlen_t n = timestamp.size();

  if (open.size() != n ||
      high.size() != n ||
      low.size() != n ||
      close.size() != n ||
      tgt_pos.size() != n ||
      pos_strat.size() != n ||
      tol_pos.size() != n) {
    Rcpp::stop("All input vectors must have the same length.");
  }

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
    strat,
    asset,
    init_cash,
    ctr_size,
    ctr_step,
    lev,
    fee_rt,
    fund_rt,
    mmr,
    static_cast<std::size_t>(n),
    rec,
    recorder_ptr
  );

  if (rec && recorder_ptr != nullptr) {
    rec_out = Rcpp::List::create(
      Rcpp::Named("timestamp") = recorder_ptr->ts,
      Rcpp::Named("event_id") = recorder_ptr->event_id_vec,
      Rcpp::Named("event_type") = recorder_ptr->event_type_vec,
      Rcpp::Named("bar_stage") = enum_vec_to_int(recorder_ptr->bar_stage),
      Rcpp::Named("action_id") = recorder_ptr->action_id_vec,
      Rcpp::Named("strat_id") = recorder_ptr->strat_id_vec,
      Rcpp::Named("asset_id") = recorder_ptr->asset_id_vec,
      Rcpp::Named("tx_id") = size_t_vec_to_int(recorder_ptr->tx_id_vec),
      Rcpp::Named("status") = enum_vec_to_int(recorder_ptr->status),
      Rcpp::Named("liquidation") = bool_vec_to_logical(recorder_ptr->liquidation),
      Rcpp::Named("action") = enum_vec_to_int(recorder_ptr->action),
      Rcpp::Named("dir") = enum_vec_to_int(recorder_ptr->dir),
      Rcpp::Named("ctr_qty") = recorder_ptr->ctr_qty,
      Rcpp::Named("price") = recorder_ptr->price,
      Rcpp::Named("equity") = recorder_ptr->eq_vec,
      Rcpp::Named("cash") = recorder_ptr->cash_vec,
      Rcpp::Named("state_dir") = enum_vec_to_int(recorder_ptr->state_dir_vec),
      Rcpp::Named("state_ctr_unit") = recorder_ptr->state_ctr_unit_vec,
      Rcpp::Named("avg_price") = recorder_ptr->avg_price_vec,
      Rcpp::Named("last_px") = recorder_ptr->last_px_vec,
      Rcpp::Named("notional") = recorder_ptr->notional_vec,
      Rcpp::Named("abs_notional") = recorder_ptr->abs_notional_vec,
      Rcpp::Named("unrealized_pnl") = recorder_ptr->unrealized_pnl_vec,
      Rcpp::Named("realized_pnl") = recorder_ptr->realized_pnl_vec,
      Rcpp::Named("fee") = recorder_ptr->fee_vec,
      Rcpp::Named("funding_fee") = recorder_ptr->funding_fee_vec,
      Rcpp::Named("maintenance_margin") = recorder_ptr->maintenance_margin_vec
    );
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
