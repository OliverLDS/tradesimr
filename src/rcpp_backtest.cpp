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
Rcpp::NumericVector backtest_rcpp(const Rcpp::NumericVector& timestamp,
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
  Recorder* recorder_ptr = nullptr;
  if (rec) recorder_ptr = new Recorder();

  backtest(
    eq.begin(),
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
    Rcpp::List rec_out = Rcpp::List::create(
      Rcpp::Named("timestamp") = recorder_ptr->ts,
      Rcpp::Named("bar_stage") = enum_vec_to_int(recorder_ptr->bar_stage),
      Rcpp::Named("action_id") = recorder_ptr->action_id_vec,
      Rcpp::Named("strat_id") = recorder_ptr->strat_id_vec,
      Rcpp::Named("tx_id") = size_t_vec_to_int(recorder_ptr->tx_id_vec),
      Rcpp::Named("status") = enum_vec_to_int(recorder_ptr->status),
      Rcpp::Named("liquidation") = bool_vec_to_logical(recorder_ptr->liquidation),
      Rcpp::Named("action") = enum_vec_to_int(recorder_ptr->action),
      Rcpp::Named("dir") = enum_vec_to_int(recorder_ptr->dir),
      Rcpp::Named("ctr_qty") = recorder_ptr->ctr_qty,
      Rcpp::Named("price") = recorder_ptr->price,
      Rcpp::Named("eq") = recorder_ptr->eq_vec
    );
    eq.attr("recorder") = rec_out;
  }

  if (recorder_ptr) delete recorder_ptr;
  return eq;
}
