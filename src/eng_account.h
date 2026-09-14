#pragma once

#include <cmath>
#include <limits>
#include <string>
#include <vector>

// Profile-aware account primitives. These deliberately do not replace
// TradeState: the legacy derivative kernel remains a compatibility path while
// the heterogeneous account kernel is validated.
struct CashBalance {
  std::string currency;
  double settled = 0.0;
  double unsettled = 0.0;
};

struct InventoryPosition {
  int asset_id = 0;
  std::string currency;
  double units = 0.0;
  double average_cost = std::numeric_limits<double>::quiet_NaN();
  double last_price = std::numeric_limits<double>::quiet_NaN();
  double contract_size = 1.0;
};

struct MarginPosition {
  int asset_id = 0;
  std::string currency;
  double signed_units = 0.0;
  double settlement_price = std::numeric_limits<double>::quiet_NaN();
  double last_price = std::numeric_limits<double>::quiet_NaN();
  double contract_size = 1.0;
  double maintenance_rate = 0.0;
};

inline double inventory_value(const InventoryPosition& p) {
  return p.units * p.last_price * p.contract_size;
}

inline double margin_notional(const MarginPosition& p) {
  return p.signed_units * p.last_price * p.contract_size;
}

inline double variation_margin(const MarginPosition& p) {
  if (!std::isfinite(p.settlement_price) || !std::isfinite(p.last_price)) return 0.0;
  return p.signed_units * (p.last_price - p.settlement_price) * p.contract_size;
}
