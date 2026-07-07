// base_utils.h
#pragma once

#include <cmath>
#include <limits>

namespace TRADESIMR {
  enum class ActionCode : int { REDUCE = -2, CLOSE = -1, NONE = 0, OPEN = 1, INCREASE = 2 };
  enum class Dir : int { SHORT = -1, FLAT = 0, LONG = 1 };
  enum class OrderType : int { MARKET = 0, LIMIT = 1 };
  enum class ActionStatus : int { PENDING = 0, FILLED = 1, FAILED = -1 };
  enum class BarStage : int { OPEN = 1, INTRA = 2, CLOSE = 3 };

  static constexpr double kNaReal = std::numeric_limits<double>::quiet_NaN();
  static constexpr double kInfReal = std::numeric_limits<double>::infinity();
}

inline bool is_na(double x) {
  return std::isnan(x);
}
