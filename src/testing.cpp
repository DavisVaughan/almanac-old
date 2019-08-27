#include "calendarrr2.h"

// [[Rcpp::export(rng = false)]]
Rcpp::DateVector calendar_roundtrip_date(Rcpp::DateVector x) {
  std::vector<QuantLib::Date> dates = as_quantlib_date(x);
  return as_r_date(dates);
}
