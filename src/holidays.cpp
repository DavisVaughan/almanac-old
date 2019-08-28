#include "almanac.h"

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_holidays_between(const Rcpp::DateVector& start,
                                           const Rcpp::DateVector& stop,
                                           const bool& weekends,
                                           const Rcpp::List& calendar) {
  if (Rcpp::DateVector::is_na(start[0]) || Rcpp::DateVector::is_na(stop[0])) {
    Rcpp::DateVector out(0);
    return out;
  }

  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  QuantLib::Date ql_start = as_quantlib_date(start[0]);
  QuantLib::Date ql_stop = as_quantlib_date(stop[0]);

  std::vector<QuantLib::Date> ql_holidays = QuantLib::Calendar::holidayList(
    ql_calendar,
    ql_start,
    ql_stop,
    weekends
  );

  int size = ql_holidays.size();
  Rcpp::DateVector out(size);

  for (int i = 0; i < size; ++i) {
    out[i] = as_r_date(ql_holidays[i]);
  }

  reset_calendar(ql_calendar);
  return out;
}
