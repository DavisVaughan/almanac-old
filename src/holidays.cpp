#include "almanac.h"

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_holidays_between(const Rcpp::DateVector start,
                                           const Rcpp::DateVector stop,
                                           const bool weekends,
                                           const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  std::vector<QuantLib::Date> ql_start = as_quantlib_date(start);
  std::vector<QuantLib::Date> ql_stop = as_quantlib_date(stop);

  std::vector<QuantLib::Date> ql_holidays = QuantLib::Calendar::holidayList(
    ql_calendar,
    ql_start[0],
    ql_stop[0],
    weekends
  );

  Rcpp::DateVector out = as_r_date(ql_holidays);

  reset_calendar(ql_calendar);
  return out;
}
