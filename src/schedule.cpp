#include "almanac.h"
#include "utils.h"
#include "ql/time/dategenerationrule.hpp"
#include "ql/time/period.hpp"
#include "ql/time/schedule.hpp"

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_seq(const Rcpp::DateVector& start,
                              const Rcpp::DateVector& stop,
                              const int& by,
                              const std::string& unit,
                              const std::string& start_convention,
                              const std::string& stop_convention,
                              const bool& end_of_month,
                              const Rcpp::List& calendar) {
  const Rcpp::Date start_elt = start[0];
  const Rcpp::Date stop_elt = stop[0];

  if (Rcpp::DateVector::is_na(start_elt)) {
    Rf_errorcall(R_NilValue, "`start` cannot be `NA`.");
  }

  if (Rcpp::DateVector::is_na(stop_elt)) {
    Rf_errorcall(R_NilValue, "`stop` cannot be `NA`.");
  }

  QuantLib::DateGeneration::Rule rule;
  bool start_before_stop = start_elt <= stop_elt;

  QuantLib::Date ql_start;
  QuantLib::Date ql_stop;

  QuantLib::BusinessDayConvention ql_start_convention;
  QuantLib::BusinessDayConvention ql_stop_convention;

  bool reverse;

  if (start_before_stop) {
    ql_start = as_quantlib_date(start_elt);
    ql_stop = as_quantlib_date(stop_elt);
    ql_start_convention = as_business_day_convention(start_convention);
    ql_stop_convention = as_business_day_convention(stop_convention);
    rule = QuantLib::DateGeneration::Forward;
    reverse = false;
  } else {
    ql_start = as_quantlib_date(stop_elt);
    ql_stop = as_quantlib_date(start_elt);
    ql_start_convention = as_business_day_convention(stop_convention);
    ql_stop_convention = as_business_day_convention(start_convention);
    rule = QuantLib::DateGeneration::Backward;
    reverse = true;
  }

  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  QuantLib::TimeUnit ql_timeunit = as_time_unit(unit);
  QuantLib::Period ql_period(by, ql_timeunit);

  QuantLib::Schedule schedule = QuantLib::MakeSchedule()
    .from(ql_start)
    .to(ql_stop)
    .withTenor(ql_period)
    .withCalendar(ql_calendar)
    .withConvention(ql_start_convention)
    .withTerminationDateConvention(ql_stop_convention)
    .withRule(rule)
    .endOfMonth(end_of_month);

  std::vector<QuantLib::Date> new_dates = schedule.dates();

  int size = new_dates.size();
  Rcpp::DateVector out(size);

  for (int i = 0; i < size; ++i) {
    out[i] = as_r_date(new_dates[i]);
  }

  if (reverse) {
    std::reverse(out.begin(), out.end());
  }

  reset_calendar(ql_calendar);
  return out;
}
