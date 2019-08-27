#include "almanac.h"
#include "utils.h"
#include "ql/time/dategenerationrule.hpp"
#include "ql/time/period.hpp"
#include "ql/time/schedule.hpp"

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_seq(const Rcpp::DateVector start,
                              const Rcpp::DateVector stop,
                              const Rcpp::IntegerVector by,
                              const std::string& unit,
                              const std::string& start_convention,
                              const std::string& stop_convention,
                              const bool& end_of_month,
                              const Rcpp::List& calendar) {
  QuantLib::DateGeneration::Rule rule;
  Rcpp::LogicalVector start_before_stop = start <= stop;

  std::vector<QuantLib::Date> ql_start;
  std::vector<QuantLib::Date> ql_stop;

  QuantLib::BusinessDayConvention ql_start_convention;
  QuantLib::BusinessDayConvention ql_stop_convention;

  bool reverse;

  if (start_before_stop[0]) {
    ql_start = as_quantlib_date(start);
    ql_stop = as_quantlib_date(stop);
    ql_start_convention = as_business_day_convention(start_convention);
    ql_stop_convention = as_business_day_convention(stop_convention);
    rule = QuantLib::DateGeneration::Forward;
    reverse = false;
  } else {
    ql_start = as_quantlib_date(stop);
    ql_stop = as_quantlib_date(start);
    ql_start_convention = as_business_day_convention(stop_convention);
    ql_stop_convention = as_business_day_convention(start_convention);
    rule = QuantLib::DateGeneration::Backward;
    reverse = true;
  }

  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  QuantLib::TimeUnit ql_timeunit = as_time_unit(unit);
  QuantLib::Period ql_period(by[0], ql_timeunit);

  QuantLib::Schedule schedule = QuantLib::MakeSchedule()
    .from(ql_start[0])
    .to(ql_stop[0])
    .withTenor(ql_period)
    .withCalendar(ql_calendar)
    .withConvention(ql_start_convention)
    .withTerminationDateConvention(ql_stop_convention)
    .withRule(rule)
    .endOfMonth(end_of_month);

  std::vector<QuantLib::Date> new_dates = schedule.dates();

  Rcpp::DateVector out = as_r_date(new_dates);

  if (reverse) {
    std::reverse(out.begin(), out.end());
  }

  reset_calendar(ql_calendar);
  return out;
}