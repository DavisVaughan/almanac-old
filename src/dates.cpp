#include "calendarrr2.h"
#include "utils.h"

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_adjust(const Rcpp::DateVector x,
                                 const std::string& convention,
                                 const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  std::vector<QuantLib::Date> dates = as_quantlib_date(x);
  int size = x.size();

  QuantLib::BusinessDayConvention ql_convention = as_business_day_convention(convention);

  std::vector<QuantLib::Date> new_dates(size);

  for (int i = 0; i < size; ++i) {
    new_dates[i] = ql_calendar.adjust(dates[i], ql_convention);
  }

  Rcpp::DateVector out = as_r_date(new_dates);

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_adjust_end_of_month(const Rcpp::DateVector x,
                                              const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  std::vector<QuantLib::Date> dates = as_quantlib_date(x);
  int size = x.size();

  std::vector<QuantLib::Date> new_dates(size);

  for (int i = 0; i < size; ++i) {
    new_dates[i] = ql_calendar.endOfMonth(dates[i]);
  }

  Rcpp::DateVector out = as_r_date(new_dates);

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_advance(const Rcpp::DateVector x,
                                  const Rcpp::IntegerVector by,
                                  const std::string& unit,
                                  const std::string& convention,
                                  const bool& end_of_month,
                                  const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  std::vector<QuantLib::Date> dates = as_quantlib_date(x);
  int size = x.size();

  QuantLib::BusinessDayConvention ql_convention = as_business_day_convention(convention);
  QuantLib::TimeUnit ql_timeunit = as_time_unit(unit);

  std::vector<QuantLib::Date> new_dates(size);

  for (int i = 0; i < size; ++i) {
    new_dates[i] = ql_calendar.advance(
      dates[i],
      by[i],
      ql_timeunit,
      ql_convention,
      end_of_month
    );
  }

  Rcpp::DateVector out = as_r_date(new_dates);

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_detect_holidays(const Rcpp::DateVector start,
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

// [[Rcpp::export(rng=false)]]
Rcpp::IntegerVector calendar_count_business_days_between(const Rcpp::DateVector starts,
                                                         const Rcpp::DateVector stops,
                                                         const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  std::vector<QuantLib::Date> ql_starts = as_quantlib_date(starts);
  std::vector<QuantLib::Date> ql_stops = as_quantlib_date(stops);

  int size = ql_starts.size();

  Rcpp::IntegerVector out(size);

  // Inclusive on both ends
  for (int i = 0; i < size; ++i) {
    out[i] = ql_calendar.businessDaysBetween(ql_starts[i], ql_stops[i], true, true);
  }

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::LogicalVector calendar_is_weekend(const Rcpp::DateVector x,
                                        const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  std::vector<QuantLib::Date> dates = as_quantlib_date(x);
  int size = x.size();

  Rcpp::LogicalVector out(size);

  for (int i = 0; i < size; ++i) {
    out[i] = ql_calendar.isWeekend(dates[i].weekday());
  }

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::LogicalVector calendar_is_business_day(const Rcpp::DateVector x,
                                             const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  std::vector<QuantLib::Date> dates = as_quantlib_date(x);
  int size = x.size();

  Rcpp::LogicalVector out(size);

  for (int i = 0; i < size; ++i) {
    out[i] = ql_calendar.isBusinessDay(dates[i]);
  }

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::LogicalVector calendar_is_holiday(const Rcpp::DateVector x,
                                        const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  std::vector<QuantLib::Date> dates = as_quantlib_date(x);
  int size = x.size();

  Rcpp::LogicalVector out(size);

  for (int i = 0; i < size; ++i) {
    out[i] = ql_calendar.isHoliday(dates[i]);
  }

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::LogicalVector calendar_is_end_of_month(const Rcpp::DateVector x,
                                             const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  std::vector<QuantLib::Date> dates = as_quantlib_date(x);
  int size = x.size();

  Rcpp::LogicalVector out(size);

  for (int i = 0; i < size; ++i) {
    out[i] = ql_calendar.isEndOfMonth(dates[i]);
  }

  reset_calendar(ql_calendar);
  return out;
}
