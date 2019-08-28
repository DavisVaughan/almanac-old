#include "almanac.h"
#include "utils.h"

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_adjust(const Rcpp::DateVector x,
                                 const std::string& convention,
                                 const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  int size = x.size();

  QuantLib::BusinessDayConvention ql_convention = as_business_day_convention(convention);

  Rcpp::Date date;
  QuantLib::Date ql_date;
  QuantLib::Date new_ql_date;
  Rcpp::DateVector out(size);

  for (int i = 0; i < size; ++i) {
    date = x[i];

    if (Rcpp::DateVector::is_na(date)) {
      out[i] = NA_REAL;
      continue;
    }

    ql_date = as_quantlib_date(date);

    new_ql_date = ql_calendar.adjust(ql_date, ql_convention);

    out[i] = as_r_date(new_ql_date);
  }

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::IntegerVector calendar_count_business_days_between(const Rcpp::DateVector starts,
                                                         const Rcpp::DateVector stops,
                                                         const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  int size = starts.size();

  Rcpp::Date start;
  Rcpp::Date stop;
  QuantLib::Date ql_start;
  QuantLib::Date ql_stop;
  Rcpp::IntegerVector out(size);

  for (int i = 0; i < size; ++i) {
    start = starts[i];

    if (Rcpp::DateVector::is_na(start)) {
      out[i] = NA_INTEGER;
      continue;
    }

    stop = stops[i];

    if (Rcpp::DateVector::is_na(stop)) {
      out[i] = NA_INTEGER;
      continue;
    }

    ql_start = as_quantlib_date(start);
    ql_stop = as_quantlib_date(stop);

    out[i] = ql_calendar.businessDaysBetween(ql_start, ql_stop, true, false);
  }

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::LogicalVector calendar_is_weekend(const Rcpp::DateVector x,
                                        const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  int size = x.size();

  Rcpp::Date date;
  QuantLib::Date ql_date;
  Rcpp::LogicalVector out(size);

  for (int i = 0; i < size; ++i) {
    date = x[i];

    if (Rcpp::DateVector::is_na(date)) {
      out[i] = NA_LOGICAL;
      continue;
    }

    ql_date = as_quantlib_date(date);

    out[i] = ql_calendar.isWeekend(ql_date.weekday());
  }

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::LogicalVector calendar_is_business_day(const Rcpp::DateVector x,
                                             const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  int size = x.size();

  Rcpp::Date date;
  QuantLib::Date ql_date;
  Rcpp::LogicalVector out(size);

  for (int i = 0; i < size; ++i) {
    date = x[i];

    if (Rcpp::DateVector::is_na(date)) {
      out[i] = NA_LOGICAL;
      continue;
    }

    ql_date = as_quantlib_date(date);

    out[i] = ql_calendar.isBusinessDay(ql_date);
  }

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::LogicalVector calendar_is_holiday(const Rcpp::DateVector x,
                                        const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  int size = x.size();

  Rcpp::Date date;
  QuantLib::Date ql_date;
  Rcpp::LogicalVector out(size);

  for (int i = 0; i < size; ++i) {
    date = x[i];

    if (Rcpp::DateVector::is_na(date)) {
      out[i] = NA_LOGICAL;
      continue;
    }

    ql_date = as_quantlib_date(date);

    out[i] = ql_calendar.isHoliday(ql_date);
  }

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::LogicalVector calendar_is_end_of_month(const Rcpp::DateVector x,
                                             const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  int size = x.size();

  Rcpp::Date date;
  QuantLib::Date ql_date;
  Rcpp::LogicalVector out(size);

  for (int i = 0; i < size; ++i) {
    date = x[i];

    if (Rcpp::DateVector::is_na(date)) {
      out[i] = NA_LOGICAL;
      continue;
    }

    ql_date = as_quantlib_date(date);

    out[i] = ql_calendar.isEndOfMonth(ql_date);
  }

  reset_calendar(ql_calendar);
  return out;
}
