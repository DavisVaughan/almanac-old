#include "almanac.h"
#include "utils.h"

// NOTE:
// We are NOT implementing `end_of_month` here like they do in `advance()`.
// I really don't like that this can produce dates that are out of order.
// For example:
// `cal_shift(c("2016-03-30", "2016-03-31"), "1 month", end_of_month = TRUE)`
// [1] "2016-05-02" "2016-04-29"
// `cal_is_weekend("2016-04-30")`
// `cal_is_weekend("2016-05-01")`
// This shift produces a decreasing sequence of dates because "2016-03-30"
// is not an end of month date, so it is shifted forward 1 month then follows
// the business day convention of "following". Since 4/30 and 5/01 are holidays,
// we end up with 5/02. For "2016-03-31", it is an end of month date, so when
// `end_of_month` is set to true we force an end of month value in the month
// we land in (we land on 4/30), and the end of month there is 4/29.

// Essentially copies the logic of `advance()` in `calendar.cpp`
// but adjusts it so that we can shift by "1 year and 1 day" while still
// performing business day adjustments correctly
static QuantLib::Date multi_advance(const QuantLib::Date& date,
                                    int year,
                                    int month,
                                    int day,
                                    const QuantLib::BusinessDayConvention convention,
                                    const QuantLib::Calendar calendar) {
  QuantLib::Date new_date = date;

  // Year and Month shifts first
  new_date = new_date + year * QuantLib::TimeUnit::Years;
  new_date = new_date + month * QuantLib::TimeUnit::Months;

  // If no Day shift, call adjust() and return
  if (day == 0) {
    return calendar.adjust(new_date, convention);
  }

  // If there is a day shift, don't call adjust(), but instead use isHoliday()
  if (day > 0) {
    while (day > 0) {
      new_date++;
      while (calendar.isHoliday(new_date))
        new_date++;
      day--;
    }

    return new_date;
  }

  while (day < 0) {
    new_date--;
    while(calendar.isHoliday(new_date))
      new_date--;
    day++;
  }

  return new_date;
}

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_shift(const Rcpp::DateVector& x,
                                const Rcpp::List& period,
                                const std::string& convention,
                                const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  int size = x.size();

  int year = period[0];
  int month = period[1];
  int day = period[2];

  QuantLib::BusinessDayConvention ql_convention = as_business_day_convention(convention);

  QuantLib::Date ql_date;
  QuantLib::Date new_ql_date;

  Rcpp::Date date;
  Rcpp::DateVector out(size);

  for (int i = 0; i < size; ++i) {
    date = x[i];

    if (Rcpp::DateVector::is_na(date)) {
      out[i] = NA_REAL;
      continue;
    }

    ql_date = as_quantlib_date(date);

    new_ql_date = multi_advance(
      ql_date,
      year,
      month,
      day,
      ql_convention,
      ql_calendar
    );

    out[i] = as_r_date(new_ql_date);
  }

  reset_calendar(ql_calendar);
  return out;
}

// [[Rcpp::export(rng=false)]]
Rcpp::DateVector calendar_shift_end_of_month(const Rcpp::DateVector x,
                                             const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  int size = x.size();

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

    new_ql_date = ql_calendar.endOfMonth(ql_date);

    out[i] = as_r_date(new_ql_date);
  }

  reset_calendar(ql_calendar);
  return out;
}
