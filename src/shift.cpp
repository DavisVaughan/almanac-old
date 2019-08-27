#include "almanac.h"
#include "utils.h"

// Essentially copies the logic of `advance()` in `calendar.cpp`
// but adjusts it so that we can shift by "1 year and 1 day" while still
// performing business day adjustments correctly
static QuantLib::Date multi_advance(const QuantLib::Date& date,
                                    int year,
                                    int month,
                                    int day,
                                    const QuantLib::BusinessDayConvention convention,
                                    const bool& end_of_month,
                                    const QuantLib::Calendar calendar) {
  QuantLib::Date new_date = date;

  // Year and Month shifts first
  new_date = new_date + year * QuantLib::TimeUnit::Years;
  new_date = new_date + month * QuantLib::TimeUnit::Months;

  // If no Day shift, call adjust() and return
  if (day == 0) {
    if (end_of_month && calendar.isEndOfMonth(date)) {
      return calendar.endOfMonth(new_date);
    }

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
Rcpp::DateVector calendar_shift(const Rcpp::DateVector x,
                                const Rcpp::List period,
                                const std::string& convention,
                                const bool& end_of_month,
                                const Rcpp::List& calendar) {
  QuantLib::Calendar ql_calendar = new_calendar(calendar);

  std::vector<QuantLib::Date> dates = as_quantlib_date(x);
  int size = x.size();

  int year = period[0];
  int month = period[1];
  int day = period[2];

  QuantLib::BusinessDayConvention ql_convention = as_business_day_convention(convention);

  QuantLib::Date date;
  QuantLib::Date new_date;

  std::vector<QuantLib::Date> new_dates(size);

  for (int i = 0; i < size; ++i) {
    date = dates[i];

    new_date = multi_advance(
      date, year, month, day,
      ql_convention, end_of_month, ql_calendar
    );

    new_dates[i] = new_date;
  }

  Rcpp::DateVector out = as_r_date(new_dates);

  reset_calendar(ql_calendar);
  return out;
}
