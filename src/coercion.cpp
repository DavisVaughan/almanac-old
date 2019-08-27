#include "calendarrr2.h"

static const unsigned int quantlib_to_r_offset_in_days = 25569;

std::vector<QuantLib::Date> as_quantlib_date(const Rcpp::DateVector dates) {
  int size = dates.size();

  std::vector<QuantLib::Date> new_dates(size);

  Rcpp::Date date;
  int r_integer_date;
  int quantlib_integer_date;

  for (int i = 0; i < size; ++i) {
    date = dates[i];

    r_integer_date = static_cast<int>(date.getDate());
    quantlib_integer_date = r_integer_date + quantlib_to_r_offset_in_days;

    new_dates[i] = QuantLib::Date(quantlib_integer_date);
  }

  return new_dates;
}

Rcpp::DateVector as_r_date(const std::vector<QuantLib::Date> dates) {
  int size = dates.size();

  Rcpp::DateVector new_dates(size);

  QuantLib::Date date;
  int r_integer_date;
  int quantlib_integer_date;

  for (int i = 0; i < size; ++i) {
    date = dates[i];

    quantlib_integer_date = date.serialNumber();
    r_integer_date = quantlib_integer_date - quantlib_to_r_offset_in_days;

    new_dates[i] = Rcpp::Date(r_integer_date);
  }

  return new_dates;
}

// -----------------------------------------------------------------------------
// Datetimes are REALLY hard to get right, and I think they are going to be less
// useful than the Dates + holidays, so lets ignore them. I don't think they are
// really fully implemented in QuantLib anyways.

/*
static const unsigned long quantlib_to_r_offset_in_seconds = 2209161600;

std::vector<QuantLib::Date> as_quantlib_date(Rcpp::DatetimeVector datetimes) {
  int size = datetimes.size();

  std::vector<QuantLib::Date> new_dates(size);

  Rcpp::Datetime datetime;

  boost::gregorian::date date_part;
  boost::posix_time::time_duration time_part;
  boost::posix_time::ptime boost_datetime;

  int year;
  int month;
  int day;
  int hour;
  int minute;
  int second;
  int fractional_second;

  for (int i = 0; i < size; ++i) {
    datetime = datetimes[i];

    year = datetime.getYear();
    month = datetime.getMonth();
    day = datetime.getDay();
    hour = datetime.getHours();
    minute = datetime.getMinutes();
    second = datetime.getSeconds();
    fractional_second = datetime.getMicroSeconds() * 1000;
    //fractional_second = datetime.getMicroSeconds() * boost::posix_time::time_duration::ticks_per_second() / 1000000;

    date_part = boost::gregorian::date(year, month, day);
    time_part = boost::posix_time::time_duration(hour, minute, second, fractional_second);

    boost_datetime = boost::posix_time::ptime(date_part, time_part);

    new_dates[i] = QuantLib::Date(boost_datetime);
  }

  return new_dates;
}

Rcpp::DatetimeVector as_r_datetime(std::vector<QuantLib::Date> datetimes) {
  int size = datetimes.size();

  Rcpp::DatetimeVector new_datetimes(size);

  // POSIXct stores datetimes as doubles
  QuantLib::Date datetime;
  boost::posix_time::ptime boost_datetime;
  boost::gregorian::date date_part;
  boost::posix_time::time_duration time_part;

  int year;
  int month;
  int day;
  int hour;
  int minute;
  int second;
  int fractional_second;

  for (int i = 0; i < size; ++i) {
    datetime = datetimes[i];
    boost_datetime = datetime.dateTime();

    date_part = boost_datetime.date();
    time_part = boost_datetime.time_of_day();

    year = date_part.year();
    month = date_part.month();
    day = date_part.day();
    hour = time_part.hours();
    minute = time_part.minutes();
    second = time_part.seconds();
    fractional_second = time_part.fractional_seconds();

    //new_datetimes[i] = Rcpp::Datetime(r_double_date);
  }

  return new_datetimes;
}

*/
