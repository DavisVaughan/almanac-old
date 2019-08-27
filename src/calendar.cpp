#include "almanac.h"
#include "utils.h"
#include "ql/time/calendars/all.hpp"

// Defined below
static QuantLib::BespokeCalendar new_empty_calendar(const Rcpp::List& calendar);

// -----------------------------------------------------------------------------

void reset_added_holidays(QuantLib::Calendar calendar) {
  std::set<QuantLib::Date> holidays = calendar.addedHolidays();
  std::set<QuantLib::Date>::iterator it;

  for (it = holidays.begin(); it != holidays.end(); ++it){
    calendar.removeHoliday(*it);
  }
}

void reset_removed_holidays(QuantLib::Calendar calendar) {
  std::set<QuantLib::Date> holidays = calendar.removedHolidays();
  std::set<QuantLib::Date>::iterator it;

  for (it = holidays.begin(); it != holidays.end(); ++it){
    calendar.addHoliday(*it);
  }
}

void reset_calendar(QuantLib::Calendar calendar) {
  reset_added_holidays(calendar);
  reset_removed_holidays(calendar);
}

// -----------------------------------------------------------------------------

static QuantLib::Calendar init_calendar(const std::string& name) {
  if (name == "argentina" || name == "argentina_merval") {
    return QuantLib::Argentina(QuantLib::Argentina::Merval);
  }

  if (name == "united_states" || name == "united_states_settlement") {
    return QuantLib::UnitedStates(QuantLib::UnitedStates::Settlement);
  }

  Rf_errorcall(R_NilValue, "Unknown calendar name, '%s'", name.c_str());
}

// -----------------------------------------------------------------------------

static void add_holidays(QuantLib::Calendar calendar, const Rcpp::DateVector& dates) {
  int size = dates.size();

  if (size == 0) {
    return;
  }

  std::vector<QuantLib::Date> holidays = as_quantlib_date(dates);

  QuantLib::Date holiday;

  for(int i = 0; i < size; i++) {
    holiday = holidays[i];
    calendar.addHoliday(holiday);
  }
}

static void remove_holidays(QuantLib::Calendar calendar, const Rcpp::DateVector& dates) {
  int size = dates.size();

  if (size == 0) {
    return;
  }

  std::vector<QuantLib::Date> holidays = as_quantlib_date(dates);

  QuantLib::Date holiday;

  for(int i = 0; i < size; i++) {
    holiday = holidays[i];
    calendar.removeHoliday(holiday);
  }
}

void adjust_holidays(QuantLib::Calendar calendar,
                     const Rcpp::DateVector& added_holidays,
                     const Rcpp::DateVector& removed_holidays) {
  add_holidays(calendar, added_holidays);
  remove_holidays(calendar, removed_holidays);
}

// -----------------------------------------------------------------------------

QuantLib::Calendar new_calendar(const Rcpp::List& calendar) {
  std::string name = calendar[0];

  if (name == "empty") {
    return new_empty_calendar(calendar);
  }

  QuantLib::Calendar ql_calendar = init_calendar(name);

  Rcpp::DateVector added_holidays = calendar[1];
  Rcpp::DateVector removed_holidays = calendar[2];

  adjust_holidays(ql_calendar, added_holidays, removed_holidays);

  return ql_calendar;
}

// -----------------------------------------------------------------------------
// "empty" calendar support - with user defined weekends as well as holidays
// Apparently you don't have to remove the weekends like you do the holidays

static QuantLib::BespokeCalendar init_empty_calendar() {
  return QuantLib::BespokeCalendar();
}

static void add_weekends(QuantLib::BespokeCalendar calendar, const Rcpp::IntegerVector& weekends) {
  int size = weekends.size();

  if (size == 0) {
    return;
  }

  QuantLib::Weekday weekend;

  for(int i = 0; i < size; i++) {
    weekend = as_weekday(weekends[i]);
    calendar.addWeekend(weekend);
  }
}

static QuantLib::BespokeCalendar new_empty_calendar(const Rcpp::List& calendar) {
  QuantLib::BespokeCalendar empty_calendar = init_empty_calendar();

  Rcpp::DateVector added_holidays = calendar[1];
  Rcpp::DateVector removed_holidays = calendar[2];
  Rcpp::IntegerVector weekends = calendar[3];

  adjust_holidays(empty_calendar, added_holidays, removed_holidays);
  add_weekends(empty_calendar, weekends);

  return empty_calendar;
}
