#include "almanac.h"
#include "utils.h"
#include "ql/time/calendars/all.hpp"

// Defined below
static QuantLib::BespokeCalendar new_custom_calendar(const Rcpp::List& calendar);

void reset_calendar(QuantLib::Calendar calendar) {
  std::set<QuantLib::Date> holidays = calendar.addedHolidays();
  std::set<QuantLib::Date>::iterator it;

  for (it = holidays.begin(); it != holidays.end(); ++it){
    calendar.removeHoliday(*it);
  }
}

static QuantLib::Calendar init_calendar(const std::string& name) {
  if (name == "argentina" || name == "argentina_merval") {
    return QuantLib::Argentina(QuantLib::Argentina::Merval);
  }

  if (name == "united_states" || name == "united_states_settlement") {
    return QuantLib::UnitedStates(QuantLib::UnitedStates::Settlement);
  }

  Rf_errorcall(R_NilValue, "Unknown calendar name, '%s'", name.c_str());
}

static void append_holidays(QuantLib::Calendar calendar, const Rcpp::DateVector& dates) {
  int size = dates.size();

  std::vector<QuantLib::Date> holidays = as_quantlib_date(dates);

  QuantLib::Date holiday;

  for(int i = 0; i < size; i++) {
    holiday = holidays[i];
    calendar.addHoliday(holiday);
  }
}

QuantLib::Calendar new_calendar(const Rcpp::List& calendar) {
  std::string name = calendar[0];

  if (name == "custom") {
    return new_custom_calendar(calendar);
  }

  Rcpp::DateVector holidays = calendar[1];
  bool has_holidays = holidays.size() > 0;

  QuantLib::Calendar ql_calendar = init_calendar(name);

  if (has_holidays) {
    append_holidays(ql_calendar, holidays);
  }

  return ql_calendar;
}

// -----------------------------------------------------------------------------
// "Custom" calendar support - with user defined weekends as well as holidays
// Apparently you don't have to remove the weekends like you do the holidays

static QuantLib::BespokeCalendar init_custom_calendar() {
  return QuantLib::BespokeCalendar();
}

static void append_weekends(QuantLib::BespokeCalendar calendar, const Rcpp::IntegerVector& weekends) {
  int size = weekends.size();

  QuantLib::Weekday weekend;

  for(int i = 0; i < size; i++) {
    weekend = as_weekday(weekends[i]);
    calendar.addWeekend(weekend);
  }
}

static QuantLib::BespokeCalendar new_custom_calendar(const Rcpp::List& calendar) {
  QuantLib::BespokeCalendar custom_calendar = init_custom_calendar();

  Rcpp::DateVector holidays = calendar[1];
  bool has_holidays = holidays.size() > 0;

  Rcpp::IntegerVector weekends = calendar[2];
  bool has_weekends = weekends.size() > 0;

  if (has_holidays) {
    append_holidays(custom_calendar, holidays);
  }

  if (has_weekends) {
    append_weekends(custom_calendar, weekends);
  }

  return custom_calendar;
}
