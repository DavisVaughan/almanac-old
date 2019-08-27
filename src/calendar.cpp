#include "calendarrr2.h"
#include "ql/time/calendars/all.hpp"

static QuantLib::Calendar init_calendar(const std::string& name) {
  if (name == "united_states") {
    return QuantLib::UnitedStates(QuantLib::UnitedStates::Settlement);
  }

  if (name == "united_states_settlement") {
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

  Rcpp::DateVector holidays = calendar[1];
  bool has_holidays = holidays.size() > 0;

  QuantLib::Calendar ql_calendar = init_calendar(name);

  if (has_holidays) {
    append_holidays(ql_calendar, holidays);
  }

  return ql_calendar;
}

void reset_calendar(QuantLib::Calendar calendar) {
  std::set<QuantLib::Date> holidays = calendar.addedHolidays();
  std::set<QuantLib::Date>::iterator it;

  for (it = holidays.begin(); it != holidays.end(); ++it){
    calendar.removeHoliday(*it);
  }
}
