#include "utils.h"

QuantLib::BusinessDayConvention as_business_day_convention(const std::string& convention) {
  if (convention == "following") {
    return QuantLib::Following;
  }

  if (convention == "modified_following") {
    return QuantLib::ModifiedFollowing;
  }

  if (convention == "preceding") {
    return QuantLib::Preceding;
  }

  if (convention == "modified_preceding") {
    return QuantLib::ModifiedPreceding;
  }

  if (convention == "unadjusted") {
    return QuantLib::Unadjusted;
  }

  if (convention == "half_month_modified_following") {
    return QuantLib::HalfMonthModifiedFollowing;
  }

  if (convention == "nearest") {
    return QuantLib::Nearest;
  }

  Rf_errorcall(R_NilValue, "Unknown `convention`, %s", convention.c_str());
}

QuantLib::TimeUnit as_time_unit(const std::string &unit) {
  if (unit == "day") {
    return QuantLib::Days;
  }

  if (unit == "week") {
    return QuantLib::Weeks;
  }

  if (unit == "month") {
    return QuantLib::Months;
  }

  if (unit == "year") {
    return QuantLib::Years;
  }

  Rf_errorcall(R_NilValue, "Unknown `unit`, %s", unit.c_str());
}

QuantLib::Weekday as_weekday(const int& weekday) {
  if (weekday == 1) {
    return QuantLib::Weekday::Sunday;
  }

  if (weekday == 2) {
    return QuantLib::Weekday::Monday;
  }

  if (weekday == 3) {
    return QuantLib::Weekday::Tuesday;
  }

  if (weekday == 4) {
    return QuantLib::Weekday::Wednesday;
  }

  if (weekday == 5) {
    return QuantLib::Weekday::Thursday;
  }

  if (weekday == 6) {
    return QuantLib::Weekday::Friday;
  }

  if (weekday == 7) {
    return QuantLib::Weekday::Saturday;
  }

  Rf_errorcall(R_NilValue, "Unknown `weekday`, %i", weekday);
}
