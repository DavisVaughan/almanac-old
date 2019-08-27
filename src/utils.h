#ifndef CALENDARRR2_UTILS_H
#define CALENDARRR2_UTILS_H

#include "calendarrr2.h"
#include "ql/time/businessdayconvention.hpp"
#include "ql/time/timeunit.hpp"

QuantLib::BusinessDayConvention as_business_day_convention(const std::string& convention);
QuantLib::TimeUnit as_time_unit(const std::string &unit);

#endif
