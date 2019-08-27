#ifndef ALMANAC_UTILS_H
#define ALMANAC_UTILS_H

#include "almanac.h"
#include "ql/time/businessdayconvention.hpp"
#include "ql/time/timeunit.hpp"

QuantLib::BusinessDayConvention as_business_day_convention(const std::string& convention);
QuantLib::TimeUnit as_time_unit(const std::string &unit);
QuantLib::Weekday as_weekday(const int& weekday);

#endif
