#ifndef CALENDARRR2_H
#define CALENDARRR2_H

#include <Rcpp.h>
#include "ql/time/date.hpp"
#include "ql/time/calendar.hpp"

// -----------------------------------------------------------------------------
// Coercion

std::vector<QuantLib::Date> as_quantlib_date(const Rcpp::DateVector dates);
Rcpp::DateVector as_r_date(const std::vector<QuantLib::Date> dates);

// -----------------------------------------------------------------------------

QuantLib::Calendar new_calendar(const Rcpp::List& calendar);
void reset_calendar(QuantLib::Calendar calendar);

#endif
