#ifndef ALMANAC_H
#define ALMANAC_H

#include <Rcpp.h>
#include "ql/time/date.hpp"
#include "ql/time/calendar.hpp"

// -----------------------------------------------------------------------------
// Coercion

QuantLib::Date as_quantlib_date(const Rcpp::Date& date);
Rcpp::Date as_r_date(const QuantLib::Date& date);

// -----------------------------------------------------------------------------

QuantLib::Calendar new_calendar(const Rcpp::List& calendar);
void reset_calendar(QuantLib::Calendar calendar);

#endif
