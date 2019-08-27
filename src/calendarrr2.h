#ifndef CALENDARRR2_H
#define CALENDARRR2_H

#include <Rcpp.h>
#include "ql/time/date.hpp"

// -----------------------------------------------------------------------------
// Coercion

std::vector<QuantLib::Date> as_quantlib_date(Rcpp::DateVector dates);

Rcpp::DateVector as_r_date(std::vector<QuantLib::Date> dates);

// -----------------------------------------------------------------------------

#endif
