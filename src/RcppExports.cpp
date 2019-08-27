// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// calendar_adjust
Rcpp::DateVector calendar_adjust(const Rcpp::DateVector x, const std::string& convention, const Rcpp::List& calendar);
RcppExport SEXP _almanac_calendar_adjust(SEXP xSEXP, SEXP conventionSEXP, SEXP calendarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type convention(conventionSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type calendar(calendarSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_adjust(x, convention, calendar));
    return rcpp_result_gen;
END_RCPP
}
// calendar_adjust_end_of_month
Rcpp::DateVector calendar_adjust_end_of_month(const Rcpp::DateVector x, const Rcpp::List& calendar);
RcppExport SEXP _almanac_calendar_adjust_end_of_month(SEXP xSEXP, SEXP calendarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type calendar(calendarSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_adjust_end_of_month(x, calendar));
    return rcpp_result_gen;
END_RCPP
}
// calendar_advance
Rcpp::DateVector calendar_advance(const Rcpp::DateVector x, const Rcpp::IntegerVector by, const std::string& unit, const std::string& convention, const bool& end_of_month, const Rcpp::List& calendar);
RcppExport SEXP _almanac_calendar_advance(SEXP xSEXP, SEXP bySEXP, SEXP unitSEXP, SEXP conventionSEXP, SEXP end_of_monthSEXP, SEXP calendarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type by(bySEXP);
    Rcpp::traits::input_parameter< const std::string& >::type unit(unitSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type convention(conventionSEXP);
    Rcpp::traits::input_parameter< const bool& >::type end_of_month(end_of_monthSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type calendar(calendarSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_advance(x, by, unit, convention, end_of_month, calendar));
    return rcpp_result_gen;
END_RCPP
}
// calendar_detect_holidays
Rcpp::DateVector calendar_detect_holidays(const Rcpp::DateVector start, const Rcpp::DateVector stop, const bool weekends, const Rcpp::List& calendar);
RcppExport SEXP _almanac_calendar_detect_holidays(SEXP startSEXP, SEXP stopSEXP, SEXP weekendsSEXP, SEXP calendarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type stop(stopSEXP);
    Rcpp::traits::input_parameter< const bool >::type weekends(weekendsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type calendar(calendarSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_detect_holidays(start, stop, weekends, calendar));
    return rcpp_result_gen;
END_RCPP
}
// calendar_count_business_days_between
Rcpp::IntegerVector calendar_count_business_days_between(const Rcpp::DateVector starts, const Rcpp::DateVector stops, const Rcpp::List& calendar);
RcppExport SEXP _almanac_calendar_count_business_days_between(SEXP startsSEXP, SEXP stopsSEXP, SEXP calendarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type starts(startsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type stops(stopsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type calendar(calendarSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_count_business_days_between(starts, stops, calendar));
    return rcpp_result_gen;
END_RCPP
}
// calendar_is_weekend
Rcpp::LogicalVector calendar_is_weekend(const Rcpp::DateVector x, const Rcpp::List& calendar);
RcppExport SEXP _almanac_calendar_is_weekend(SEXP xSEXP, SEXP calendarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type calendar(calendarSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_is_weekend(x, calendar));
    return rcpp_result_gen;
END_RCPP
}
// calendar_is_business_day
Rcpp::LogicalVector calendar_is_business_day(const Rcpp::DateVector x, const Rcpp::List& calendar);
RcppExport SEXP _almanac_calendar_is_business_day(SEXP xSEXP, SEXP calendarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type calendar(calendarSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_is_business_day(x, calendar));
    return rcpp_result_gen;
END_RCPP
}
// calendar_is_holiday
Rcpp::LogicalVector calendar_is_holiday(const Rcpp::DateVector x, const Rcpp::List& calendar);
RcppExport SEXP _almanac_calendar_is_holiday(SEXP xSEXP, SEXP calendarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type calendar(calendarSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_is_holiday(x, calendar));
    return rcpp_result_gen;
END_RCPP
}
// calendar_is_end_of_month
Rcpp::LogicalVector calendar_is_end_of_month(const Rcpp::DateVector x, const Rcpp::List& calendar);
RcppExport SEXP _almanac_calendar_is_end_of_month(SEXP xSEXP, SEXP calendarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type calendar(calendarSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_is_end_of_month(x, calendar));
    return rcpp_result_gen;
END_RCPP
}
// calendar_seq
Rcpp::DateVector calendar_seq(const Rcpp::DateVector start, const Rcpp::DateVector stop, const Rcpp::IntegerVector by, const std::string& unit, const std::string& start_convention, const std::string& stop_convention, const bool& end_of_month, const Rcpp::List& calendar);
RcppExport SEXP _almanac_calendar_seq(SEXP startSEXP, SEXP stopSEXP, SEXP bySEXP, SEXP unitSEXP, SEXP start_conventionSEXP, SEXP stop_conventionSEXP, SEXP end_of_monthSEXP, SEXP calendarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< const Rcpp::DateVector >::type stop(stopSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type by(bySEXP);
    Rcpp::traits::input_parameter< const std::string& >::type unit(unitSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type start_convention(start_conventionSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type stop_convention(stop_conventionSEXP);
    Rcpp::traits::input_parameter< const bool& >::type end_of_month(end_of_monthSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type calendar(calendarSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_seq(start, stop, by, unit, start_convention, stop_convention, end_of_month, calendar));
    return rcpp_result_gen;
END_RCPP
}
// calendar_roundtrip_date
Rcpp::DateVector calendar_roundtrip_date(Rcpp::DateVector x);
RcppExport SEXP _almanac_calendar_roundtrip_date(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::DateVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(calendar_roundtrip_date(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_almanac_calendar_adjust", (DL_FUNC) &_almanac_calendar_adjust, 3},
    {"_almanac_calendar_adjust_end_of_month", (DL_FUNC) &_almanac_calendar_adjust_end_of_month, 2},
    {"_almanac_calendar_advance", (DL_FUNC) &_almanac_calendar_advance, 6},
    {"_almanac_calendar_detect_holidays", (DL_FUNC) &_almanac_calendar_detect_holidays, 4},
    {"_almanac_calendar_count_business_days_between", (DL_FUNC) &_almanac_calendar_count_business_days_between, 3},
    {"_almanac_calendar_is_weekend", (DL_FUNC) &_almanac_calendar_is_weekend, 2},
    {"_almanac_calendar_is_business_day", (DL_FUNC) &_almanac_calendar_is_business_day, 2},
    {"_almanac_calendar_is_holiday", (DL_FUNC) &_almanac_calendar_is_holiday, 2},
    {"_almanac_calendar_is_end_of_month", (DL_FUNC) &_almanac_calendar_is_end_of_month, 2},
    {"_almanac_calendar_seq", (DL_FUNC) &_almanac_calendar_seq, 8},
    {"_almanac_calendar_roundtrip_date", (DL_FUNC) &_almanac_calendar_roundtrip_date, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_almanac(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
