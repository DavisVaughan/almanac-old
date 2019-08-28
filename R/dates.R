#' Adjust a date
#'
#' `cal_adjust()` adjusts a sequence of dates according to a business
#' calendar. If a date is currently a holiday or weekend, it is adjusted
#' using the corresponding `convention`.
#'
#' @inheritParams cal_shift
#'
#' @param x `[Date]`
#'
#'   The dates to adjust.
#'
#' @examples
#'
#' # Holidays are adjusted according to the convention
#' cal_adjust("2019-01-01")
#' cal_adjust("2019-01-01", "preceding")
#'
#' # No adjustment is required if we remove the holiday
#' cal <- holidays_remove(calendar(), "2019-01-01")
#' cal_adjust("2019-01-01", cal = cal)
#'
#' @export
cal_adjust <- function(x, convention = conventions$following, cal = calendar()) {
  x <- vec_cast_date(x)
  vec_assert(convention, character(), 1L)
  assert_calendar(cal)
  calendar_adjust(x, convention, cal)
}

#' Count the number of business days
#'
#' `cal_count()` counts the number of business days between `starts` and
#' `stops`. The count is inclusive for `starts`, but not inclusive for `stops`.
#' In mathematical notation, it counts the business days in the range of
#' `[starts, stops)`.
#'
#' @param starts,stops `[Date]`
#'
#'   Vectors of `Date`s determining the boundaries to count business days
#'   between. Recycling is performed using standard tidyverse recycling rules.
#'
#' @param cal `[calendar]`
#'
#'   A calendar.
#'
#' @examples
#'
#' # - 2018-12-31 is a business day (starts are inclusive)
#' # - 2019-01-01 is a holiday
#' # - 2019-01-02 is a business day
#' # - 2019-01-02 is not included (stops are not inclusive)
#' cal_count("2018-12-31", "2019-01-03")
#'
#' # Technically it is allowed for `starts` to be after `stops`
#' cal_count("2019-01-03", "2018-12-31")
#'
#' @export
cal_count <- function(starts, stops, cal = calendar()) {
  starts <- vec_cast_date(starts)
  stops <- vec_cast_date(stops)
  assert_calendar(cal)

  args <- vec_recycle_common(starts, stops)
  starts <- args[[1L]]
  stops <- args[[2L]]

  calendar_count(starts, stops, cal)
}

#' Calendar predicates
#'
#' @description
#'
#' - `cal_is_weekend()` checks if `x` is a weekend.
#'
#' - `cal_is_business_day()` checks if `x` is a business day (i.e., not a
#'   weekend and not a holiday).
#'
#' - `cal_is_holiday()` checks if `x` is a business holiday (but not a weekend).
#'
#' - `cal_is_end_of_month()` checks if `x` is the last business day of
#'   the month.
#'
#' @param x `[Date]`
#'
#'   A vector of dates to check.
#'
#' @param cal `[calendar]`
#'
#'   A calendar.
#'
#' @return
#'
#' A logical vector the same size as `x`.
#'
#' @examples
#' # Not a weekend
#' cal_is_weekend("2019-01-01")
#'
#' # But it is a business holiday!
#' cal_is_holiday("2019-01-01")
#'
#' # This is a weekend
#' cal_is_weekend("2019-01-05")
#'
#' # Neither are business days
#' cal_is_business_day(c("2019-01-01", "2019-01-05"))
#'
#' # You can remove all holidays and weekends with an empty calendar,
#' # which is then respected by `cal_is_holiday()` and `cal_is_weekend()`
#' cal_no_weekends_no_holidays <- empty_calendar(weekends = character())
#' cal_is_holiday("2019-01-01", cal_no_weekends_no_holidays)
#' cal_is_weekend("2019-01-05", cal_no_weekends_no_holidays)
#'
#' # The "end of the month" is relative to the business calendar, meaning
#' # that it is the last business day of the month, not the last day of the
#' # month
#' cal_is_weekend(c("2019-03-29", "2019-03-30", "2019-03-31"))
#' cal_is_end_of_month(c("2019-03-29", "2019-03-30", "2019-03-31"))
#'
#' @name calendar-predicates
#' @export
cal_is_weekend <- function(x, cal = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(cal)
  calendar_is_weekend(x, cal)
}

#' @rdname calendar-predicates
#' @export
cal_is_business_day <- function(x, cal = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(cal)
  calendar_is_business_day(x, cal)
}

#' @rdname calendar-predicates
#' @export
cal_is_holiday <- function(x, cal = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(cal)

  # In quantlib, "holidays" are weekends or holidays, but we only want
  # strict holidays here.
  calendar_is_holiday(x, cal) & !cal_is_weekend(x, cal)
}

#' @rdname calendar-predicates
#' @export
cal_is_end_of_month <- function(x, cal = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(cal)

  # In quantlib, the end of the month is "iff in the given market, the date is
  # on or after the last business day for that month." this is not a bug:
  # https://github.com/lballabio/QuantLib/issues/376
  # but we want to only report the end of the business month
  calendar_is_end_of_month(x, cal) & cal_is_business_day(x, cal)
}

# ------------------------------------------------------------------------------

time_units <- function() {
  c("day", "month", "year", "week")
}
