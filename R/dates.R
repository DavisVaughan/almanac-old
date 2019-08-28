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

#' @export
cal_count_business_days_between <- function(starts, stops, cal = calendar()) {
  starts <- vec_cast_date(starts)
  stops <- vec_cast_date(stops)
  assert_calendar(cal)

  args <- vec_recycle_common(starts, stops)
  starts <- args[[1L]]
  stops <- args[[2L]]

  calendar_count_business_days_between(starts, stops, cal)
}

#' @export
cal_is_weekend <- function(x, cal = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(cal)
  calendar_is_weekend(x, cal)
}

#' @export
cal_is_business_day <- function(x, cal = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(cal)
  calendar_is_business_day(x, cal)
}

#' @export
cal_is_holiday <- function(x, cal = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(cal)
  calendar_is_holiday(x, cal)
}

#' @export
cal_is_end_of_month <- function(x, cal = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(cal)
  calendar_is_end_of_month(x, cal)
}

# ------------------------------------------------------------------------------

time_units <- function() {
  c("day", "month", "year", "week")
}
