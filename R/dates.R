#' @export
cal_adjust <- function(x, convention = conventions$following, calendar = calendar()) {
  x <- vec_cast_date(x)
  vec_assert(convention, character(), 1L)
  assert_calendar(calendar)
  calendar_adjust(x, convention, calendar)
}

#' @export
cal_adjust_end_of_month <- function(x, calendar = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(calendar)
  calendar_adjust_end_of_month(x, calendar)
}

#' @export
cal_count_business_days_between <- function(starts, stops, calendar = calendar()) {
  starts <- vec_cast_date(starts)
  stops <- vec_cast_date(stops)
  assert_calendar(calendar)

  args <- vec_recycle_common(starts, stops)
  starts <- args[[1L]]
  stops <- args[[2L]]

  calendar_count_business_days_between(starts, stops, calendar)
}

#' @export
cal_is_weekend <- function(x, calendar = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(calendar)
  calendar_is_weekend(x, calendar)
}

#' @export
cal_is_business_day <- function(x, calendar = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(calendar)
  calendar_is_business_day(x, calendar)
}

#' @export
cal_is_holiday <- function(x, calendar = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(calendar)
  calendar_is_holiday(x, calendar)
}

#' @export
cal_is_end_of_month <- function(x, calendar = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(calendar)
  calendar_is_end_of_month(x, calendar)
}

# ------------------------------------------------------------------------------

time_units <- function() {
  c("day", "month", "year", "week")
}
