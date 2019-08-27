#' @export
cal_adjust <- function(x, convention = conventions$following, calendar = default_calendar()) {
  x <- vec_cast(x, new_date())
  vec_assert(convention, character(), 1L)
  assert_calendar(calendar)
  calendar_adjust(x, convention, calendar)
}

#' @export
cal_adjust_end_of_month <- function(x, calendar = default_calendar()) {
  x <- vec_cast(x, new_date())
  assert_calendar(calendar)
  calendar_adjust_end_of_month(x, calendar)
}

#' @export
cal_shift <- function(x,
                      by = 1L,
                      unit = "day",
                      convention = conventions$following,
                      end_of_month = FALSE,
                      calendar = default_calendar()) {
  x <- vec_cast(x, new_date())
  by <- vec_cast(by, integer())
  arg_match(unit, time_units())
  vec_assert(unit, character(), 1L)
  vec_assert(convention, character(), 1L)
  vec_assert(end_of_month, logical(), 1L)
  assert_calendar(calendar)

  args <- vec_recycle_common(x, by)
  x <- args[[1L]]
  by <- args[[2L]]

  calendar_shift(x, by, unit, convention, end_of_month, calendar)
}

#' @export
cal_count_business_days_between <- function(starts, stops, calendar = default_calendar()) {
  starts <- vec_cast(starts, new_date())
  stops <- vec_cast(stops, new_date())
  assert_calendar(calendar)

  args <- vec_recycle_common(starts, stops)
  starts <- args[[1L]]
  stops <- args[[2L]]

  calendar_count_business_days_between(starts, stops, calendar)
}

#' @export
cal_is_weekend <- function(x, calendar = default_calendar()) {
  x <- vec_cast(x, new_date())
  assert_calendar(calendar)
  calendar_is_weekend(x, calendar)
}

#' @export
cal_is_business_day <- function(x, calendar = default_calendar()) {
  x <- vec_cast(x, new_date())
  assert_calendar(calendar)
  calendar_is_business_day(x, calendar)
}

#' @export
cal_is_holiday <- function(x, calendar = default_calendar()) {
  x <- vec_cast(x, new_date())
  assert_calendar(calendar)
  calendar_is_holiday(x, calendar)
}

#' @export
cal_is_end_of_month <- function(x, calendar = default_calendar()) {
  x <- vec_cast(x, new_date())
  assert_calendar(calendar)
  calendar_is_end_of_month(x, calendar)
}

# ------------------------------------------------------------------------------

time_units <- function() {
  c("day", "month", "year", "week")
}
