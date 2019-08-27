#' @export
cal_adjust <- function(x, convention = conventions$following, calendar = default_calendar()) {
  vec_assert(x, ptype = new_date())
  vec_assert(convention, character(), 1L)
  assert_calendar(calendar)
  calendar_adjust(x, convention, calendar)
}

#' @export
cal_adjust_end_of_month <- function(x, calendar = default_calendar()) {
  vec_assert(x, ptype = new_date())
  assert_calendar(calendar)
  calendar_adjust_end_of_month(x, calendar)
}

#' @export
cal_advance <- function(x,
                        by = 1L,
                        unit = "day",
                        convention = conventions$following,
                        end_of_month = FALSE,
                        calendar = default_calendar()) {
  vec_assert(x, ptype = new_date())
  by <- vec_cast(by, integer())
  arg_match(unit, time_units())
  vec_assert(unit, character(), 1L)
  vec_assert(convention, character(), 1L)
  vec_assert(end_of_month, logical(), 1L)
  assert_calendar(calendar)

  args <- vec_recycle_common(x, by)
  x <- args[[1L]]
  by <- args[[2L]]

  calendar_advance(x, by, unit, convention, end_of_month, calendar)
}

time_units <- function() {
  c("day", "month", "year", "week")
}

#' @export
cal_detect_holidays <- function(start, stop, weekends = FALSE, calendar = default_calendar()) {
  vec_assert(start, ptype = new_date(), size = 1L)
  vec_assert(stop, ptype = new_date(), size = 1L)
  vec_assert(weekends, ptype = logical(), size = 1L)
  assert_calendar(calendar)
  calendar_detect_holidays(start, stop, weekends, calendar)
}

#' @export
cal_count_business_days_between <- function(starts, stops, calendar = default_calendar()) {
  vec_assert(starts, ptype = new_date())
  vec_assert(stops, ptype = new_date())
  assert_calendar(calendar)

  args <- vec_recycle_common(starts, stops)
  starts <- args[[1L]]
  stops <- args[[2L]]

  calendar_count_business_days_between(starts, stops, calendar)
}

#' @export
cal_is_weekend <- function(x, calendar = default_calendar()) {
  vec_assert(x, ptype = new_date())
  assert_calendar(calendar)
  calendar_is_weekend(x, calendar)
}

#' @export
cal_is_business_day <- function(x, calendar = default_calendar()) {
  vec_assert(x, ptype = new_date())
  assert_calendar(calendar)
  calendar_is_business_day(x, calendar)
}

#' @export
cal_is_holiday <- function(x, calendar = default_calendar()) {
  vec_assert(x, ptype = new_date())
  assert_calendar(calendar)
  calendar_is_holiday(x, calendar)
}

#' @export
cal_is_end_of_month <- function(x, calendar = default_calendar()) {
  vec_assert(x, ptype = new_date())
  assert_calendar(calendar)
  calendar_is_end_of_month(x, calendar)
}
