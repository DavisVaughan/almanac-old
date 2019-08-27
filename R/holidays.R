#' @export
cal_add_holidays <- function(calendar, holidays) {
  assert_calendar(calendar)
  vec_assert(holidays, new_date())

  holidays <- set_union(get_holidays(calendar), holidays)
  holidays <- vec_sort(holidays)

  calendar <- set_holidays(calendar, holidays)

  calendar
}

#' @export
cal_remove_holidays <- function(calendar, holidays) {
  assert_calendar(calendar)
  vec_assert(holidays, new_date())

  holidays <- set_diff(get_holidays(calendar), holidays)

  calendar <- set_holidays(calendar, holidays)

  calendar
}

#' @export
cal_holidays <- function(calendar) {
  assert_calendar(calendar)
  get_holidays(calendar)
}

# ------------------------------------------------------------------------------

set_union <- function(x, y) {
  vec_unique(vec_c(x, y))
}

set_diff <- function(x, y) {
  x_in_y <- vec_in(x, y)
  vec_slice(x, !x_in_y)
}
