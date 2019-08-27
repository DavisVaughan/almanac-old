#' @export
holidays_add <- function(calendar, holidays) {
  assert_calendar(calendar)
  vec_assert(holidays, new_date())

  holidays <- set_union(get_holidays(calendar), holidays)
  holidays <- vec_sort(holidays)

  calendar <- set_holidays(calendar, holidays)

  calendar
}

#' @export
holidays_remove <- function(calendar, holidays) {
  assert_calendar(calendar)
  vec_assert(holidays, new_date())

  holidays <- set_diff(get_holidays(calendar), holidays)

  calendar <- set_holidays(calendar, holidays)

  calendar
}

#' @export
holidays_custom <- function(calendar) {
  assert_calendar(calendar)
  get_holidays(calendar)
}

#' @export
holidays_all <- function(calendar) {
  holidays_between(beginning_of_time(), end_of_time(), calendar = calendar)
}

#' @export
holidays_all_calendars <- function() {
  names_of_calendars <- names(calendars)
  list_of_calendars <- lapply(names_of_calendars, calendar)
  list_of_holidays <- lapply(list_of_calendars, holidays_all)

  holiday_df <- new_data_frame(list(
    calendar = names_of_calendars,
    holidays = list_of_holidays
  ))

  if (is_installed("tibble")) {
    holiday_df <- tibble::as_tibble(holiday_df)
  }

  holiday_df
}

#' @export
holidays_between <- function(start, stop, weekends = FALSE, calendar = default_calendar()) {
  vec_assert(start, ptype = new_date(), size = 1L)
  vec_assert(stop, ptype = new_date(), size = 1L)
  vec_assert(weekends, ptype = logical(), size = 1L)
  assert_calendar(calendar)
  calendar_holidays_between(start, stop, weekends, calendar)
}

# ------------------------------------------------------------------------------

beginning_of_time <- function() {
  as.Date("1901-01-01")
}

end_of_time <- function() {
  as.Date("2199-12-30")
}

set_union <- function(x, y) {
  vec_unique(vec_c(x, y))
}

set_diff <- function(x, y) {
  x_in_y <- vec_in(x, y)
  vec_slice(x, !x_in_y)
}
