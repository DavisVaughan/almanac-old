#' @export
holidays_add <- function(calendar, holidays) {
  assert_calendar(calendar)
  holidays <- vec_cast(holidays, new_date())

  holidays <- set_union(get_holidays(calendar), holidays)
  holidays <- vec_sort(holidays)

  calendar <- set_holidays(calendar, holidays)

  calendar
}

#' @export
holidays_remove <- function(calendar, holidays) {
  assert_calendar(calendar)
  holidays <- vec_cast(holidays, new_date())

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
holidays_all <- function(calendar, weekends = FALSE) {
  holidays_between(beginning_of_time(), end_of_time(), weekends = weekends, calendar = calendar)
}

#' @export
holidays_all_calendars <- function() {
  names_of_calendars <- names(calendars)
  list_of_calendars <- lapply(names_of_calendars, calendar)
  list_of_holidays <- lapply(list_of_calendars, holidays_all)

  holiday_df <- new_data_frame(list(
    calendar = names_of_calendars,
    holidays = as_list_of(list_of_holidays)
  ))

  if (is_installed("tibble")) {
    holiday_df <- tibble::as_tibble(holiday_df)
  }

  holiday_df
}

#' @export
holidays_between <- function(start, stop, weekends = FALSE, calendar = default_calendar()) {
  start <- vec_cast(start, new_date())
  stop <- vec_cast(stop, new_date())
  vec_assert(start, size = 1L)
  vec_assert(stop, size = 1L)
  assert_start_before_stop(start, stop)
  vec_assert(weekends, ptype = logical(), size = 1L)
  assert_calendar(calendar)
  calendar_holidays_between(start, stop, weekends, calendar)
}

assert_start_before_stop <- function(start, stop) {
  if (vec_compare(start, stop) >= 0L) {
    glubort("`start` ({start}) must be strictly less than `stop` ({stop}).")
  }
  invisible()
}

# ------------------------------------------------------------------------------

glubort <- function (..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

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
