#' Add and remove holidays
#'
#' @description
#'
#' - Use `holidays_add()` to add new holidays to a calendar. Existing holidays
#'   are ignored. If a holiday you are trying to add has previously been
#'   removed, then it will be re-added.
#'
#' - Use `holiday_remove()` to remove holidays from a calendar. These can be
#'   manually added holidays from using `holidays_add()`, or pre-existing
#'   holidays in that calendar (such as July 4th for a certain year in the
#'   `"united_states"` calendar). Dates that are not holidays are ignored.
#'
#' @export
holidays_add <- function(calendar, holidays) {
  assert_calendar(calendar)
  holidays <- vec_cast(holidays, new_date())

  removed_holidays <- get_removed_holidays(calendar)
  in_removed <- vec_in(holidays, removed_holidays)

  if (any(in_removed)) {
    removed_holidays_to_add <- vec_slice(holidays, in_removed)
    new_removed_holidays <- set_diff(removed_holidays, removed_holidays_to_add)

    calendar <- set_removed_holidays(calendar, new_removed_holidays)

    holidays <- vec_slice(holidays, !in_removed)
  }

  # Only add to the holiday list if it is not already a holiday
  is_holiday <- cal_is_holiday(holidays, calendar)
  holidays <- holidays[!is_holiday]

  holidays <- set_union(get_added_holidays(calendar), holidays)
  holidays <- vec_sort(holidays)

  calendar <- set_added_holidays(calendar, holidays)

  calendar
}

#' @rdname holidays_add
#' @export
holidays_remove <- function(calendar, holidays) {
  assert_calendar(calendar)
  holidays <- vec_cast(holidays, new_date())

  added_holidays <- get_added_holidays(calendar)
  in_added <- vec_in(holidays, added_holidays)

  if (any(in_added)) {
    added_holidays_to_remove <- vec_slice(holidays, in_added)
    new_added_holidays <- set_diff(added_holidays, added_holidays_to_remove)

    calendar <- set_added_holidays(calendar, new_added_holidays)

    holidays <- vec_slice(holidays, !in_added)
  }

  # Only add to the remove list if it is actually a holiday
  is_holiday <- cal_is_holiday(holidays, calendar)
  holidays <- holidays[is_holiday]

  holidays <- set_union(get_removed_holidays(calendar), holidays)
  holidays <- vec_sort(holidays)

  calendar <- set_removed_holidays(calendar, holidays)

  calendar
}

#' @export
holidays_added <- function(calendar) {
  assert_calendar(calendar)
  get_added_holidays(calendar)
}

#' @export
holidays_removed <- function(calendar) {
  assert_calendar(calendar)
  get_removed_holidays(calendar)
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
