#' Add and remove holidays
#'
#' @description
#'
#' - `holidays_add()` adds new holidays to a calendar. Existing holidays
#'   are ignored. If a holiday you are trying to add has previously been
#'   removed, then it will be re-added.
#'
#' - `holidays_remove()` remove holidays from a calendar. These can be
#'   manually added holidays from using `holidays_add()`, or pre-existing
#'   holidays in that calendar (such as July 4th for a certain year in the
#'   `"united_states"` calendar). Dates that are not holidays are ignored.
#'
#' - `holidays_added()` lists the manually added holidays in a calendar.
#'
#' - `holidays_removed()` lists the manually removed holidays in a calendar.
#'
#' @param calendar `[calendar]`
#'
#'   A calendar.
#'
#' @param holidays `[Date]`
#'
#'   A vector of dates to add or remove from a calendar.
#'
#' @examples
#' cal <- calendar()
#'
#' date <- as.Date("2019-01-03")
#'
#' # Notice that this day is not a holiday
#' cal_is_holiday(date, cal)
#' cal_shift("2019-01-02", "1 day", calendar = cal)
#'
#' # Now let's add it as a holiday for this calendar
#' cal_with_holiday <- holidays_add(cal, date)
#' cal_with_holiday
#'
#' # Now it registers as a holiday
#' cal_is_holiday(date, cal_with_holiday)
#' cal_shift("2019-01-02", "1 day", calendar = cal_with_holiday)
#'
#' # You can remove it afterwards
#' cal_back_to_normal <- holidays_remove(cal_with_holiday, date)
#' cal_back_to_normal
#'
#' # Dates that are already holidays are ignored if you try to add them
#' # This is New Years
#' cal_is_holiday("2019-01-01", cal)
#' holidays_add(cal, "2019-01-01")
#'
#' # If you don't want New Years to be a holiday in the US calendar, you
#' # can remove it
#' cal_no_new_years <- holidays_remove(cal, "2019-01-01")
#' cal_no_new_years
#'
#' # Which means that this is an allowed date when shifting
#' cal_shift("2018-12-31", "1 day", calendar = cal)
#' cal_shift("2018-12-31", "1 day", calendar = cal_no_new_years)
#'
#' # List the added holidays
#' holidays_added(cal_with_holiday)
#'
#' # List the removed holidays
#' holidays_removed(cal_no_new_years)
#'
#' @export
holidays_add <- function(calendar, holidays) {
  assert_calendar(calendar)
  holidays <- vec_cast_date(holidays)

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
  holidays <- vec_cast_date(holidays)

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

#' @rdname holidays_add
#' @export
holidays_added <- function(calendar) {
  assert_calendar(calendar)
  get_added_holidays(calendar)
}

#' @rdname holidays_add
#' @export
holidays_removed <- function(calendar) {
  assert_calendar(calendar)
  get_removed_holidays(calendar)
}

# ------------------------------------------------------------------------------

#' Find holidays
#'
#' @description
#'
#' These functions help with locating _all_ holidays for a specific calendar.
#' They locate both pre-existing and manually added holidays.
#'
#' - `holidays_all()` lists every holiday in `calendar` from the minimum date
#'   (1901-01-01) to the maximum date (2199-12-30).
#'
#' - `holidays_all_calendars()` maps `holidays_all()` over all calendars
#'   available in [calendars], and returns a data frame.
#'
#' - `holidays_between()` locates holidays betwen two dates.
#'
#' @param calendar `[calendar]`
#'
#'   A calendar.
#'
#' @param weekends `[logical(1)]`
#'
#'   Should weekends be treated as holidays?
#'
#' @param start,stop `[Date(1)]`
#'
#'   Boundary values to look for holidays between. Both are inclusive
#'   boundaries. It is required that `start` is _strictly_ less than `stop`.
#'
#' @return
#'
#' - `holidays_all()` returns a vector of `Date`s.
#'
#' - `holidays_all_calendars()` returns a data frame with a `character`
#'   `"calendar"` column, and a `list_of<date>` `"holidays"` column. If
#'   the tibble package is installed, a tibble will be returned.
#'
#' - `holidays_between()` returns a vector of `Date`s.
#'
#' @examples
#' us_holidays <- holidays_all(calendar())
#'
#' length(us_holidays)
#' us_holidays[1:5]
#'
#' holidays_all_calendars()
#'
#' # Locate holidays between two dates
#' cal <- calendar()
#'
#' holidays_between("2019-01-01", "2019-03-01", calendar = cal)
#'
#' # Manually added holidays are respected
#' cal <- holidays_add(cal, "2019-01-02")
#' holidays_between("2019-01-01", "2019-03-01", calendar = cal)
#'
#' # So are manually removed holidays
#' cal <- holidays_remove(cal, "2019-01-01")
#' holidays_between("2019-01-01", "2019-03-01", calendar = cal)
#'
#' @export
holidays_all <- function(calendar, weekends = FALSE) {
  holidays_between(
    start = beginning_of_time(),
    stop = end_of_time(),
    weekends = weekends,
    calendar = calendar
  )
}

#' @rdname holidays_all
#' @export
holidays_all_calendars <- function(weekends = FALSE) {
  names_of_calendars <- names(calendars)
  list_of_calendars <- lapply(names_of_calendars, calendar)
  list_of_holidays <- lapply(list_of_calendars, holidays_all, weekends = weekends)

  holiday_df <- new_data_frame(list(
    calendar = names_of_calendars,
    holidays = as_list_of(list_of_holidays)
  ))

  if (is_installed("tibble")) {
    holiday_df <- tibble::as_tibble(holiday_df)
  }

  holiday_df
}

#' @rdname holidays_all
#' @export
holidays_between <- function(start,
                             stop,
                             weekends = FALSE,
                             calendar = calendar()) {
  start <- vec_cast_date(start)
  stop <- vec_cast_date(stop)
  vec_assert(start, size = 1L)
  vec_assert(stop, size = 1L)
  assert_start_before_stop(start, stop)
  vec_assert(weekends, ptype = logical(), size = 1L)
  assert_calendar(calendar)
  calendar_holidays_between(start, stop, weekends, calendar)
}

# ------------------------------------------------------------------------------

assert_start_before_stop <- function(start, stop) {
  if (vec_compare(start, stop) >= 0L) {
    glubort("`start` ({start}) must be strictly less than `stop` ({stop}).")
  }
  invisible()
}

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
