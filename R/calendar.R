#' Construct a calendar
#'
#' @description
#'
#' These are various functions revolving around creating calendar objects, and
#' setting the default calendar.
#'
#' - `calendar()` constructs a new calendar object. There are a large
#'   assortment of existing calendars to pick from, each with their own
#'   pre-existing holiday list.
#'
#' - `empty_calendar()` constructs a special calendar that has no predefined
#'   holidays, and no predefined business week. You can set the weekends on
#'   creation.
#'
#' - `calendars` lists the set of possible calendars that can be used.
#'
#' @param name `[character(1)]`
#'
#'   The base calendar to create. This should be one of the options listed in
#'   `calendars`.
#'
#' @param weekends `[character]`
#'
#'   Weekdays that should be special-cased as weekends. Weekends are considered
#'   "holidays," and are skipped over when shifting dates forward in the
#'   business week. Choose from:
#'
#'   - `"Sunday"`
#'   - `"Monday"`
#'   - `"Tuesday"`
#'   - `"Wednesday"`
#'   - `"Thursday"`
#'   - `"Friday"`
#'   - `"Saturday"`
#'
#' @return
#'
#' - `calendar()` returns a new calendar.
#'
#' - `empty_calendar()` returns a new empty calendar.
#'
#' - `calendars` returns a named `list_of<character>`.
#'
#' @examples
#' calendar()
#'
#' calendar(calendars$argentina)
#'
#' empty_calendar()
#'
#' empty_calendar(weekends = "Monday")
#'
#' # All available calendars
#' calendars
#'
#' @export
calendar <- function(name = calendars$united_states) {
  vec_assert(name, ptype = character(), size = 1L)
  validate_calendar_name(name)
  new_calendar(name = name)
}

#' @export
print.calendar <- function(x, ...) {
  if(has_added_holidays(x)) {
    header <- "Added holidays: \n"
    added_holidays <- get_added_holidays(x)
    added_holidays <- bullet_list(added_holidays)
    added_holidays <- c("\n", header, added_holidays)
  } else {
    added_holidays <- ""
  }

  if(has_removed_holidays(x)) {
    header <- "Removed holidays: \n"
    removed_holidays <- get_removed_holidays(x)
    removed_holidays <- bullet_list(removed_holidays)
    removed_holidays <- c("\n", header, removed_holidays)
  } else {
    removed_holidays <- ""
  }

  cat(
    get_header(x),
    added_holidays,
    removed_holidays,
    sep = ""
  )
}

new_calendar <- function(name = calendars$united_states,
                         added_holidays = new_date(),
                         removed_holidays = new_date(),
                         ...,
                         subclass = character()) {
  if (!is.character(name)) {
    abort("`name` must be a character vector.")
  }

  if (!vec_is(added_holidays, new_date())) {
    abort("`added_holidays` must be a Date vector.")
  }

  if (!vec_is(removed_holidays, new_date())) {
    abort("`removed_holidays` must be a Date vector.")
  }

  structure(
    list(
      name = name,
      added_holidays = added_holidays,
      removed_holidays = removed_holidays,
      ...
    ),
    class = c(subclass, "calendar")
  )
}

#' @rdname calendar
#' @export
empty_calendar <- function(weekends = c("Saturday", "Sunday")) {
  weekends <- check_weekends(weekends)
  new_empty_calendar(weekends = weekends)
}

#' @export
print.empty_calendar <- function(x, ...) {
  NextMethod()

  if(has_weekends(x)) {
    header <- "Weekends: \n"
    weekends <- get_weekends(x)
    weekdays <- seq_along(weekday())

    loc_in_weekdays <- vec_match(weekends, weekdays)
    weekends <- weekday()[loc_in_weekdays]
    weekends <- capitalize(weekends)

    weekends <- paste("-", weekends, "\n")

    weekends <- c(header, weekends)
  } else {
    weekends <- ""
  }

  cat("\n")
  cat(weekends)
}

new_empty_calendar <- function(added_holidays = new_date(),
                                removed_holidays = new_date(),
                                weekends = c(weekday$saturday, weekday$sunday)) {
  if (!is.integer(weekends)) {
    abort("`weekends` must be an integer vector.")
  }

  new_calendar(
    name = "empty",
    added_holidays = added_holidays,
    removed_holidays = removed_holidays,
    weekends = weekends,
    subclass = "empty_calendar"
  )
}

# ------------------------------------------------------------------------------

#' @rdname calendar
#' @export
calendars <- vctrs::list_of(
  argentina = "argentina",
  argentina_merval = "argentina_merval",

  united_states = "united_states",
  united_states_settlement = "united_states_settlement"
)

#' Business day conventions
#'
#' @description
#'
#' The following business day conventions are used throughout the package. They
#' are generally applied when you land on a holiday or weekend, and have to
#' decide which direction to shift to move to the next date.
#'
#'   - `"following"`
#'
#'     Choose the first business day after the given holiday.
#'
#'   - `"modified_following"`
#'
#'     Choose the first business day after the given
#'     holiday unless it belongs to a different month, in which case choose the
#'     first business day before the holiday.
#'
#'   - `"preceding"`
#'
#'     Choose the first business day before the given holiday.
#'
#'   - `"modified_preceding"`
#'
#'     Choose the first business day before the given holiday unless it belongs
#'     to a different month, in which case choose the first business day after
#'     the holiday.
#'
#'   - `"unadjusted"`
#'
#'     No adjustment is made.
#'
#'   - `"half_month_modified_following"`
#'
#'     Choose the first business day after the given holiday unless that day
#'     crosses the mid-month (15th) or the end of month, in which case choose
#'     the first business day before the holiday.
#'
#'   - `"nearest"`
#'
#'     Choose the nearest business day to the given holiday. If both the
#'     preceding and following business days are equally far away, default to
#'     following business day.
#'
#' @export
conventions <- vctrs::list_of(
  following = "following",
  modified_following = "modified_following",
  preceding = "preceding",
  modified_preceding = "modified_preceding",
  unadjusted = "unadjusted",
  half_month_modified_following = "half_month_modified_following",
  nearest = "nearest"
)

# ------------------------------------------------------------------------------

is_calendar <- function(x) {
  inherits(x, "calendar")
}

has_added_holidays <- function(x) {
  !identical(get_added_holidays(x), new_date())
}

has_removed_holidays <- function(x) {
  !identical(get_removed_holidays(x), new_date())
}

has_weekends <- function(x) {
  !identical(get_weekends(x), integer())
}

get_weekends <- function(x) {
  x[["weekends"]]
}

get_added_holidays <- function(x) {
  x[["added_holidays"]]
}

get_removed_holidays <- function(x) {
  x[["removed_holidays"]]
}

set_added_holidays <- function(x, holidays) {
  x[["added_holidays"]] <- holidays
  x
}

set_removed_holidays <- function(x, holidays) {
  x[["removed_holidays"]] <- holidays
  x
}

get_name <- function(x) {
  x[["name"]]
}

get_header <- function(x) {
  if (inherits(x, "empty_calendar")) {
    "<Calendar>"
  } else {
    paste0("<Calendar: ", get_name(x), ">")
  }
}

validate_calendar_name <- function(name) {
  is_valid <- name %in% names(calendars)

  if(!is_valid) {
    msg <- glue::glue("`name` must be a calendar name from `calendars`, not '{name}'.")
    abort(msg)
  }

  invisible(name)
}

assert_calendar <- function(x) {
  if (!is_calendar(x)) {
    msg <- glue::glue("`calendar` must be a 'calendar', not a '{class(x)[1]}'.")
    abort(msg)
  }

  invisible(x)
}

check_weekends <- function(weekends) {
  vec_assert(weekends, character())
  weekends <- vec_unique(weekends)

  weekdays <- weekday()

  loc_in_weekdays <- vec_match(weekends, weekdays)

  if (anyNA(loc_in_weekdays)) {
    bad_weekends <- weekends[is.na(loc_in_weekdays)]
    bad_weekends <- quote_collapse(bad_weekends)
    weekdays <- quote_collapse(weekdays, last = " or ")
    glubort("`weekends` can only contain {weekdays}, not {bad_weekends}.")
  }

  loc_in_weekdays <- vec_sort(loc_in_weekdays)

  loc_in_weekdays
}

quote_collapse <- function(x, last = "") {
  glue::glue_collapse(glue::single_quote(x), ", ", last = last)
}

weekday <- function() {
 c(
   "Sunday",
   "Monday",
   "Tuesday",
   "Wednesday",
   "Thursday",
   "Friday",
   "Saturday"
 )
}

capitalize <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

bullet_list <- function(x) {
  if (vec_size(x) > 5L) {
    x <- x[1:5]
    x <- paste(" -", x, collapse = "\n")
    x <- c(x, "\n - ...")
  } else {
    x <- paste(" -", x, collapse = "\n")
  }

  x
}
