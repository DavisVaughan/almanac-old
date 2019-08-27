#' Construct a calendar
#'
#' @description
#'
#' These are various functions revolving around creating calendar objects, and
#' setting the default calendar.
#'
#' - `calendar()` constructs a new 'calendar' object.
#'
#' - `default_calendar()` is the current default calendar. The default is just
#'   to use `calendar()`.
#'
#' - `set_default_calendar()` is used to alter the default calendar. This is
#'   set globally for all future calls to `default_calendar()` in the current
#'   R session.
#'
#' - `calendars` lists the set of possible calendars that can be used.
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

bullet_list <- function(x) {
  if (vec_size(x) > 5L) {
    x <- x[1:5]
    x <- paste("   - ", x, collapse = "\n")
    x <- c(x, "\n   - ...")
  } else {
    x <- paste("   - ", x, collapse = "\n")
  }

  x
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
custom_calendar <- function(weekends = c(weekday$saturday, weekday$sunday)) {
  weekends <- vec_cast(weekends, integer())
  weekends <- vec_unique(weekends)
  weekends <- vec_sort(weekends)
  assert_weekends(weekends)
  new_custom_calendar(weekends = weekends)
}

#' @export
print.custom_calendar <- function(x, ...) {
  NextMethod()

  if(has_weekends(x)) {
    header <- "Custom weekends: \n"
    weekends <- get_weekends(x)
    weekdays <- vec_c(!!!weekday)

    loc_in_weekdays <- vec_match(weekends, weekdays)
    weekends <- names(weekdays[loc_in_weekdays])
    weekends <- capitalize(weekends)

    weekends <- paste0("   - ", weekends, "\n")

    weekends <- c(header, weekends)
  } else {
    weekends <- ""
  }

  cat("\n")
  cat(weekends)
}

new_custom_calendar <- function(added_holidays = new_date(),
                                removed_holidays = new_date(),
                                weekends = c(weekday$saturday, weekday$sunday)) {
  if (!is.integer(weekends)) {
    abort("`weekends` must be an integer vector.")
  }

  new_calendar(
    name = "custom",
    added_holidays = added_holidays,
    removed_holidays = removed_holidays,
    weekends = weekends,
    subclass = "custom_calendar"
  )
}

#' @rdname calendar
#' @export
default_calendar <- function() {
  if (is.null(calendar_env$calendar)) {
    calendar()
  } else {
    calendar_env$calendar
  }
}

#' @rdname calendar
#' @export
set_default_calendar <- function(x) {
  assert_calendar(x)

  calendar_env$calendar <- x

  invisible(x)
}

calendar_env <- new.env(parent = emptyenv())

# ------------------------------------------------------------------------------

#' @rdname calendar
#' @export
calendars <- list(
  argentina = "argentina",
  argentina_merval = "argentina_merval",

  united_states = "united_states",
  united_states_settlement = "united_states_settlement"
)

#' @export
conventions <- list(
  following = "following",
  modified_following = "modified_following",
  preceding = "preceding",
  modified_preceding = "modified_preceding",
  unadjusted = "unadjusted",
  half_month_modified_following = "half_month_modified_following",
  nearest = "nearest"
)

#' @export
weekday <- list(
  sunday = 1L,
  monday = 2L,
  tuesday = 3L,
  wednesday = 4L,
  thursday = 5L,
  friday = 6L,
  saturday = 7L
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
  if (inherits(x, "custom_calendar")) {
    "<Custom Calendar>"
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

assert_weekends <- function(x) {
  all_weekdays <- all(x %in% 1:7)

  if (!all_weekdays) {
    msg <- glue::glue("`weekends` must be an integer vector between 1 and 7.")
    abort(msg)
  }

  invisible(x)
}

capitalize <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}
