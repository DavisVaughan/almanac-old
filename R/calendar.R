#' @export
calendar <- function(name = calendars$united_states) {
  vec_assert(name, ptype = character(), size = 1L)
  validate_calendar_name(name)
  new_calendar(name, holidays = new_date())
}

#' @export
default_calendar <- function() {
  if (is.null(calendar_env$calendar)) {
    calendar()
  } else {
    calendar_env$calendar
  }
}

#' @export
set_default_calendar <- function(x) {
  if (!is_calendar(x)) {
    abort(glue::glue("`x` must be a 'calendar' object, not a '{class(x)[1]}'."))
  }

  calendar_env$calendar <- x

  invisible(x)
}

calendar_env <- new.env(parent = emptyenv())

#' @export
print.calendar <- function(x, ...) {
  if(has_holidays(x)) {
    header <- "Custom holidays: \n"
    holidays <- get_holidays(x)

    if (vec_size(holidays) > 5L) {
      holidays <- holidays[1:5]
      holidays <- paste0("   - ", holidays, "\n")
      holidays <- c(holidays, "   - ...\n")
    } else {
      holidays <- paste0("   - ", holidays, "\n")
    }

    holidays <- c(header, holidays)
  } else {
    holidays <- ""
  }

  # Print
  cat(
    "<Calendar: ", get_name(x), ">", "\n",
    holidays,
    sep = ""
  )
}

new_calendar <- function(name = calendars$united_states, holidays = new_date()) {
  if (!is.character(name)) {
    abort("`name` must be a character vector.")
  }

  if (!vec_is(holidays, new_date())) {
    abort("`holidays` must be a Date vector.")
  }

  structure(
    list(
      name = name,
      holidays = holidays
    ),
    class = "calendar"
  )
}

# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------

is_calendar <- function(x) {
  inherits(x, "calendar")
}

has_holidays <- function(x) {
  !identical(get_holidays(x), new_date())
}

get_holidays <- function(x) {
  x[["holidays"]]
}

set_holidays <- function(x, holidays) {
  x[["holidays"]] <- holidays
  x
}

get_name <- function(x) {
  x[["name"]]
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
