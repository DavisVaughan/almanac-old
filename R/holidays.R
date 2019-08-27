#' @export
cal_add_holidays <- function(calendar, holidays) {
  assert_calendar(calendar)
  vec_assert(holidays, new_date())

  holidays <- vec_union(get_holidays(calendar), holidays)
  holidays <- vec_sort(holidays)

  calendar <- set_holidays(calendar, holidays)

  calendar
}

#' @export
cal_remove_holidays <- function(calendar, holidays) {
  assert_calendar(calendar)
  vec_assert(holidays, new_date())

  holidays <- vec_set_diff(get_holidays(calendar), holidays)

  calendar <- set_holidays(calendar, holidays)

  calendar
}

#' @export
cal_holidays <- function(calendar) {
  assert_calendar(calendar)
  get_holidays(calendar)
}

# ------------------------------------------------------------------------------

vec_union <- function(x, y) {
  vec_unique(vec_c(x, y))
}

vec_set_diff <- function(x, y) {
  match <- vec_match(y, x)
  match <- match[!is.na(match)]
  vec_slice(x, setdiff(vec_seq_along(x), match))
}
