#' @export
cal_shift <- function(x,
                      period = "1 day",
                      convention = conventions$following,
                      end_of_month = FALSE,
                      calendar = default_calendar()) {
  x <- vec_cast(x, new_date())
  vec_assert(convention, character(), 1L)
  vec_assert(end_of_month, logical(), 1L)
  assert_calendar(calendar)

  period <- parse_period(period)

  calendar_shift(x, period, convention, end_of_month, calendar)
}

parse_period <- function(x) {
  if (!lubridate::is.period(x) && !is.character(x)) {
    abort("`period` must be a character, or a lubridate Period object.")
  }

  vec_assert(x, size = 1L)

  if (is.character(x)) {
    x <- lubridate::period(x)
  }

  sub_daily <- sum(lubridate::hour(x), lubridate::minute(x), lubridate::second(x))
  if (sub_daily != 0L) {
    abort("`period` can only use year, month, week, and day periods.")
  }

  list(
    year = lubridate::year(x),
    month = lubridate::month(x),
    day = lubridate::day(x)
  )
}
