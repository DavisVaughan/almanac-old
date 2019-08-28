#' Shift dates relative to a business calendar
#'
#' @description
#'
#' - `cal_shift()` shifts a vector of dates by a `period`, while respecting
#'   business calendar rules.
#'
#' - `cal_shift_end_of_month()` shifts a vector of dates to the last day
#'   in the business month.
#'
#' @param x `[Date]`
#'
#'   The dates to shift.
#'
#' @param period `[character(1), lubridate::Period]`
#'
#'   The period to shift `x` with. This is allowed to be a character such as
#'   `"1 day"`, which is parsed by [lubridate::period()], or a lubridate
#'   Period object generated from one of the helpers, such as
#'   [lubridate::days()].
#'
#'   The period is restricted to any combination of years, months, weeks, or
#'   days. Higher resolution periods such as hours, minutes, and seconds
#'   are not allowed.
#'
#'   Negative period values are allowed, to shift backwards.
#'
#' @param convention `[character(1)]`
#'
#'   A business day convention to follow if the date you land on after shifting
#'   is a holiday. These conventions only apply for monthly / yearly periods.
#'   For daily periods, the date is always shifted to the next available
#'   business day. The following conventions are available in the [conventions]
#'   object:
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
#' @param cal `[calendar]`
#'
#'   A calendar.
#'
#' @examples
#' library(lubridate)
#'
#' x <- as.Date("2018-12-18") + 0:3
#' x
#'
#' # The 22nd and 23rd are weekends so
#' # "2018-12-21" is shifted to the next available
#' # business day, "2018-12-24"
#' cal_shift(x, "1 day")
#'
#' # The 25th is Christmas
#' # The following occurs because we skip over the weekend:
#' # "2018-12-20" -> "2018-12-24"
#' # Here, first we land at "2018-12-24" after skipping over the weekend,
#' # then we go one more business day forward to "2018-12-26", skipping
#' # over Christmas
#' # "2018-12-21" -> "2018-12-26"
#' cal_shift(x, "2 day")
#'
#' # We could define a calendar where Christmas for that year is not a holiday
#' cal <- calendar()
#' cal <- holidays_remove(cal, "2018-12-25")
#' cal_shift(x, "2 day", cal = cal)
#'
#' # ---------------------------------------------------------------------------
#' # Conventions
#'
#' # In this case, moving forward 1 month would land on New Years. The
#' # default convention is to move to the "following" business day
#' cal_shift("2019-12-01", months(1))
#'
#' # We can adjust this with the convention argument. Here we ask to move
#' # to the first business day before the holiday that we landed on
#' cal_shift("2019-12-01", months(1), "preceding")
#'
#' # If you are okay with landing on New Years, use unadjusted.
#' cal_shift("2019-12-01", months(1), "unadjusted")
#'
#' # 2010 was special because New Year was celebrated on the
#' # last day of December.
#' cal_shift("2009-12-31", years(1))
#'
#' # In this case, "modified_following" can be useful. It generally
#' # shifts forward unless the holiday you land on is the end of the month,
#' # in which case it rolls back to the previous business day.
#' cal_shift("2009-12-31", years(1), "modified_following")
#'
#' # Which means it gives the same result as preceding here
#' cal_shift("2009-12-31", years(1), "preceding")
#'
#'
#' # Shifting to the end of the business month
#' cal_shift_end_of_month("2019-03-14")
#'
#' # Note that this can shift dates backwards if they are past the end of the
#' # business month, but not yet into the next month
#' cal_shift_end_of_month("2019-03-31")
#'
#' @export
cal_shift <- function(x,
                      period = "1 day",
                      convention = conventions$following,
                      cal = calendar()) {
  x <- vec_cast_date(x)
  vec_assert(convention, character(), 1L)
  assert_calendar(cal)

  period <- parse_period(period)

  calendar_shift(x, period, convention, cal)
}

#' @rdname cal_shift
#' @export
cal_shift_end_of_month <- function(x, cal = calendar()) {
  x <- vec_cast_date(x)
  assert_calendar(cal)
  calendar_shift_end_of_month(x, cal)
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

# Safer date cast until:
# https://github.com/r-lib/vctrs/issues/549
vec_cast_date <- function(x) {
  if (is.character(x)) {
    vec_cast_date_character(x)
  } else {
    vec_cast(x, new_date())
  }
}

vec_cast_date_character <- function(x) {
  to <- new_date()
  out <- vec_cast(x, to)
  maybe_lossy_cast(out, x, to, lossy = is.na(out) & !is.na(x))
}
