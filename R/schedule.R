# README example
# You can also generate date sequences relative to these calendars.
#
# ```{r}
# # Skips over the weekend, and President's Day (2019-02-18)
# cal_seq("2019-02-15", "2019-02-25", 1, cal = cal)
#
# # Skips over the weekend, includes President's Day, skips over the additional
# # holiday on 2019-02-21
# cal_seq("2019-02-15", "2019-02-25", 1, cal = cal_tweaked)
# ```

cal_seq <- function(start,
                    stop,
                    by = 1L,
                    unit = "day",
                    start_convention = conventions$following,
                    stop_convention = conventions$following,
                    end_of_month = FALSE,
                    cal = calendar()) {

  start <- vec_cast_date(start)
  vec_assert(start, size = 1L)
  stop <- vec_cast_date(stop)
  vec_assert(stop, size = 1L)
  by <- vec_cast(by, integer())
  vec_assert(by, size = 1L)
  arg_match(unit, time_units())
  vec_assert(unit, character(), 1L)
  vec_assert(start_convention, character(), 1L)
  vec_assert(stop_convention, character(), 1L)
  vec_assert(end_of_month, logical(), 1L)
  assert_calendar(cal)

  calendar_seq(
    start,
    stop,
    by,
    unit,
    start_convention,
    stop_convention,
    end_of_month,
    cal
  )
}
