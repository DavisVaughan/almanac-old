#' @export
cal_seq <- function(start,
                    stop,
                    by = 1L,
                    unit = "day",
                    start_convention = conventions$following,
                    stop_convention = conventions$following,
                    end_of_month = FALSE,
                    calendar = default_calendar()) {

  vec_assert(start, ptype = new_date(), size = 1L)
  vec_assert(stop, ptype = new_date(), size = 1L)
  by <- vec_cast(by, integer())
  vec_assert(by, size = 1L)
  arg_match(unit, time_units())
  vec_assert(unit, character(), 1L)
  vec_assert(start_convention, character(), 1L)
  vec_assert(stop_convention, character(), 1L)
  vec_assert(end_of_month, logical(), 1L)
  assert_calendar(calendar)

  calendar_seq(
    start,
    stop,
    by,
    unit,
    start_convention,
    stop_convention,
    end_of_month,
    calendar
  )
}
