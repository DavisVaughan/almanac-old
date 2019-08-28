#' @include calendar.R
NULL

bdays <- function(x = 1, cal = calendar()) {
  new_bperiod(lubridate::days(x = x), cal = cal)
}

byears <- function(x = 1, cal = calendar()) {
  new_bperiod(lubridate::years(x = x), cal = cal)
}

bmonths <- function(x = 1, cal = calendar()) {
  new_bperiod(months(x = x), cal = cal)
}

bweeks <- function(x = 1, cal = calendar()) {
  new_bperiod(lubridate::weeks(x = x), cal = cal)
}

new_bperiod <- function(period = NULL, cal = calendar()) {
  if (is.null(period)) {
    period <- lubridate::days()
  }

  new("BPeriod", period = period, cal = cal)
}

as_period <- function(x) {
  as(x, "Period")
}

# ------------------------------------------------------------------------------

vec_arith.BPeriod <- function(op, x, y, ...) {
  UseMethod("vec_arith.BPeriod", y)
}

vec_arith.BPeriod.BPeriod <- function(op, x, y, ...) {
  x_cal <- x@cal
  y_cal <- y@cal

  if (!identical(x_cal, y_cal)) {
    abort("`BPeriod` objects must have identical calendars to perform arithmetic on them.")
  }

  x <- as(x, "Period")
  y <- as(y, "Period")

  op <- getExportedValue("base", op)
  out <- op(x, y)

  new_bperiod(period = out, cal = x_cal)
}

vec_arith.BPeriod.Period <- function(op, x, y, ...) {
  cal <- x@cal
  x <- as(x, "Period")

  op <- getExportedValue("base", op)
  out <- op(x, y)

  new_bperiod(period = out, cal = cal)
}

vec_arith.BPeriod.Date <- function(op, x, y, ...) {
  switch(
    op,
    "+" = add_bperiod_to_date(x, y),
    "-" = subtract_bperiod_from_date(x, y)
  )
}

vec_arith.BPeriod.POSIXct <- function(op, x, y, ...) {
  abort("Not supported")
}

vec_arith.BPeriod.numeric <- function(op, x, y, ...) {
  y <- vec_cast(y, integer())
  vec_arith(op, x, lubridate::days(y))
}

vec_arith.BPeriod.integer <- function(op, x, y, ...) {
  vec_arith(op, x, lubridate::days(y))
}


add_bperiod_to_date <- function(x, y) {
  cal <- x@cal
  x <- as(x, "Period")
  cal_shift(x = y, period = x, cal = cal)
}

subtract_bperiod_from_date <- function(x, y) {
  cal <- x@cal
  x <- as(x, "Period")
  cal_shift(x = y, period = -x, cal = cal)
}

# ------------------------------------------------------------------------------

# Required to set it as a slot in BPeriod
setOldClass("calendar")

setClass(
  "BPeriod",
  contains = "Period",
  slots = c(cal = "calendar"),
  prototype = prototype(cal = calendar())
)

setMethod(
  "initialize",
  "BPeriod",
  function(.Object, ..., period, cal) {
    slot(.Object, "cal") <- cal
    slot(.Object, ".Data") <- lubridate::second(period)
    slot(.Object, "year") <- lubridate::year(period)
    slot(.Object, "month") <- lubridate::month(period)
    slot(.Object, "day") <- lubridate::day(period)
    slot(.Object, "hour") <- lubridate::hour(period)
    slot(.Object, "minute") <- lubridate::minute(period)
    .Object
  }
)

# ------------------------------------------------------------------------------

setMethod(
  "+",
  signature(e1 = "BPeriod", e2 = "BPeriod"),
  function(e1, e2) vec_arith("+", e1, e2)
)

setMethod(
  "+",
  signature(e1 = "BPeriod", e2 = "Period"),
  function(e1, e2) vec_arith("+", e1, e2)
)

setMethod(
  "+",
  signature(e1 = "Period", e2 = "BPeriod"),
  function(e1, e2) vec_arith("+", e2, e1)
)

setMethod(
  "+",
  signature(e1 = "BPeriod", e2 = "Date"),
  function(e1, e2) vec_arith("+", e1, e2)
)

setMethod(
  "+",
  signature(e1 = "Date", e2 = "BPeriod"),
  function(e1, e2) vec_arith("+", e2, e1)
)

setMethod(
  "+",
  signature(e1 = "BPeriod", e2 = "POSIXct"),
  function(e1, e2) vec_arith("+", e1, e2)
)

setMethod(
  "+",
  signature(e1 = "POSIXct", e2 = "BPeriod"),
  function(e1, e2) vec_arith("+", e2, e1)
)

setMethod(
  "+",
  signature(e1 = "BPeriod", e2 = "POSIXlt"),
  function(e1, e2) vec_arith("+", e1, e2)
)

setMethod(
  "+",
  signature(e1 = "POSIXlt", e2 = "BPeriod"),
  function(e1, e2) vec_arith("+", e2, e1)
)

setMethod(
  "+",
  signature(e1 = "BPeriod", e2 = "numeric"),
  function(e1, e2) vec_arith("+", e1, e2)
)

setMethod(
  "+",
  signature(e1 = "numeric", e2 = "BPeriod"),
  function(e1, e2) vec_arith("+", e2, e1)
)

setMethod(
  "+",
  signature(e1 = "BPeriod", e2 = "integer"),
  function(e1, e2) vec_arith("+", e1, e2)
)

setMethod(
  "+",
  signature(e1 = "integer", e2 = "BPeriod"),
  function(e1, e2) vec_arith("+", e2, e1)
)


# ------------------------------------------------------------------------------

# as(<BPeriod>, "Period")
setMethod(
  "coerce",
  signature(from = "BPeriod", to = "Period"),
  function(from, to) {
    new(
      "Period",
      from@.Data,
      year = from@year,
      month = from@month,
      day = from@day,
      hour = from@hour,
      minute = from@minute
    )
  }
)

