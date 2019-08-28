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

  x <- as_period(x)
  y <- as_period(y)

  op <- getExportedValue("base", op)
  out <- op(x, y)

  new_bperiod(period = out, cal = x_cal)
}

vec_arith.BPeriod.Period <- function(op, x, y, ...) {
  if (is_subdaily(y)) {
    abort("When combining a `Period` with a `BPeriod`, the `Period` cannot have a finer resolution than daily.")
  }

  cal <- x@cal
  x <- as_period(x)

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
  x <- as_period(x)
  cal_shift(x = y, period = x, cal = cal)
}

subtract_bperiod_from_date <- function(x, y) {
  cal <- x@cal
  x <- as_period(x)
  cal_shift(x = y, period = -x, cal = cal)
}

is_subdaily <- function(x) {
  sum(lubridate::hour(x), lubridate::minute(x), lubridate::second(x)) != 0L
}

# ------------------------------------------------------------------------------

# Required to set it as a slot in BPeriod
setOldClass("calendar")
setOldClass(c("empty_calendar", "calendar"))

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

setMethod("show", signature(object = "BPeriod"), function(object) {
  cat_line("<BPeriod: ", get_name(object@cal), ">")
  print(format(object))
})

format.BPeriod <- function(x, ...) {
  if (vec_size(x@.Data) == 0L) {
    return("BPeriod(0)")
  }

  show <- paste(
    x@year, "y ",
    x@month, "m ",
    x@day, "d",
    sep = ""
  )

  start <- regexpr("[-1-9]|(0\\.)", show)
  show <- ifelse(start > 0, substr(show, start, nchar(show)), "0d")

  show[is.na(x)] <- NA

  show
}

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}

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

# So vec_recycle() and vec_slice() work

setMethod(
  "[",
  signature(x = "BPeriod"),
  function(x, i, j, ..., drop = TRUE) {
    cal <- x@cal
    x <- as_period(x)
    x <- x[i, j, ..., drop = drop]
    new_bperiod(x, cal)
  }
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

