test_that("can create a default calendar", {
  cal <- calendar()
  expect_is(cal, "calendar")
  expect_equal(cal$name, "united_states")
  expect_equal(cal$holidays, new_date())
})

test_that("can create a calendar with a different name", {
  cal <- calendar(calendars$argentina)
  expect_is(cal, "calendar")
  expect_equal(cal$name, "argentina")
})

test_that("error when using an unknown name", {
  expect_error(calendar("arggg"), "name from `calendars`, not 'arggg'")
})

test_that("can get default calendar", {
  expect_equal(default_calendar(), calendar())
})

test_that("can set default calendar", {
  set_default_calendar(calendar(calendars$argentina))
  on.exit(set_default_calendar(calendar()))

  expect_equal(default_calendar(), calendar(calendars$argentina))
})

test_that("error if new default calendar is not a calendar", {
  expect_error(set_default_calendar("x"), "'calendar', not a 'character'")
})

test_that("calendar printing", {
  cal <- calendar()
  expect_known_output(print(cal), file = test_path("output/print-calendar.txt"))

  cal <- calendar(calendars$argentina)
  expect_known_output(print(cal), file = test_path("output/print-calendar-argentina.txt"))

  cal <- calendar()
  holiday <- as.Date("2019-01-01")
  cal <- holidays_add(cal, holiday)
  expect_known_output(print(cal), file = test_path("output/print-calendar-holiday.txt"))

  cal <- calendar(calendars$argentina)
  holidays <- as.Date(c("2019-01-01", "2019-01-03", "2019-01-02"))
  cal <- holidays_add(cal, holidays)
  expect_known_output(print(cal), file = test_path("output/print-calendar-holidays.txt"))

  cal <- calendar(calendars$argentina)
  many_holidays <- as.Date("2019-01-01") + 0:10
  cal <- holidays_add(cal, many_holidays)
  expect_known_output(print(cal), file = test_path("output/print-calendar-many-holidays.txt"))
})

test_that("calendars object", {
  expect_is(calendars, "list")
  expect_true(is_named(calendars))
})

test_that("conventions object", {
  expect_equal(
    conventions,
    list(
      following = "following",
      modified_following = "modified_following",
      preceding = "preceding",
      modified_preceding = "modified_preceding",
      unadjusted = "unadjusted",
      half_month_modified_following = "half_month_modified_following",
      nearest = "nearest"
    )
  )
})