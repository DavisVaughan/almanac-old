# ------------------------------------------------------------------------------
# Adding holidays

test_that("can add a holiday", {
  cal <- calendar()
  cal <- holidays_add(cal, "2019-01-02")

  expect_equal(holidays_added(cal), as.Date("2019-01-02"))

  cal <- holidays_add(cal, "2019-01-03")
  expect_equal(holidays_added(cal), as.Date(c("2019-01-02", "2019-01-03")))
})

test_that("can add multiple holidays at once", {
  cal <- calendar()
  cal <- holidays_add(cal, c("2019-01-02", "2019-01-03"))

  expect_equal(holidays_added(cal), as.Date(c("2019-01-02", "2019-01-03")))
})

test_that("holidays are sorted", {
  cal <- calendar()
  cal <- holidays_add(cal, c("2019-01-03", "2019-01-02"))

  expect_equal(holidays_added(cal), as.Date(c("2019-01-02", "2019-01-03")))
})

test_that("holidays are unique", {
  cal <- calendar()
  cal <- holidays_add(cal, c("2019-01-02", "2019-01-02"))

  expect_equal(holidays_added(cal), as.Date("2019-01-02"))

  cal <- holidays_add(cal, "2019-01-02")

  expect_equal(holidays_added(cal), as.Date("2019-01-02"))
})

test_that("existing holidays are not added", {
  cal <- calendar()
  cal <- holidays_add(cal, "2019-01-01")

  expect_equal(holidays_added(cal), new_date())

  cal <- holidays_add(cal, "2019-01-02")
  cal <- holidays_add(cal, "2019-01-02")

  expect_equal(holidays_added(cal), as.Date("2019-01-02"))
})

test_that("can both add to added and remove from removed in one step", {
  cal <- calendar()
  cal <- holidays_remove(cal, "2019-01-01")
  cal <- holidays_add(cal, c("2019-01-01", "2019-01-02"))

  expect_equal(holidays_added(cal), as.Date("2019-01-02"))
})

test_that("can look at added holidays", {
  cal <- calendar()
  cal <- holidays_add(cal, "2019-01-02")

  expect_equal(holidays_added(cal), cal$added_holidays)
})

test_that("weekends are counted as holidays", {
  cal <- calendar()
  cal <- holidays_add(cal, "2019-01-05")
  expect_equal(holidays_added(cal), new_date())
})

# ------------------------------------------------------------------------------
# Removing holidays

test_that("holidays can be removed", {
  cal <- calendar()
  cal <- holidays_add(cal, "2019-01-02")
  cal <- holidays_remove(cal, "2019-01-02")

  expect_equal(holidays_added(cal), new_date())
})

test_that("non-existant holidays are not removed", {
  cal <- calendar()
  cal <- holidays_remove(cal, "2019-01-02")

  expect_equal(holidays_removed(cal), new_date())
})

test_that("can remove pre-existing holidays", {
  cal <- calendar()
  cal <- holidays_remove(cal, "2019-01-01")

  expect_equal(holidays_removed(cal), as.Date("2019-01-01"))
})

test_that("can both remove from added and add to removed in one step", {
  cal <- calendar()
  cal <- holidays_add(cal, "2019-01-02")
  cal <- holidays_remove(cal, c("2019-01-01", "2019-01-02"))

  expect_equal(holidays_removed(cal), as.Date("2019-01-01"))
})

test_that("can look at removed holidays", {
  cal <- calendar()
  cal <- holidays_remove(cal, "2019-01-01")

  expect_equal(holidays_removed(cal), cal$removed_holidays)
})

test_that("weekends can be removed, since they are holidays", {
  cal <- calendar()
  cal <- holidays_remove(cal, "2019-01-05")
  expect_equal(holidays_removed(cal), as.Date("2019-01-05"))
})

# ------------------------------------------------------------------------------

test_that("can look at all holidays in a calendar", {
  cal <- calendar()
  holidays <- holidays_all(cal)
  expect_is(holidays, "Date")

  # Approximate length in case more holidays are added
  expect_gt(length(holidays), 2800)

  # Just look a a random one of them that we know is a holiday
  expect_true(vec_in(as.Date("1980-07-04"), holidays))
})

test_that("custom holidays are included in all holiday query", {
  cal <- calendar()
  cal <- holidays_add(cal, "2019-01-07")
  holidays <- holidays_all(cal)

  expect_true(vec_in(as.Date("2019-01-07"), holidays))
})

test_that("can get all holidays for all calendars", {
  all_holidays <- holidays_all_calendars()

  if (is_installed("tibble")) {
    expect_is(all_holidays, "tbl_df")
  } else {
    expect_is(all_holidays, "data.frame")
  }

  expect_equal(colnames(all_holidays), c("calendar", "holidays"))

  expect_is(all_holidays$calendar, "character")
  expect_is(all_holidays$holidays, "vctrs_list_of")
  expect_equal(attributes(all_holidays$holidays)$ptype, new_date())
})

test_that("can get holidays between dates", {
  cal <- calendar()

  expect_equal(
    holidays_between("2018-12-20", "2019-01-05", calendar = cal),
    as.Date(c("2018-12-25", "2019-01-01"))
  )

  expect_equal(
    holidays_between("2018-12-28", "2019-01-05", weekends = TRUE, calendar = cal),
    as.Date(c("2018-12-29", "2018-12-30", "2019-01-01", "2019-01-05"))
  )
})

test_that("custom holidays are included when searching for holidays", {
  cal <- calendar()
  cal <- holidays_add(cal, "2019-01-02")

  expect_equal(
    holidays_between("2018-12-20", "2019-01-05", calendar = cal),
    as.Date(c("2018-12-25", "2019-01-01", "2019-01-02"))
  )
})

test_that("start must be strictly before stop", {
  expect_error(
    holidays_between("2019-12-20", "2019-01-05"),
    "`start` [(]2019-12-20[)] must be strictly less than `stop` [(]2019-01-05[)]"
  )

  expect_error(
    holidays_between("2019-12-20", "2019-12-20"),
    "`start` [(]2019-12-20[)] must be strictly less than `stop` [(]2019-12-20[)]"
  )
})
