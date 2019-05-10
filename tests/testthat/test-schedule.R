context("test-schedule")
skip_on_os("windows")

test_that("Function Args2Null doesn't work.", {
  expect_null(Args2Null(NA))
  expect_identical("2016-01-01", Args2Null("2016-01-01"))
})

test_that("Function cron_create doesn't work.", {
  expect_warning(cron_create("SFO", "LHR", "2019-07-01", key = NULL, frequency = "daily"))
  expect_invisible(cron_create("SFO", "LHR", "2019-07-01", frequency = "daily", id = 585), "list")
  cron_rm(id = 585)
})
