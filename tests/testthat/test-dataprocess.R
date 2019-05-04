context("test-dataprocess")

CreateSession_eg <- CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10")
PollSession_eg <- PollSession(response = CreateSession_eg)

test_that("GetData function works", {
  expect_error(GetData(x = 123))
  testthat::expect_is(GetData(x = PollSession_eg), "list")
})

test_that("BetweenTime function works", {
  expect_error(BetweenTime(x = 123, interval = c("6:00", "7:30")))
  expect_error(BetweenTime(x = lubridate::ymd_hms("2019-01-02 07:30:00"), interval = 123))
  expect_true(BetweenTime(x = lubridate::ymd_hms("2019-01-02 07:30:00"), interval = c("7:30", "8:00")))
})
