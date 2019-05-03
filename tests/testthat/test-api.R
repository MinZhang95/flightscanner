context("test-api")

test_that("CreateSession function inputs are correct", {
  expect_error(CreateSession(origin = 123, destination = "DSM", startDate = "2019-06-10"))
  expect_error(CreateSession(origin = "DSM", destination = 123, startDate = "2019-06-10"))
  expect_error(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019"))
  expect_error(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", returnDate = "ISU"))
  expect_error(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", returnDate = "2019-06-01"))
  expect_error(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", adults = "4"))
})

test_that("CreateSession function output type is correct",{
  expect_class(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10"), "response")
})

test_that("PollSession function input sort type and order is correct",{
  expect_error(PollSession(sortType = "time"))
  expect_error(PollSession(sortOrder = "increase"))
})

test_that("PollSession function output type is correct",{
  expect_class(PollSession(), "response")
})