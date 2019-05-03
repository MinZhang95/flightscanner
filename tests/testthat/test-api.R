context("test-api")

test_that("CreateSession function inputs are correct", {
  expect_warning(CreateSession(origin = 123, destination = "DSM", startDate = "2019-06-10"))
  expect_warning(CreateSession(origin = "DSM", destination = 123, startDate = "2019-06-10"))
  expect_warning(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019"))
  expect_warning(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", returnDate = "ISU"))
  expect_warning(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", returnDate = "2019-06-01"))
  expect_warning(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", adults = "as"))
})

test_that("CreateSession function output type is correct",{
  expect_class(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10"), "response")
})

test_that("PollSession function input sort type and order is correct",{
  expect_error(PollSession(sortType = "time"))
  expect_error(PollSession(sortOrder = "increase"))
})

test_that("PollSession function output type is correct",{
  expect_class(PollSession(respondPOST = CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10")), "response")
})