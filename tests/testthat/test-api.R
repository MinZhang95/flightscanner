context("test-api")

test_that("CreateSession function inputs are correct", {
  expect_error(CreateSession(origin = 123, destination = "DSM", startDate = "2019-06-10"))
  expect_error(CreateSession(origin = "DSM", destination = 123, startDate = "2019-06-10"))
  expect_error(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", returnDate = "2019-06-01"))
  expect_error(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", adults = "as"))
})


test_that("PollSession function input sort type and order is correct",{
  expect_error(PollSession(sortType = "time"))
  expect_error(PollSession(sortOrder = "increase"))
})



test_that("BrowseFlight function's input and output type are correct",{
  expect_error(BrowseFlight("quote", orig = "DSM", dest = "DTW", start = "2019-06-01", return = NULL))
  expect_error(BrowseFlight("quotes", orig = 233, dest = "DTW", start = "2019-06-01", return = NULL))
})

