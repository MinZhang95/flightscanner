context("test-api")

SetAPI(host = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
       key = "a01b3ec5e9msh2698ef80ca5232dp18fc92jsnddf5fad0cc7d")

test_that("CreateSession function inputs and outputs are correct", {
  expect_error(CreateSession(origin = 123, destination = "DSM", startDate = "2019-06-10"))
  expect_error(CreateSession(origin = "DSM", destination = 123, startDate = "2019-06-10"))
  expect_error(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", returnDate = "2019-06-01"))
  expect_error(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", adults = "as"))
  expect_s3_class(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10"), "response")
})


test_that("PollSession function input sort type and order is correct",{
  expect_error(PollSession(sortType = "time"))
  expect_error(PollSession(sortOrder = "increase"))
  expect_s3_class(PollSession(respondPOST = CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10")), "response")
})



test_that("BrowseFlight function's input and output type are correct",{
  expect_error(BrowseFlight("quote", origin = "DSM", destination  = "DTW", startDate  = "2019-06-01", returnDate = NULL))
  expect_error(BrowseFlight("quotes", origin = 233, destination = "DTW", startDate = "2019-06-01", returnDate = NULL))
  expect_error(BrowseFlight("quotes", origin = "DSM", destination = "DTW", , startDate = "2019-06-10", returnDate = "2019-06-01"))
})

