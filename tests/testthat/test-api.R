context("test-api")

SetAPI(host = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
       key = "3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")

test_that("CreateSession function inputs and outputs are correct", {
  expect_error(CreateSession(origin = 123, destination = "DSM", startDate = "2019-06-10"))
  expect_error(CreateSession(origin = "DSM", destination = 123, startDate = "2019-06-10"))
  expect_error(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", returnDate = "2019-06-01"))
  expect_error(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10", adults = "as"))
  expect_true(http_status(CreateSession(origin = "DSM", destination = "ORD", startDate = "2019-06-10"))$category=="Success")
})


test_that("PollSession function input sort type and order is correct", {
  expect_error(PollSession(sortType = "time"))
  expect_error(PollSession(sortOrder = "increase"))
})



test_that("BrowseFlight function's input and output type are correct", {
  #expect_error(BrowseFlight("quote", origin = "DSM", destination  = "DTW", startDate  = "2019-06-10", returnDate = NULL))
  expect_error(BrowseFlight("quotes", origin = 233, destination = "DTW", startDate = "2019-06-01", returnDate = NULL))
  expect_error(BrowseFlight("quotes", origin = "DSM", destination = "DTW", startDate = "2019-06-10", returnDate = "2019-06-01"))
})

