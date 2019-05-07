context("test-api")

apiSet("3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")

test_that("Function apiCheckKey doesn't work.", {
  expect_silent(apiCheckKey())
  options(APIkey = NULL)
  expect_silent(apiCheckKey())
})

test_that("Function apiCheckStatus doesn't work.", {
  expect_true(apiCheckStatus(200L))
  expect_warning(apiCheckStatus(404L))
})

test_that("Function apiCreateSession doesn't work.", {
  expect_error(apiCreateSession(123, "DSM", "2019-06-10"))
  expect_error(apiCreateSession("DSM", 123, "2019-06-10"))
  expect_error(apiCreateSession("DSM", "ORD", "2019-06-10", returnDate = "2019-06-01"))
  expect_error(apiCreateSession("DSM", "ORD", "2019-06-10", adults = "as"))
  resp <- apiCreateSession("DSM", "ORD", "2019-06-10")
  expect_equal(http_status(resp)$category, "Success")
})

test_that("Function apiPollSession doesn't work.", {
  expect_error(apiPollSession(sortType = "time"))
  expect_error(apiPollSession(sortOrder = "increase"))
  resp <- apiPollSession(apiCreateSession("DSM", "ORD", "2019-06-10"))
  expect_equal(http_status(resp)$category, "Success")
})

test_that("Function apiBrowseFlight doesn't work.", {
  expect_error(apiBrowseFlight("quotes", 233, "DTW", "2019-06-01", returnDate = NULL))
  expect_error(apiBrowseFlight("quotes", "DSM", "DTW", "2019-06-10", returnDate = "2019-06-01"))
  resp <- apiBrowseFlight("quotes", "DSM", "DTW", "2019-06-01", returnDate = NULL)
  expect_equal(http_status(resp)$category, "Success")
})
