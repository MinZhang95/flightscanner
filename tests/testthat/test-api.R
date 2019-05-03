context("test-api")

test_that("origin is a string", {
  expect_error(CreateSession(origin = 123, destination = "DSM", startDate = "2019-06-10"))
})
