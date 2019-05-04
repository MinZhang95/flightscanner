context("test-unpack")

name <- load("resptest.rda")
response <- get(name)

test_that("GetPrice works", {
  expect_error(GetPrice(iris))
  #price <- GetPrice(response)
  #expect_s3_class(b, "tbl")
  expect_true(is.tbl(GetPrice(response)))
})

