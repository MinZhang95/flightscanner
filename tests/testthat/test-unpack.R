context("test-unpack")

name <- load("resptest.rda")
response <- get(name)

test_that("GetPrice works", {
  expect_error(GetPrice(iris))
  #price <- GetPrice(response)
  #expect_s3_class(b, "tbl")
  expect_true(is.tbl(GetPrice(response)))
})

test_that("GetItineraries works", {
  expect_true(is.tbl(GetItineraries(response)))
})

test_that("GetLegs works", {
  expect_true(is.tbl(GetLegs(response)))
})

test_that("GetSegments works", {
  expect_true(is.tbl(GetSegments(response)))
})

test_that("GetCarriers works", {
  expect_true(is.tbl(GetCarriers(response)))
})

test_that("GetAgents works", {
  expect_true(is.tbl(GetAgents(response)))
})

test_that("GetPlaces works", {
  expect_true(is.tbl(GetPlaces(response)))
})
