context("test-unpack")

name <- load("test-response.rda")
response <- get(name)

test_that("Function GetPrice doesn't work.", {
  expect_error(GetPrice(iris))
  expect_true(is.tbl(GetPrice(response)))
})

test_that("Function GetItineraries doesn't work.", {
  expect_error(GetItineraries(iris))
  expect_true(is.tbl(GetItineraries(response)))
})

test_that("Function GetLegs works doesn't work.", {
  expect_error(GetLegs(iris))
  expect_true(is.tbl(suppressWarnings(GetLegs(response))))
})

test_that("Function GetSegments doesn't work.", {
  expect_error(GetSegments(iris))
  expect_true(is.tbl(GetSegments(response)))
})

test_that("Function GetCarriers doesn't work.", {
  expect_error(GetCarriers(iris))
  expect_true(is.tbl(GetCarriers(response)))
})

test_that("Function GetAgents doesn't work.", {
  expect_error(GetAgents(iris))
  expect_true(is.tbl(GetAgents(response)))
})

test_that("Function GetPlaces doesn't work.", {
  expect_error(GetPlaces(iris))
  expect_true(is.tbl(GetPlaces(response)))
})
