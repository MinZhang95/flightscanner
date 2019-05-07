context("test-dataprocess")

name <- load("test-response.rda")
response <- get(name)

test_that("Function flightGet doesn't work.", {
  expect_error(flightGet(123))
  expect_type(suppressWarnings(flightGet(response)), "list")
  con <- dbConnect(RSQLite::SQLite(), dbname = "test-flight.db")
  expect_type(flightGet(con), "list")
  dbDisconnect(con)
})

test_that("Function BetweenTime doesn't work.", {
  x <- lubridate::ymd_hms("2019-01-02 07:30:00")
  expect_error(BetweenTime(123, c("6:00", "7:30")))
  expect_error(BetweenTime(x, 123))
  expect_true(BetweenTime(x, c("7:30", "8:00")))
  expect_false(BetweenTime(x, c("6:00", "7:29")))
})

test_that("Function CheckDuplicateRow doesn't work.", {
  df <- data.frame(Id = c(1, 1), name = c("A", "B"))
  expect_error(CheckDuplicateRow(123))
  expect_warning(CheckDuplicateRow(df))
  expect_false(CheckDuplicateRow(df, "name"))
})

test_that("Function flightFilter doesn't work.", {
  data <- suppressWarnings(flightGet(response))
  expect_error(flightFilter(data, max_price = "aa"))
  expect_error(flightFilter(data, max_duration = "aa"))
  expect_error(flightFilter(data, max_stops = "aa"))
  expect_error(flightFilter(data, layover = "aa"))
  expect_error(flightFilter(data, carrier_include = 123))
  expect_error(flightFilter(data, carrier_exclude = 123))
  expect_error(flightFilter(data, out_departure = 123))
  expect_error(flightFilter(data, out_arrival = 123))
  expect_error(flightFilter(data, in_departure = 123))
  expect_error(flightFilter(data, in_arrival = 123))
  expect_true(is.tbl(flightFilter(data, out_departure = c(0, 24 * 60))))
  expect_warning(flightFilter(lapply(data, function(t) {t[2, ] <- t[1, ]; t})))
})
