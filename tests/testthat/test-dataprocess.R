context("test-dataprocess")

name <- load("resptest.rda")
response <- get(name)
data <- GetData(response)
con <- dbConnect(RSQLite::SQLite(), dbname = "flighttest.db")

test_that("Function GetData doesn't work.", {
  expect_error(GetData(123))
  expect_type(data, "list")
  expect_type(GetData(con), "list")
})

test_that("Function BetweenTime doesn't work.", {
  x <- lubridate::ymd_hms("2019-01-02 07:30:00")
  expect_error(BetweenTime(123, c("6:00", "7:30")))
  expect_error(BetweenTime(x, 123))
  expect_true(BetweenTime(x, c("7:30", "8:00")))
  expect_false(BetweenTime(x, c("6:00", "7:29")))
})

test_that("Function CheckDuplicate doesn't work.", {
  expect_error(CheckDuplicate(123))
  expect_warning(CheckDuplicate(tibble(OutboundLegId = c(12, 12), InboundLegId = c(34, 34))))
  expect_false(CheckDuplicate(tibble(OutboundLegId = c(12, 34), InboundLegId = c(56, 78))))
  expect_false(CheckDuplicate(tibble(FakeName1 = c(12, 34), FakeName2 = c(56, 78))))
})

test_that("Function FilterFlight doesn't work.", {
  expect_error(FilterFlight(data, max_price = "aa"))
  expect_error(FilterFlight(data, max_duration = "aa"))
  expect_error(FilterFlight(data, max_stops = "aa"))
  expect_error(FilterFlight(data, layover = "aa"))
  expect_error(FilterFlight(data, carrier_include = 123))
  expect_error(FilterFlight(data, carrier_exclude = 123))
  expect_error(FilterFlight(data, out_departure = 123))
  expect_error(FilterFlight(data, out_arrival = 123))
  expect_error(FilterFlight(data, in_departure = 123))
  expect_error(FilterFlight(data, in_arrival = 123))
  expect_true(is.tbl(FilterFlight(data, out_departure = c(0, 24 * 60))))
  expect_warning(FilterFlight(lapply(data, function(t) {t[2, ] <- t[1, ]; t})))
})
