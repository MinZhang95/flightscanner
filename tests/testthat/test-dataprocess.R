context("test-dataprocess")

name <- load("resptest.rda")
response <- get(name)
getdata <- GetData(x = response)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "flighttest.db")

test_that("GetData function works", {
  expect_error(GetData(x = 123))
  testthat::expect_is(getdata, "list")
  testthat::expect_is(GetData(x = con), "list")
})

test_that("BetweenTime function works", {
  expect_error(BetweenTime(x = 123, interval = c("6:00", "7:30")))
  expect_error(BetweenTime(x = lubridate::ymd_hms("2019-01-02 07:30:00"), interval = 123))
  expect_true(BetweenTime(x = lubridate::ymd_hms("2019-01-02 07:30:00"), interval = c("7:30", "8:00")))
})

test_that("CheckDuplicate function works", {
  expect_error(flightscanner:::CheckDuplicate(.data = 123))
  expect_warning(flightscanner:::CheckDuplicate(.data = tibble(OutboundLegId = c(12, 12), InboundLegId = c(34, 34))))
  expect_false(flightscanner:::CheckDuplicate(.data = tibble(OutboundLegId = c(12, 34), InboundLegId = c(56, 78))))
  expect_false(flightscanner:::CheckDuplicate(.data = tibble(FakeName1 = c(12, 34), FakeName2 = c(56, 78))))
})

test_that("FilterFlight funtion works", {
  expect_error(FilterFlight(x = getdata, max_price = "aa"))
  expect_error(FilterFlight(x = getdata, max_duration = "aa"))
  expect_error(FilterFlight(x = getdata, max_stops = "aa"))
  expect_error(FilterFlight(x = getdata, layover = "aa"))
  expect_error(FilterFlight(x = getdata, carrier_include = 123))
  # expect_error(FilterFlight(x = getdata, carrier_exclude = 123))
  expect_error(FilterFlight(x = getdata, out_departure = 123))
  expect_error(FilterFlight(x = getdata, out_arrival = 123))
  expect_error(FilterFlight(x = getdata, in_departure = 123))
  expect_error(FilterFlight(x = getdata, in_arrival = 123))
  expect_true(is.tbl(FilterFlight(x = getdata, out_departure = c(0, 24*60))))
  expect_warning(FilterFlight(x = lapply(getdata, function(t) {t[2, ]=t[1, ]; t})))
})


