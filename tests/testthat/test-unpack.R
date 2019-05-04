context("test-unpack")

SetAPI(host = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
       key = "fefb4945e2msh1d70bbb54d6ef9bp1901acjsn9d9be332a30e")

test_that("GetItineraries works", {
  resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
  resp <- PollSession(resp)
  expect_true((http_status(resp))$category=="Success")
})

test_that("GetPrice works", {
  expect_error(GetPrice(iris))
})
