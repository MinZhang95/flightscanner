context("test-database")

test_that("ListUnpack works", {
  expect_error(ListUnpack(x=2, mutate = TRUE))
})
