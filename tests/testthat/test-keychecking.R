context("test-keychecking")

test_that("Function CheckAPIkey doesn't work.", {
  expect_silent(CheckAPIkey())
})
