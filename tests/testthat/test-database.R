context("test-database")

test_that("ListUnpack works", {
  expect_error(ListUnpack(x = 2, mutate = TRUE))
  res1 <- ListUnpack(iris, mutate = TRUE)
  expect_true(is.list(res1))
  res2 <- ListUnpack(iris, mutate = TRUE, vars = "Species")
  expect_true(is.character(res2$Species))
})

test_that("ListPack works",{
  expect_error(ListPack(x = 2, mutate = TRUE))
  res1 <- ListPack(iris, mutate = FALSE)
  expect_true(is.numeric(res1))
  res2 <- ListPack(iris, mutate = TRUE, vars = "Species")
  expect_true(is.list(res2$Species))
  }
)

test_that("dbAppendTableNew works", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "iris", iris[0, ])
  expect_true(is.numeric(flightscanner:::dbAppendTableNew(con, "iris", iris)))
  expect_error(flightscanner:::dbAppendTableNew(con, "iris", iris$Species))
})

test_that("dbCreateDB works",{
  expect_error(dbCreateDB(c(0,1), dbname = "flight.db"))
  con1 <- RSQLite::SQLite()
  expect_s4_class(dbCreateDB(con1, dbname = "flight.db"), "SQLiteConnection")
  con2 <- dbConnect(RSQLite::SQLite())
  expect_s4_class(dbCreateDB(con2, dbname = "flight.db"), "SQLiteConnection")
  unlink("flight.db")
})
