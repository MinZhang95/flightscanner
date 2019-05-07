context("test-database")

name <- load("test-response.rda")
response <- get(name)

test_that("Function ListUnpack doesn't work.", {
  expect_error(ListUnpack(2, mutate = TRUE))
  res1 <- ListUnpack(iris, mutate = TRUE)
  expect_type(res1, "list")
  res2 <- ListUnpack(iris, mutate = TRUE, vars = "Species")
  expect_type(res2$Species, "character")
})

test_that("Function ListPack doesn't work.", {
  expect_error(ListPack(2, mutate = TRUE))
  res1 <- ListPack(iris, mutate = FALSE)
  expect_type(res1, "double")
  res2 <- ListPack(iris, mutate = TRUE, vars = "Species")
  expect_type(res2$Species, "list")
})

test_that("Function dbAppendTableNew doesn't work.", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  dbWriteTable(con, "iris", iris[0, ])
  expect_type(dbAppendTableNew(con, "iris", iris), "double")
  expect_error(dbAppendTableNew(con, "iris", iris$Species))
  dbDisconnect(con)
})

test_that("Function dbCreateDB doesn't work.", {
  expect_error(dbCreateDB(c(0, 1), dbname = ":memory:"))
  con <- dbCreateDB(RSQLite::SQLite(), dbname = ":memory:")
  expect_s4_class(con, "SQLiteConnection")
  expect_s4_class(dbCreateDB(con, dbname = ":memory:"), "SQLiteConnection")
  dbDisconnect(con)
})

test_that("Function dbSaveData doesn't work.", {
  con <- dbCreateDB(dbname = ":memory:")
  expect_error(dbSaveData(con, iris))
  expect_invisible(suppressWarnings(dbSaveData(response, con)))
  
  # dbSaveData(resp, con)
  # dbSaveData(flightGet(data), con)
  dbDisconnect(con)
})
