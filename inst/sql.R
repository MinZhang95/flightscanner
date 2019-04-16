library(DBI)

# initialize SQLite database
con <- dbCreateDB(dbname = "inst/flight.db")
dbDisconnect(con)
dbListTables(con)

# remove all tables
sapply(dbListTables(con), dbRemoveTable, conn = con)

# write data into database
SaveData(con, x = resp.get)

# extract data
data <- GetData(resp.get)  # from API response
GetData(con)  # from SQLite database





library(sqldf)
sqldf()


dcon <- src_sqlite("inst/flight.db")
a = tbl(con, from = "segment")
a
