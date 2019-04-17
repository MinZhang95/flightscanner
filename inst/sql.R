library(dplyr)
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
data1 <- GetData(resp.get)  # from API response
data2 <- GetData(con)  # from SQLite database
GetData(con, lazy = T)

src <- src_sqlite("inst/flight.db")
tbl(con, from = "price")
