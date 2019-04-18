library(dplyr)
library(DBI)

# initialize SQLite database
con <- dbCreateDB(dbname = "inst/flight.db")
con <- dbConnect(RSQLite::SQLite(),dbname = "inst/flight.db")

dbReadTable(con,'agent')-> agent
dbReadTable(con,'price')-> price
dbReadTable(con,'itinerary') -> itinerary


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
