library(DBI)

con <- dbConnect(RSQLite::SQLite(), dbname = "flight.db")
dbDisconnect(con)
dbListTables(con)

itinerary <- GetItineraries(resp.get)
leg <- GetLegs(resp.get)
segment <- GetSegments(resp.get)

carriers <- GetCarriers(resp.get)
agents <- GetAgents(resp.get)
places <- GetPlaces(resp.get)

# remove all tables
sapply(dbListTables(con), dbRemoveTable, conn = con)

dbCreateDB(con)

dbAppendTableNew(con, "carrier", carriers)
dbAppendTableNew(con, "agent", agents)
dbAppendTableNew(con, "place", places)



dcon <- src_sqlite("flight.db")



a = tbl(con, from = "carrier")
