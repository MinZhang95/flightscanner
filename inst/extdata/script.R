#!/usr/bin/env Rscript
cat("--------------------------------------------------------------------------------\n")
cat(sprintf("Time: %s\n", Sys.time()))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4 || length(args) > 5) {
  stop("Arguments should have length of 4 or 5: ",
       "Directory (save database and log files), ",
       "origin, destination, startDate, returnDate (optional).")
}

setwd(args[1])
library(flightscanner)
SetAPI(host = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
       key = "3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")

cat(sprintf("Working Directory: %s\n", getwd()))
cat(sprintf("API Key: %s\n", getOption("API")$key))
cat("Command Line Arguments: ", args[-1], "\n")

con <- dbCreateDB(dbname = file.path(args[1], "flight.db"))
resp.post <- CreateSession(orig = args[2], dest = args[3], startDate = args[4],
                           returnDate = flightscanner:::Args2null(args[5]))
resp.get <- PollSession(respondPOST = resp.post)
SaveData(con, x = resp.get)
DBI::dbDisconnect(con)
cat("\n\n")