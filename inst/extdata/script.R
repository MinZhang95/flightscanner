#!/usr/bin/env Rscript
cat("--------------------------------------------------------------------------------\n")
cat("Time:", as.character(Sys.time()), "\n")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4 || length(args) > 5) {
  stop("Arguments should have length of 4 or 5: ",
       "Directory (save database and log files), ",
       "origin, destination, startDate, returnDate (optional).")
}

setwd(args[1])
library(flightscanner)

cat("Working Directory:", getwd(), "\n")
cat("API Key:", apiGetKey(), "\n")
cat("Command Line Arguments:", args[-1], "\n")

name_dbfile <- paste0("flight_", paste(args[-1], collapse = "_"), ".db")
con <- dbCreateDB(dbname = file.path(args[1], name_dbfile))
resp <- apiCreateSession(orig = args[2], dest = args[3], startDate = args[4],
                         returnDate = flightscanner:::Args2Null(args[5]))
resp <- apiPollSession(resp)
dbSaveData(resp, con)
dbDisconnect(con)
cat("\n\n")