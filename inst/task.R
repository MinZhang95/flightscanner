library(flightscanner)
# library(taskscheduleR)  # Windows
# library(cronR)  # Unix/Linux

# rstudioapi::getPersistentValue
# rstudioapi::jobRunScript()
# rstudioapi::selectFile()
# rstudioapi::documentSave(1)
# rstudioapi::getActiveDocumentContext()
# 
# Sys.info()["sysname"]
# .Platform$OS.type
# R.version$os
# 
# cron_rstudioaddin()

# Create cron job
apiSetKey("23d5e72978msh9f54564c51ae7d9p11f285jsna232a1fe2fdb")
cron_create("DSM", "PVG", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "0 0-23/2 * * *")
cron_create("DSM", "DTW", "2019-06-10", frequency = "daily")

con <- dbCreateDB(dbname = "inst/flight_DSM_PVG_2019-06-01.db")
data <- flightGet(con)

# Get all the jobs
cron_ls()

# Clear all the jobs
cron_clear(ask = FALSE)
