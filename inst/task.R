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
cron_create("DSM", "DTW", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "minutely")
cron_create("DSM", "DTW", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "hourly")
# cron_create("DSM", "DTW", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "daily", at = "3AM")

# Get all the jobs
cron_ls()

# Clear all the jobs
cron_clear(ask = FALSE)
