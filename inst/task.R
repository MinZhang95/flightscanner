# library(taskscheduleR)  # Windows
library(cronR)  # Unix/Linux

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
CreateJob("DSM", "DTW", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "minutely")
# CreateJob("DSM", "DTW", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "hourly")
# CreateJob("DSM", "DTW", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "daily", at = "3AM")

## Get all the jobs
cron_njobs()
cron_clear(ask = FALSE)
cron_ls()
