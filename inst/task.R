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
cron_create("DSM", "DTW", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "47/10 * * * *")
cron_create("DSM", "DTW", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "* 20/8 * * *")
cron_create("dtw", "dsm", "2019-06-02", "2019/06/07", path = file.path(getwd(), "inst"), frequency = "minutely")
 cron_create("DSM", "DTW", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "hourly")
# cron_create("DSM", "DTW", "2019-06-01", path = file.path(getwd(), "inst"), frequency = "daily", at = "3AM")

# Get all the jobs
cron_ls()

# Clear all the jobs
cron_clear(ask = FALSE)
