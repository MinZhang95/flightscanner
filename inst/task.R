dump()


























####################################
Sys.getenv()
system()

rstudioapi::getPersistentValue
rstudioapi::jobRunScript()
rstudioapi::selectFile()
rstudioapi::documentSave(1)
rstudioapi::getActiveDocumentContext()

Sys.info()["sysname"]
.Platform$OS.type
R.version$os

rappdirs:::get_os()


library(taskscheduleR)  # Windows
library(cronR)  # Unix/Linux

cron_rstudioaddin()

f <- system.file(package = "cronR", "extdata", "helloworld.R")
f <- file.path(getwd(), "inst/test/helloworld.R")

cmd <- cron_rscript(f, rscript_args = c("productx", "20160101"))
## Every minute
cron_add(cmd, frequency = 'minutely', id = 'job1', description = 'Customers')
## Every hour at 20 past the hour on Monday and Tuesday
cron_add(cmd, frequency = 'hourly', id = 'job2', at = '00:20', description = 'Weather', days_of_week = c(1, 2))
## Every day at 14h20 on Sunday, Wednesday and Friday
cron_add(cmd, frequency = 'daily', id = 'job3', at = '14:20', days_of_week = c(0, 3, 5))
## Every starting day of the month at 10h30
cron_add(cmd, frequency = 'monthly', id = 'job4', at = '10:30', days_of_month = 'first', days_of_week = '*')
## Get all the jobs
cron_njobs()
cron_ls()
## Remove all scheduled jobs
cron_clear(ask = FALSE)
cron_ls()

save()


