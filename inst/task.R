Sys.getenv()
system()

rstudioapi::getPersistentValue
rstudioapi::jobRunScript()
rstudioapi::selectFile()


Sys.info()["sysname"]
.Platform$OS.type
R.version$os

rappdirs:::get_os()

get_os <- function() {
  if (.Platform$OS.type == "windows") { 
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac" 
  } else if (.Platform$OS.type == "unix") { 
    "unix"
  } else {
    stop("Unknown OS")
  }
}


library(taskscheduleR)  # Windows
library(cronR)  # Unix/Linux







