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


#' Convert missing arguments to NULL.
#'
#' @param x Argument.
#'
#' @return Converted argument.
Args2null <- function(x) {
  if (is.na(x)) NULL else x
}


#' Create a cron job.
#' @description Generate a cron job, and pass it to crontab.
#' It will automatically scape flight data and save them to a local SQLite database file.
#'
#' @param origin The origin place, can be country, city, airport, in Skyscanner code.
#' @param destination The destination, can be country, city, airport, in Skyscanner code.
#' @param startDate The outbound date. Format 'yyyy-mm-dd'.
#' @param returnDate The return date. Format 'yyyy-mm-dd'. Use NULL for oneway trip.
#' @param path Where to put the log, defaults is the current working directory.
#' @param frequency A character string equal to one of
#' \code{"minutely"}, \code{"hourly"}, or \code{"daily"}.
#' @param at The actual time of day at which to execute the command.
#' When unspecified, we default to "8PM",
#' when the command is to be run less frequently than \code{"hourly"}.
#' @param ... Other arguments.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' CreateJob("SFO", "LHR", "2019-07-01", frequency = "daily", at = "3AM")
#' CreateJob("SFO", "LHR", "2019-07-01", frequency = "hourly")
#' CreateJob("SFO", "LHR", "2019-07-01", frequency = "minutely")
#' }
CreateJob <- function(origin, destination, startDate, returnDate = NULL, path = getwd(),
                      frequency = c("daily", "hourly", "minutely"), at, ...) {
  frequency <- match.arg(frequency)
  args_flight <- c(origin, destination, startDate, returnDate)
  path <- tools::file_path_as_absolute(path)
  
  f <- system.file(package = "flightscanner", "extdata", "script.R")
  id <- paste(args_flight, collapse = "_")
  name_log <- paste0("script_", id, ".log")
  
  cmd <- cronR::cron_rscript(f, rscript_log = file.path(path, name_log),
                             rscript_args = c(paste0("'", path, "'"), args_flight))
  
  args_cron <- list(command = cmd, frequency = frequency, id = id, description = "Flights")
  if (frequency == "daily") args_cron[["at"]] <- if (missing(at)) "8PM" else at
  do.call(cronR::cron_add, args_cron)
}
