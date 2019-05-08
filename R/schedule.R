# #' Identify the operating system.
# #' @return A character of OS.
# get_os <- function() {
#   if (.Platform$OS.type == "windows") {
#     "win"
#   } else if (Sys.info()["sysname"] == "Darwin") {
#     "mac"
#   } else if (.Platform$OS.type == "unix") {
#     "unix"
#   } else {
#     stop("Unknown OS")
#   }
# }


#' Convert missing arguments to NULL.
#'
#' @param x Argument.
#' @return Converted argument.
#' 
#' @examples
#' flightscanner:::Args2Null("character")
#' flightscanner:::Args2Null(NA)
Args2Null <- function(x) {
  if (is.na(x)) NULL else x
}


#' Create a cron job.
#' @description Generate a cron job, and pass it to crontab. It will automatically scape flight data
#' and save them to a local SQLite database file.
#'
#' @param origin The origin place, can be country, city, airport, in Skyscanner code.
#' @param destination The destination, can be country, city, airport, in Skyscanner code.
#' @param startDate The outbound date. Format 'yyyy-mm-dd'.
#' @param returnDate The return date. Format 'yyyy-mm-dd'. Use NULL for oneway trip.
#' @param key API key. Default using the key in global options, see \code{\link{apiSetKey}}.
#' @param path Where to put the log, defaults is the current working directory.
#' @param frequency A character string equal to one of \code{"minutely"}, \code{"hourly"},
#' \code{"daily"}, \code{"monthly"}, or \code{"yearly"}. Or any complex cron schedule.
#' .
#' @param at The actual time of day at which to execute the command. When unspecified, we default to
#' \code{"8PM"}, when the command is to be run less frequently than \code{"hourly"}.
#' @param id An id, or name, to give to the cronjob task, for easier revision in the future.
#' @param ... Other arguments.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' apiSetKey("YOUR_API_KEY")
#' cron_create("SFO", "LHR", "2019-07-01", frequency = "daily", at = "3AM")
#' cron_create("SFO", "LHR", "2019-07-01", frequency = "hourly")
#' cron_create("SFO", "LHR", "2019-07-01", frequency = "minutely")
#' cron_create("SFO", "LHR", "2019-07-01", frequency = "0 */2 * * *")  # every 2 hours
#' }
cron_create <- function(origin, destination, startDate, returnDate = NULL, key = apiGetKey(),
                        path = getwd(), frequency = "daily", at, id, ...) {
  if (is.null(key) || is.na(key) || key == "") {
    warning("Can't create a cron job. Please provide a valid API key.")
    return(invisible())
  }
  
  path <- tools::file_path_as_absolute(path)
  utils::write.table(key, file.path(path, "APIkey.txt"), quote = FALSE,
                     row.names = FALSE, col.names = FALSE)
  
  f <- system.file(package = "flightscanner", "extdata", "script.R")
  args_flight <- c(toupper(c(origin, destination)),
                   format(lubridate::ymd(c(startDate, returnDate)), "%Y-%m-%d"))
  tag <- paste(args_flight, collapse = "_")
  name_log <- paste0("script_", tag, ".log")
  
  cmd <- cronR::cron_rscript(f, rscript_log = file.path(path, name_log),
                             rscript_args = c(paste0("'", path, "'"), args_flight))
  
  args_cron <- list(command = cmd, frequency = frequency, tag = tag, description = "Flights")
  if (frequency == "daily") args_cron[["at"]] <- if (missing(at)) "8PM" else at
  if (!missing(id)) args_cron[["id"]] <- id
  do.call(cronR::cron_add, args_cron)
}


#' Remove a cronjob.
#' @description See \code{cronR::\link[cronR]{cron_rm}} for details.
#'
#' @name cron_rm
#' @export
#' @importFrom cronR cron_rm
NULL


#' Clear all cron jobs.
#' @description See \code{cronR::\link[cronR]{cron_clear}} for details.
#'
#' @name cron_clear
#' @export
#' @importFrom cronR cron_clear
NULL


#' List the contents of a crontab.
#' @description See \code{cronR::\link[cronR]{cron_ls}} for details.
#'
#' @name cron_ls
#' @export
#' @importFrom cronR cron_ls
NULL
