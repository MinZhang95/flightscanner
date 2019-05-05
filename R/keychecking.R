#' Check the API key.
#' @description Check the API key. This function will automatically run when loading the package.
CheckAPIkey <- function() {
  website <- "https://rapidapi.com/skyscanner/api/skyscanner-flight-search"
  filename <- "APIkey.txt"
  
  if (!is.null(getOption("APIkey"))) {
    key <- getOption("APIkey")
  } else if (file.exists(filename)) {
    key <- tryCatch(utils::read.table(filename, stringsAsFactors = FALSE),
                    warning = function(w) {}, error = function(e) {})[1, 1]
  } else if (interactive()) {
    cat("API key is required!\nPlease follow the instructions to get the key:\n")
    cat("1. Browse and login: ", website, "\n")
    cat("   Do you want to visit this website (1 for YES; 0 for NO)?")
    ans <- readline()
    if (ans == 1) BROWSE(website)
    cat("2. Copy the value of X-RapidAPI-Key in Header Parameters.\n")
    cat("   Paste your key (without quote):")
    key <- readline()
  } else key <- ""
  
  header <- apiMakeHeader(key)
  url <- paste0("https://", header["X-RapidAPI-Host"], "/apiservices/reference/v1.0/currencies")
  resp <- GET(url, add_headers(header))
  
  if (suppressWarnings(apiCheckStatus(resp))) {
    SetAPI(key)
    utils::write.table(key, filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
    if (interactive()) cat("Welcome to FlightScanner!\n")
  } else if (interactive()) {
    cat("Check your API key or network connection. And use SetAPI to set your key later.\n")
  }
}
