#' Check the API key.
#' @description Check the API key. This function will automatically run when loading the package.
CheckAPIkey <- function() {
  host <- "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com"
  website <- "https://rapidapi.com/skyscanner/api/skyscanner-flight-search"
  filename <- "API_key.txt"
  
  if (!file.exists(filename)) {
    cli::cat_line("API key is required!", col = "red")
    cli::cat_line("See the following instruction for the Key:")
    cli::cat_bullet("1. Browse and Log In: \n\t", website)
    cli::cat_line("Do you need me to open the browser? 1 for YES; 0 for NO")
    
    ANS <- readline("")
    if (ANS == 1) BROWSE(website)
    cli::cat_bullet("2. Copy the X-RapidAPI-Key from the 3rd line in Request Snippet(Right Panel)")
    cli::cat_line("What's your key (without quote)?")
    
    key <- readline("")
    if (key != "") {
      utils::write.table(key, file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
      SetAPI(host, key)
      cli::cat_line("Welcome to FlightScanner!")
    }
  } else {
    key <- tryCatch(utils::read.table(file = filename, stringsAsFactors = FALSE),
                    warning = function(w) {}, error = function(e) {})[1, 1]
    url <- paste0("https://", host, "/apiservices/reference/v1.0/currencies")
    resp <- GET(url, add_headers(MakeHeader(host, key)))
    
    if (!suppressWarnings(CheckStatus(resp))) {
      cli::cat_line("Welcome to FlightScanner!")
      SetAPI(host, key)
    } else cli::cat_line("Check your API key or network connection.")
  }
}
