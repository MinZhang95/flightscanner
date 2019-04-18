#' Checking the API key
#' @description Checking the API key. This function will automatically run when loading the package.
#'
#' @import utils
#' @export
#'
Key_checking <- function(){
  if(!file.exists('API_key.txt')){
    cli::cat_line("API key is required!",col = "red")
    cli::cat_line("See the following instruction for the Key:")
    cli::cat_bullet('1. Browse and Log In: \n      https://rapidapi.com/skyscanner/api/skyscanner-flight-search')
    cli::cat_line("Do you need me to open the browser? 1 for YES; 0 for NO")
    ANS = readline("")
    if (ANS == 1) {httr::BROWSE('https://rapidapi.com/skyscanner/api/skyscanner-flight-search')}
    cli::cat_bullet('2. Copy the X-RapidAPI-Key from the 3rd line in Request Snippet(Right Panel)')
    cli::cat_line("What's your key?(without quote)")
    API_key = readline("")
    if(API_key!=""){
    write.table(API_key, file = "API_key.txt", quote = FALSE,
                row.names = FALSE,col.names = FALSE)
    return(API_key)
    }
  }else{
    API_key = tryCatch(read.table(file = 'API_key.txt', stringsAsFactors = F), error=function(e) NULL)[1,1]
    URL = 'https://skyscanner-skyscanner-flight-search-v1.p.rapidapi.com/apiservices/reference/v1.0/currencies'
    header = MakeHeader("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",API_key)
    resp <- GET(URL, add_headers(header))
    if(suppressWarnings(CheckStatus(resp))){
      cli::cat_line("Check your API key or network connection")
    }else {
      return(API_key)
      cli::cat_line("Welcome to FlightScanner!")
    }
  }
}


