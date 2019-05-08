#' API key.
#' @description Set or get API key in global options. To get the key, see
#' \url{https://rapidapi.com/skyscanner/api/skyscanner-flight-search}.
#'
#' @param key API key.
#' @export
#'
#' @examples
#' \dontrun{
#' apiSetKey("YOUR_API_KEY")
#' apiGetKey()
#' }
apiSetKey <- function(key) {
  options(APIkey = key)
}


#' @rdname apiSetKey
#' @return \code{apiGetKey} will return the key if it has been set, otherwise \code{NULL}.
#' @export
apiGetKey <- function() {
  getOption("APIkey")
}


#' Check the API key.
#' @description Check the API key. This function will automatically run when attach the package.
apiCheckKey <- function() {
  website <- "https://rapidapi.com/skyscanner/api/skyscanner-flight-search"
  filename <- "APIkey.txt"
  
  if (!is.null(apiGetKey())) {
    key <- apiGetKey()
  } else if (file.exists(filename)) {
    key <- tryCatch(utils::read.table(filename, stringsAsFactors = FALSE),
                    warning = function(w) {}, error = function(e) {})[1, 1]
  } else if (interactive()) {
    cat("API key is required!\nPlease follow the instructions to get the key:\n")
    cat("1. Browse and login: ", website, "\n")
    cat("   Do you want to visit this website (1 for YES; 0 for NO)?")
    if (readline() == 1) BROWSE(website)
    cat("2. Copy the value of X-RapidAPI-Key in Header Parameters.\n")
    cat("   Paste your key (without quote):")
    key <- readline()
  } else key <- ""
  
  header <- apiMakeHeader(key)
  url <- paste0("https://", header["X-RapidAPI-Host"], "/apiservices/reference/v1.0/currencies")
  resp <- GET(url, add_headers(header))
  
  if (suppressWarnings(apiCheckStatus(resp))) {
    apiSetKey(key)
    utils::write.table(key, filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
    if (interactive()) cat("Welcome to FlightScanner!\n")
  } else if (interactive()) {
    cat("Check your key or network connection. And use function `apiSetKey` to set key later.\n")
  }
}


#' Make headers to a request.
#' @param key API key.
#' @return A character vector contains host, key, and type.
#' @examples
#' flightscanner:::apiMakeHeader("YOUR_API_KEY")
apiMakeHeader <- function(key) {
  c("X-RapidAPI-Host" = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
    "X-RapidAPI-Key" = if (missing(key)) apiGetKey() else key,
    "Content-Type" = "application/x-www-form-urlencoded")
}


#' Check status of request response.
#' @description Extract the http status code and convert it into a human readable message. Give
#' warning if has an error.
#'
#' @param x A \code{\link[httr:response]{response()}} object or a number.
#' @return \code{FALSE} if has an error, otherwise \code{TRUE}.
#' 
#' @examples
#' flightscanner:::apiCheckStatus(200L)
#' flightscanner:::apiCheckStatus(404L)
apiCheckStatus <- function(x) {
  # warn_for_status(x)
  if (http_error(x)) {
    warning(http_status(x)$message)
    FALSE
  } else TRUE
}


#' Create session of live flight search.
#' @description Returns live prices from all our suppliers for the requested flight itinerary. This
#' function is POST step. Make sure you have set API using \code{\link{apiSetKey}} before.
#'
#' See \url{https://rapidapi.com/skyscanner/api/skyscanner-flight-search}.
#'
#' @seealso \code{\link{apiPollSession}}.
#'
#' @param origin (REQUIRED) The origin place, can be country, city, airport, in Skyscanner code.
#' @param destination (REQUIRED) The destination, can be country, city, airport, in Skyscanner code.
#' @param startDate (REQUIRED) The outbound date. Format \code{"yyyy-mm-dd"}.
#' @param returnDate (OPTIONAL) The return date. Format \code{"yyyy-mm-dd"}. Use NULL for oneway
#' trip.
#' @param adults (REQUIRED) Number of adults (16+ years). Must be between 1 and 8.
#' @param children (OPTIONAL) Number of children (1-16 years). Can be between 0 and 8.
#' @param infants (OPTIONAL) Number of infants (under 12 months). Can be between 0 and 8.
#' @param country (REQUIRED) The market/country your user is in.
#' @param currency (REQUIRED) The currency you want the prices in.
#' @param locale (REQUIRED) The locale you want the results in (ISO locale).
#' @param cabinClass (OPTIONAL) The cabin class.
#' Can be \code{"economy"}, \code{"premiumeconomy"}, \code{"business"}, \code{"first"}.
#' @param includeCarriers (OPTIONAL) Only return results from those carriers. Comma-separated list
#' of carrier ids.
#' @param excludeCarriers (OPTIONAL) Filter out results from those carriers. Comma-separated list of
#' carrier ids.
#'
#' @return A \code{\link[httr:response]{response()}} object of request.
#' @import httr
#' @export
#'
#' @examples
#' \dontrun{
#' apiSetKey("YOUR_API_KEY")
#' apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' }
apiCreateSession <- function(origin, destination, startDate, returnDate = NULL,
                             adults = 1, children = NULL, infants = NULL,
                             country = "US", currency = "USD", locale = "en-US",
                             cabinClass = c("economy", "premiumeconomy", "business", "first"),
                             includeCarriers = NULL, excludeCarriers = NULL) {
  cabinClass <- match.arg(cabinClass)
  checkmate::assert_character(origin, len = 1)
  checkmate::assert_character(destination, len = 1)
  checkmate::assert_character(country, len = 1)
  checkmate::assert_character(currency, len = 1)
  checkmate::assert_character(locale, len = 1)
  checkmate::assert_numeric(adults, lower = 0, len = 1)
  checkmate::assert_date(lubridate::ymd(startDate), len = 1)
  checkmate::assert_date(lubridate::ymd(returnDate), min.len = 0, max.len = 1)
  if (!is.null(returnDate))
    checkmate::assert_true(lubridate::ymd(returnDate) > lubridate::ymd(startDate))
  
  header <- apiMakeHeader()
  url <- paste0("https://", header["X-RapidAPI-Host"], "/apiservices/pricing/v1.0")
  body <- list(cabinClass = cabinClass,
               country = country,
               currency = currency,
               locale = locale,
               originPlace = paste0(origin, "-sky"),
               destinationPlace = paste0(destination, "-sky"),
               outboundDate = startDate,
               inboundDate = returnDate,
               adults = adults,
               children = children,
               infants = infants,
               includeCarriers = includeCarriers,
               excludeCarriers = excludeCarriers)
  
  resp <- POST(url, add_headers(header), body = body, encode = "form")
  flag <- apiCheckStatus(resp)
  checkmate::assert_class(resp, "response")
  resp
}


#' Poll session of live flight search.
#' @description Returns live prices from all our suppliers for the requested flight itinerary. This
#' function is GET step. Make sure you have set API using \code{\link{apiSetKey}} before.
#'
#' See \url{https://rapidapi.com/skyscanner/api/skyscanner-flight-search}.
#'
#' @seealso \code{\link{apiCreateSession}}.
#'
#' @param response Return object of \code{\link{apiCreateSession}}.
#' @param sortType (OPTIONAL) The parameter to sort results on. Can be \code{"price"},
#' \code{"duration"}, \code{"carrier"}, \code{"outboundarrivetime"}, \code{"outbounddeparttime"},
#' \code{"inboundarrivetime"}, \code{"inbounddeparttime"}.
#' @param sortOrder (OPTIONAL) The sort order. \code{"asc"} or \code{"desc"}.
#' @param duration (OPTIONAL) Filter for maximum duration in minutes. Integer between 0 and 1800.
#' @param stops (OPTIONAL) Filter by number of stops. 0: direct flights only. 1: flights with one
#' stop only. To show all flights do not use (only supports values 0 and 1).
#' @param includeCarriers (OPTIONAL) Filter flights by the specified carriers. Must be
#' semicolon-separated IATA codes.
#' @param excludeCarriers (OPTIONAL) Filter flights by any but the specified carriers. Must be
#' semicolon-separated IATA codes.
#' @param originAirports (OPTIONAL) Origin airports to filter on. List of airport codes delimited by
#' ';'.
#' @param destinationAirports (OPTIONAL) Destination airports to filter on. List of airport codes
#' delimited by ';'.
#' @param outboundDepartTime (OPTIONAL) Filter for outbound departure time by time period of the day
#' (i.e. morning, afternoon, evening). List of day time period delimited by ';' (acceptable values
#' are \code{"M"}, \code{"A"}, \code{"E"}).
#' @param outboundDepartStartTime (OPTIONAL) Filter for start of range for outbound departure time.
#' Format \code{"hh:mm"}.
#' @param outboundDepartEndTime (OPTIONAL) Filter for end of range for outbound departure time.
#' Format \code{"hh:mm"}.
#' @param outboundArriveStartTime (OPTIONAL) Filter for start of range for outbound arrival time.
#' Format \code{"hh:mm"}.
#' @param outboundArriveEndTime (OPTIONAL) Filter for end of range for outbound arrival time. Format
#' \code{"hh:mm"}.
#' @param inboundDepartTime (OPTIONAL) Filter for inbound departure time by time period of the day
#' (i.e. morning, afternoon, evening). List of day time period delimited by ';' (acceptable values
#' are \code{"M"}, \code{"A"}, \code{"E"}).
#' @param inboundDepartStartTime (OPTIONAL) Filter for start of range for inbound departure time.
#' Format \code{"hh:mm"}.
#' @param inboundDepartEndTime (OPTIONAL) Filter for end of range for inbound departure time. Format
#' \code{"hh:mm"}.
#' @param inboundArriveStartTime (OPTIONAL) Filter for start of range for inbound arrival time.
#' Format \code{"hh:mm"}.
#' @param inboundArriveEndTime (OPTIONAL) Filter for end of range for inbound arrival time. Format
#' \code{"hh:mm"}.
#'
#' @return A \code{\link[httr:response]{response()}} object of request.
#' @import httr
#' @export
#'
#' @examples
#' \dontrun{
#' apiSetKey("YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' apiPollSession(resp)
#' }
apiPollSession <- function(response, sortType = c("price", "duration", "carrier",
                                                  "outboundarrivetime", "outbounddeparttime",
                                                  "inboundarrivetime", "inbounddeparttime"),
                           sortOrder = c("asc", "desc"),
                           duration = NULL, stops = NULL,
                           includeCarriers = NULL, excludeCarriers = NULL,
                           originAirports = NULL, destinationAirports = NULL,
                           outboundDepartTime = NULL,
                           outboundDepartStartTime = NULL, outboundDepartEndTime = NULL,
                           outboundArriveStartTime = NULL, outboundArriveEndTime = NULL,
                           inboundDepartTime = NULL,
                           inboundDepartStartTime = NULL, inboundDepartEndTime = NULL,
                           inboundArriveStartTime = NULL, inboundArriveEndTime = NULL) {
  sortType <- match.arg(sortType)
  sortOrder <- match.arg(sortOrder)
  checkmate::assert_class(response, "response")
  
  header <- apiMakeHeader()
  url <- paste0("https://", header["X-RapidAPI-Host"], "/apiservices/pricing/uk2/v1.0")
  sessionkey <- utils::tail(strsplit(headers(response)$location, "/")[[1]], 1)
  path <- c(parse_url(url)$path, sessionkey)
  query <- list(sortType = sortType,
                sortOrder = sortOrder,
                duration = duration,
                stops = stops,
                includeCarriers = includeCarriers,
                excludeCarriers = excludeCarriers,
                originAirports = originAirports,
                destinationAirports = destinationAirports,
                outboundDepartTime = outboundDepartTime,
                outboundDepartStartTime = outboundDepartStartTime,
                outboundDepartEndTime = outboundDepartEndTime,
                outboundArriveStartTime = outboundArriveStartTime,
                outboundArriveEndTime = outboundArriveEndTime,
                inboundDepartTime = inboundDepartTime,
                inboundDepartStartTime = inboundDepartStartTime,
                inboundDepartEndTime = inboundDepartEndTime,
                inboundArriveStartTime = inboundArriveStartTime,
                inboundArriveEndTime = inboundArriveEndTime)
  
  for (count in 1:100) {
    resp <- GET(url, add_headers(header), path = path, query = query)
    if (content(resp)$Status == "UpdatesComplete") break
  }
  
  flag <- apiCheckStatus(resp)
  checkmate::assert_class(resp, "response")
  resp
}


#' Browse flight prices from the skyscanner cache.
#' @description Gets you information about flights from the skyscanner cache. It might be slightly
#' outdated in comparison to live search, but more detailed and immediate. Make sure you have set
#' API using \code{\link{apiSetKey}} before.
#'
#' See \url{https://rapidapi.com/skyscanner/api/skyscanner-flight-search}.
#'
#' @details The \code{endpoint} argument:
#' \describe{
#'   \item{\code{"quotes"}}{
#'   Returns the cheapest quotes that meet your query. The prices come from our cached prices
#'   resulting from our users' searches.
#'   }
#'   \item{\code{"routes"}}{
#'   Similar to Browse Quotes but with the quotes grouped by routes. This provides the cheapest
#'   destinations (countries, cities or airports) from our cached data.
#'   }
#'   \item{\code{"dates"}}{
#'   Similar to Browse Quotes but with the quotes grouped by outbound and inbound date. Useful to
#'   find the lowest price for a given route, over either a month or a 12 month period.
#'   }
#' }
#'
#' @param endpoint Endpoint to choose. One of \code{"quotes"}, \code{"routes"}, \code{"dates"}.
#' @param origin (REQUIRED) The origin place, can be country, city, airport, in Skyscanner code.
#' @param destination (REQUIRED) The destination, can be country, city, airport, in Skyscanner code.
#' @param startDate (REQUIRED) The outbound date. Format \code{"yyyy-mm-dd"}, \code{"yyyy-mm"} or
#' \code{"anytime"}.
#' @param returnDate (OPTIONAL) The return date. Format \code{"yyyy-mm-dd"}, \code{"yyyy-mm"} or
#' \code{"anytime"}. Use NULL for oneway trip.
#' @param country (REQUIRED) The market country your user is in.
#' @param currency (REQUIRED) The currency you want the prices in.
#' @param locale (REQUIRED) The locale you want the results in (ISO locale).

#' @return A \code{\link[httr:response]{response()}} object of request.
#' @import httr
#' @export
#'
#' @examples
#' \dontrun{
#' apiSetKey("YOUR_API_KEY")
#' apiBrowseFlight("quotes", origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' apiBrowseFlight("routes", origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' apiBrowseFlight("dates", origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' }
apiBrowseFlight <- function(endpoint = c("quotes", "routes", "dates"),
                            origin, destination, startDate, returnDate = NULL,
                            country = "US", currency = "USD", locale = "en-US") {
  endpoint <- match.arg(endpoint)
  checkmate::assert_character(origin, len = 1)
  checkmate::assert_character(destination, len = 1)
  checkmate::assert_character(country, len = 1)
  checkmate::assert_character(currency, len = 1)
  checkmate::assert_character(locale, len = 1)
  checkmate::assert_date(lubridate::ymd(startDate), len = 1)
  checkmate::assert_date(lubridate::ymd(returnDate), min.len = 0, max.len = 1)
  if(!is.null(returnDate))
    checkmate::assert_true(lubridate::ymd(returnDate) > lubridate::ymd(startDate))
  
  header <- apiMakeHeader()
  url <- paste0("https://", header["X-RapidAPI-Host"], "/apiservices/browse", endpoint, "/v1.0")
  path <- c(parse_url(url)$path, country, currency, locale,
            paste0(origin, "-sky"), paste0(destination, "-sky"), startDate)
  query <- list(inboundpartialdate = returnDate)
  
  resp <- GET(url, add_headers(header), path = path, query = query)
  flag <- apiCheckStatus(resp)
  checkmate::assert_class(resp, "response")
  resp
}
