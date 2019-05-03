#' Set API hostname and key.
#' @description Set API hostname and key globally.
#'
#' See \url{https://rapidapi.com/skyscanner/api/skyscanner-flight-search}.
#'
#' @param host API hostname.
#' @param key API key.
#'
#' @return A list of hostname and key.
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' }
SetAPI <- function(host, key) {
  options(API = list(host = host, key = key))
}


#' Make headers to a request.
#'
#' @param host API hostname.
#' @param key API key.
#' @param type Content type.
#'
#' @return A character vector contains host, key, and type.
MakeHeader <- function(host, key, type) {
  if (missing(host)) host <- getOption("API")$host
  if (missing(key)) key <- getOption("API")$key
  if (missing(type)) type <- "application/x-www-form-urlencoded"
  c("X-RapidAPI-Host" = host, "X-RapidAPI-Key" = key, "Content-Type" = type)
}


#' Create session of live flight search.
#' @description Returns live prices from all our suppliers for the requested flight itinerary.
#' This function is POST step. Make sure you have set API using \code{\link{SetAPI}} before.
#'
#' See \url{https://rapidapi.com/skyscanner/api/skyscanner-flight-search}.
#'
#' @seealso \code{\link{PollSession}}.
#'
#' @param origin (REQUIRED) The origin place, can be country, city, airport, in Skyscanner code.
#' @param destination (REQUIRED) The destination, can be country, city, airport, in Skyscanner code.
#' @param startDate (REQUIRED) The outbound date. Format 'yyyy-mm-dd'.
#' @param returnDate (OPTIONAL) The return date. Format 'yyyy-mm-dd'. Use NULL for oneway trip.
#' @param adults (REQUIRED) Number of adults (16+ years). Must be between 1 and 8.
#' @param children (OPTIONAL) Number of children (1-16 years). Can be between 0 and 8.
#' @param infants (OPTIONAL) Number of infants (under 12 months). Can be between 0 and 8.
#' @param country (REQUIRED) The market/country your user is in.
#' @param currency (REQUIRED) The currency you want the prices in.
#' @param locale (REQUIRED) The locale you want the results in (ISO locale).
#' @param cabinClass (OPTIONAL) The cabin class. Can be 'economy', 'premiumeconomy', 'business', 'first'
#' @param includeCarriers (OPTIONAL) Only return results from those carriers. Comma-separated list of carrier ids.
#' @param excludeCarriers (OPTIONAL) Filter out results from those carriers. Comma-separated list of carrier ids.
#'
#' @return A \code{\link[httr:response]{response()}} object of request.
#' @import httr
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' }
CreateSession <- function(origin, destination, startDate, returnDate = NULL,
                          adults = 1, children = NULL, infants = NULL,
                          country = "US", currency = "USD", locale = "en-US",
                          cabinClass = "economy", includeCarriers = NULL, excludeCarriers = NULL) {
  # Add checking here.

  url <- paste0("https://", getOption("API")$host, "/apiservices/pricing/v1.0")
  header <- MakeHeader()
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
  checkmate::assertCharacter(c(origin, destination, country, currency, locale))
  checkmate::assertDate(c(ymd(startDate), ymd(returnDate)))
  if(!is.null(returnDate)){
  checkmate::assert_true(ymd(returnDate) > ymd(startDate))
  }
  checkmate::assert_numeric(adults)

  resp <- POST(url, add_headers(header), body = body, encode = "form")
  flag <- CheckStatus(resp)
  checkmate::assertClass(resp,"response")
  resp
}


#' Poll session of live flight search.
#' @description Returns live prices from all our suppliers for the requested flight itinerary.
#' This function is GET step. Make sure you have set API using \code{\link{SetAPI}} before.
#'
#' See \url{https://rapidapi.com/skyscanner/api/skyscanner-flight-search}.
#'
#' @seealso \code{\link{CreateSession}}.
#'
#' @param sessionKey The Session key to identify the session.
#' @param respondPOST Return object of \code{\link{CreateSession}}.
#' @param sortType (OPTIONAL) The parameter to sort results on.
#' Can be carrier, duration, outboundarrivetime, outbounddeparttime, inboundarrivetime, inbounddeparttime, price.
#' @param sortOrder (OPTIONAL) The sort order. 'asc' or 'desc'.
#' @param duration (OPTIONAL) Filter for maximum duration in minutes. Integer between 0 and 1800.
#' @param stops (OPTIONAL) Filter by number of stops. 0: direct flights only. 1: flights with one stop only.
#' To show all flights do not use (only supports values 0 and 1).
#' @param includeCarriers (OPTIONAL) Filter flights by the specified carriers.
#' Must be semicolon-separated IATA codes.
#' @param excludeCarriers (OPTIONAL) Filter flights by any but the specified carriers.
#' Must be semicolon-separated IATA codes.
#' @param originAirports (OPTIONAL) Origin airports to filter on. List of airport codes delimited by ';'.
#' @param destinationAirports (OPTIONAL) Destination airports to filter on. List of airport codes delimited by ';'.
#' @param outboundDepartTime (OPTIONAL) Filter for outbound departure time by time period of the day
#' (i.e. morning, afternoon, evening). List of day time period delimited by ';' (acceptable values are M, A, E).
#' @param outboundDepartStartTime (OPTIONAL) Filter for start of range for outbound departure time. Format 'hh:mm'.
#' @param outboundDepartEndTime (OPTIONAL) Filter for end of range for outbound departure time. Format 'hh:mm'.
#' @param outboundArriveStartTime (OPTIONAL) Filter for start of range for outbound arrival time. Format 'hh:mm'.
#' @param outboundArriveEndTime (OPTIONAL) Filter for end of range for outbound arrival time. Format 'hh:mm'.
#' @param inboundDepartTime (OPTIONAL) Filter for inbound departure time by time period of the day
#' (i.e. morning, afternoon, evening). List of day time period delimited by ';' (acceptable values are M, A, E).
#' @param inboundDepartStartTime (OPTIONAL) Filter for start of range for inbound departure time. Format 'hh:mm'.
#' @param inboundDepartEndTime (OPTIONAL) Filter for end of range for inbound departure time. Format 'hh:mm'.
#' @param inboundArriveStartTime (OPTIONAL) Filter for start of range for inbound arrival time. Format 'hh:mm'.
#' @param inboundArriveEndTime (OPTIONAL) Filter for end of range for inbound arrival time. Format 'hh:mm'.
#'
#' @return A \code{\link[httr:response]{response()}} object of request.
#' @import httr
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' PollSession(SessionKey(resp))
#' PollSession(respondPOST = resp)
#' }
PollSession <- function(sessionKey, respondPOST = NULL,
                        sortType = "price", sortOrder = "asc",
                        duration = NULL, stops = NULL,
                        includeCarriers = NULL, excludeCarriers = NULL,
                        originAirports = NULL, destinationAirports = NULL,
                        outboundDepartTime = NULL,
                        outboundDepartStartTime = NULL, outboundDepartEndTime = NULL,
                        outboundArriveStartTime = NULL, outboundArriveEndTime = NULL,
                        inboundDepartTime = NULL,
                        inboundDepartStartTime = NULL, inboundDepartEndTime = NULL,
                        inboundArriveStartTime = NULL, inboundArriveEndTime = NULL) {
  par.options <- list(sortType = c("price", "duration", "carrier",
                                   "outboundarrivetime", "outbounddeparttime",
                                   "inboundarrivetime", "inbounddeparttime"),
                      sortOrder = c("asc", "desc"))
  # Add checking here.
  checkmate::assert_choice(sortType, par.options$sortType)
  checkmate::assert_choice(sortOrder, par.options$sortOrder)

  if (missing(sessionKey)) sessionKey <- SessionKey(respondPOST)
  if (!missing(sortType)) sortType <- match.arg(sortType, par.options$sortType)
  if (!missing(sortOrder)) sortOrder <- match.arg(sortOrder, par.options$sortOrder)

  url <- paste0("https://", getOption("API")$host, "/apiservices/pricing/uk2/v1.0")
  header <- MakeHeader()
  path <- c(parse_url(url)$path, sessionKey)
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

  for (count in 0:100) {
    resp <- GET(url, add_headers(header), path = path, query = query)
    if (content(resp)$Status == "UpdatesComplete") break
  }
  if (count) message("Try to update data ", count, " times.")
  flag <- CheckStatus(resp)
  checkmate::assertClass(resp,"response")
  resp
}


#' Browse flight prices from the skyscanner cache.
#' @description Gets you information about flights from the skyscanner cache.
#' It might be slightly outdated in comparison to live search, but more detailed and immediate.
#' Make sure you have set API using \code{\link{SetAPI}} before.
#'
#' See \url{https://rapidapi.com/skyscanner/api/skyscanner-flight-search}.
#'
#' @details The \code{endpoint} argument:
#' \describe{
#'   \item{\code{'quotes'}}{
#'   Returns the cheapest quotes that meet your query.
#'   The prices come from our cached prices resulting from our users' searches.
#'   }
#'   \item{\code{'routes'}}{
#'   Similar to Browse Quotes but with the quotes grouped by routes.
#'   This provides the cheapest destinations (countries, cities or airports) from our cached data.
#'   }
#'   \item{\code{'dates'}}{
#'   Similar to Browse Quotes but with the quotes grouped by outbound and inbound date.
#'   Useful to find the lowest price for a given route, over either a month or a 12 month period.
#'   }
#' }
#'
#' @param endpoint Endpoint to choose. One of 'quotes', 'routes', 'dates'.
#' @param origin (REQUIRED) The origin place, can be country, city, airport, in Skyscanner code.
#' @param destination (REQUIRED) The destination, can be country, city, airport, in Skyscanner code.
#' @param startDate (REQUIRED) The outbound date. Format 'yyyy-mm-dd', 'yyyy-mm' or 'anytime'.
#' @param returnDate (OPTIONAL) The return date. Format 'yyyy-mm-dd', 'yyyy-mm' or 'anytime'.
#' Use NULL for oneway trip.
#' @param country (REQUIRED) The market country your user is in.
#' @param currency (REQUIRED) The currency you want the prices in.
#' @param locale (REQUIRED) The locale you want the results in (ISO locale).

#' @return A \code{\link[httr:response]{response()}} object of request.
#' @import httr
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' BrowseFlight("quotes", origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' BrowseFlight("routes", origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' BrowseFlight("dates", origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' }
BrowseFlight <- function(endpoint = c("quotes", "routes", "dates"),
                         origin, destination, startDate, returnDate = NULL,
                         country = "US", currency = "USD", locale = "en-US") {
  endpoint <- match.arg(endpoint)
  # Add checking here.

  url <- paste0("https://", getOption("API")$host, "/apiservices/browse", endpoint, "/v1.0")
  header <- MakeHeader()
  path <- c(parse_url(url)$path, country, currency, locale,
            paste0(origin, "-sky"), paste0(destination, "-sky"), startDate)
  query <- list(inboundpartialdate = returnDate)

  resp <- GET(url, add_headers(header), path = path, query = query)
  flag <- CheckStatus(resp)
  resp
}


#' Check status of request response.
#' @description Extract the http status code and convert it into a human readable message.
#' Give warning if has an error.
#'
#' @param x A \code{\link[httr:response]{response()}} object or a number.
#'
#' @return \code{TRUE} if has an error, otherwise \code{FALSE}.
CheckStatus <- function(x) {
  # warn_for_status(x)
  if (http_error(x)) {
    warning(http_status(x)$message)
    TRUE
  } else FALSE
}


#' Extract session key from request response.
#' @description Extract session key from request response.
#' The last value of location header contains the session key which is required when polling the session.
#'
#' @param x A \code{\link[httr:response]{response()}} object.
#'
#' @return Session key.
SessionKey <- function(x) {
  location <- headers(x)$location
  y <- strsplit(location, "/")[[1]]
  y[length(y)]
}
