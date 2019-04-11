# API Website: https://rapidapi.com/skyscanner/api/skyscanner-flight-search
library(dplyr)
library(httr)

SetAPI <- function(host, key) {
  options(API = list(host = host, key = key))
}


MakeHeader <- function(host, key, type) {
  if (missing(host)) host <- getOption("API")$host
  if (missing(key)) key <- getOption("API")$key
  if (missing(type)) type <- "application/x-www-form-urlencoded"
  c("X-RapidAPI-Host" = host, "X-RapidAPI-Key" = key, "Content-Type" = type)
}


CreateSession <- function(origin, destination, startDate, returnDate = NULL,
                          adults = 1, children = NULL, infants = NULL,
                          country = "US", currency = "USD", locale = "en-US",
                          cabinClass = "economy", includeCarriers = NULL, excludeCarriers = NULL) {
  # Add checking here.

  url <- paste0("https://", getOption("API")$host, "/apiservices/pricing/v1.0")
  header <- MakeHeader()
  body <- list("cabinClass" = cabinClass,
               "country" = country,
               "currency" = currency,
               "locale" = locale,
               "originPlace" = paste0(origin, "-sky"),
               "destinationPlace" = paste0(destination, "-sky"),
               "outboundDate" = startDate,
               "inboundDate" = returnDate,
               "adults" = adults,
               "children" = children,
               "infants" = infants,
               "includeCarriers" = includeCarriers,
               "excludeCarriers" = excludeCarriers)

  resp <- POST(url, add_headers(header), body = body, encode = "form")
  flag <- CheckStatus(resp)
  resp
}


PollSession <- function(sessionKey, respondPOST = NULL) {
  if (missing(sessionKey))
    sessionKey <- SessionKey(respondPOST)

  url <- paste0("https://", getOption("API")$host, "/apiservices/pricing/uk2/v1.0")
  header <- MakeHeader()
  path <- c(parse_url(url)$path, sessionKey)

  resp <- GET(url, add_headers(header), path = path)
  flag <- CheckStatus(resp)
  resp
}


BrowseFlight <- function(endpoint = c("quotes", "routes", "dates"),
                         origin, destination, startDate, returnDate = NULL,
                         country = "US", currency = "USD", locale = "en-US") {
  endpoint <- match.arg(endpoint)
  # Add checking here.

  url <- paste0("https://", getOption("API")$host, "/apiservices/browse", endpoint, "/v1.0")
  header <- MakeHeader()
  path <- c(parse_url(url)$path, country, currency, locale,
            paste0(origin, "-sky"), paste0(destination, "-sky"), startDate)
  query <- list("inboundpartialdate" = returnDate)

  resp <- GET(url, add_headers(header), path = path, query = query)
  flag <- CheckStatus(resp)
  resp
}


# return 1 if error, else 0.
CheckStatus <- function(x) {
  # warn_for_status(x)
  if (http_error(x)) {
    warning(http_status(x)$message)
    1
  } else 0
}


SessionKey <- function(x) {
  location <- headers(x)$location
  last(strsplit(location, "/")[[1]])
}




########################################
########################################
# header information
SetAPI(host = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
       key = "3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")
getOption("API")


### Create session - Live Flight Search
resp.post <- CreateSession(orig = "DSM", dest = "DTW", startDate = "2019-05-01", returnDate = NULL)
resp.post
SessionKey(resp.post)


### Poll session - Live Flight Search
resp.get <- PollSession(respondPOST = resp.post)
resp.get
resp.get$request
headers(resp.get)
glimpse(resp.get)

res <- content(resp.get)
names(res)

res$Query
res$Itineraries[[1]]
res$Legs[[1]]
res$Segments[[1]]
res$Carriers[[1]]
res$Agents[[1]]
res$Places[[1]]
res$Currencies[[1]]


### Browse Quotes - Browse Flight Prices
resp.quote <- BrowseFlight("quotes", orig = "DSM", dest = "DTW", start = "2019-05-01", return = NULL)
resp.quote
headers(resp.quote)
glimpse(resp.quote)
content(resp.quote)


### Browse Routes - Browse Flight Prices
resp.route <- BrowseFlight("routes", orig = "DSM", dest = "DTW", start = "2019-05-01", return = NULL)
resp.route
headers(resp.route)
glimpse(resp.route)
content(resp.route)


### Browse Dates - Browse Flight Prices
resp.date <- BrowseFlight("dates", orig = "DSM", dest = "DTW", start = "2019-05-01", return = NULL)
resp.date
headers(resp.date)
glimpse(resp.date)
content(resp.date)
