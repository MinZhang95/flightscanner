# API Website: https://rapidapi.com/skyscanner/api/skyscanner-flight-search
library(dplyr)
library(httr)

# header information
host <- "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com"
key <- "3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33"  # 50/day
type <- "application/x-www-form-urlencoded"


### Create session - Live Flight Search
url.post <- "https://skyscanner-skyscanner-flight-search-v1.p.rapidapi.com/apiservices/pricing/v1.0"
header <- c("X-RapidAPI-Host" = host,
            "X-RapidAPI-Key" = key,
            "Content-Type" = type)

query <- list("cabinClass" = "economy",
              "children" = 0,
              "infants" = 0,
              "country" = "US",
              "currency" = "USD",
              "locale" = "en-US",
              "originPlace" = "DSM-sky",
              "destinationPlace" = "DTW-sky",
              "outboundDate" = "2019-05-01",
              "adults" = 1)
# body <- stringr::str_split(modify_url("", query = query), "\\?")[[1]][2]

resp.post <- POST(url.post, add_headers(.headers = header), body = query, encode = "form")
resp.post
resp.post$request
headers(resp.post)
glimpse(resp.post)
content(resp.post)


### Poll session - Live Flight Search
url.get <- "https://skyscanner-skyscanner-flight-search-v1.p.rapidapi.com/apiservices/pricing/uk2/v1.0/"
key.session <- stringr::word(resp.post$headers$location, -1, sep = "/")
# "8c38fc31-37fc-4fbe-bdb7-26348f5a79fe"

resp.get <- GET(url.get, add_headers(.headers = header), path = paste0(parse_url(url.get)$path, key.session))
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








### Browse Routes - Browse Flight Prices
url <- "https://skyscanner-skyscanner-flight-search-v1.p.rapidapi.com/apiservices/browseroutes/v1.0/US/USD/en-US/DSM-sky/DTW-sky/2019-05-01"
header <- c("X-RapidAPI-Host" = host,
            "X-RapidAPI-Key" = key)

resp <- GET(url, add_headers(.headers = header))
resp
headers(resp)
glimpse(resp)
content(resp)
