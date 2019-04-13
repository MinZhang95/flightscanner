# rm(list = ls())
library(dplyr)
library(httr)
library(flightcrawl)

# devtools::document()
# usethis::use_readme_rmd()
# usethis::use_readme_md()
# usethis::use_gpl3_license("Yang Qiao")
# usethis::use_r()
# usethis::use_pipe()
# usethis::use_testthat()
# usethis::use_test("api")
# usethis::use_git()
# usethis::use_git_config()
# usethis::use_github(auth_token = ")
# usethis::use_package("httr")

devtools::document()




# country = "US"; currency = "USD"; locale = "en-US"
# url <- paste0("https://", getOption("API")$host, "/autosuggest/v1.0")
# header <- flightcrawl:::MakeHeader()
# path <- c(parse_url(url)$path, country, currency, locale)
# query <- list(id = "DTW-sky")
#
# resp.place <- GET(url, add_headers(header), path = path, query = query)
#
# resp.place$request
# content(resp.place)

# ----------------------------------------------------------------------------
# header information
SetAPI(host = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
       key = "3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")
getOption("API")


### Create session - Live Flight Search
resp.post <- CreateSession(orig = "DSM", dest = "DTTA", startDate = "2019-05-01", returnDate = NULL)
resp.post
SessionKey(resp.post)


### Poll session - Live Flight Search
resp.get <- PollSession(respondPOST = resp.post)
resp.get
headers(resp.get)

res <- content(resp.get)
names(res)

res$Query
res$Status

length(res$Itineraries)
res$Itineraries[[2]]  # outbound/inbound legs

res$Itineraries %>% lapply(function(x, link = F) {
  id <- x$PricingOptions %>% sapply(function(y) y$Price) %>% which.min
  which <- c("Price", "DeeplinkUrl")
  if (link == F) which <- which[which != "DeeplinkUrl"]
  data.frame(OutboundLegId = x$OutboundLegId, x$PricingOptions[[id]][which])
}) %>% do.call(rbind, .) %>% as.tbl

length(res$Legs)
res$Legs[[1]]  # multiple segements
length(res$Segments)
res$Segments[[1]]

headers(resp.get)$location
res$Carriers %>% lapply(function(x) data.frame(x[c("Id", "Name")])) %>% do.call(rbind, .)
res$Agents %>% lapply(function(x) data.frame(x[c("Id", "Name")])) %>% do.call(rbind, .)
res$Places %>% lapply(function(x) data.frame(x[c("Id", "Name")])) %>% do.call(rbind, .)
res$Currencies

res$Itineraries %>% lapply(function(x) {x$PricingOptions %>% lapply(function(y) y$Agents)}) %>% unlist

res$Legs %>% lapply(function(x) x$Carriers %>% unlist %>% paste(collapse = ",")) %>% unlist















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









