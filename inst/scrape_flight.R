rm(list = ls())
library(dplyr)
# library(httr)
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




# ----------------------------------------------------------------------------
# header information
SetAPI(host = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
       key = "3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")
getOption("API")


### Create session - Live Flight Search
resp.post <- CreateSession(orig = "SEA", dest = "PVG", startDate = "2019-05-01")
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
res$Itineraries[[1]]  # outbound/inbound legs
res$Legs[[1]]  # multiple segements
res$Segments[[1]]

GetPrice(resp.get)
GetItineraries(resp.get)
GetLegs(resp.get)
GetSegments(resp.get)
GetCarriers(resp.get)
GetAgents(resp.get)
GetPlaces(resp.get)







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









