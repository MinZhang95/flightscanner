rm(list = ls())
library(flightscanner)

# usethis::use_readme_rmd()
# usethis::use_gpl3_license("Dapeng Hu;Yang Qiao;Min Zhang;Xin Zhang;Zerui Zhang")
# usethis::use_r("")
# usethis::use_pipe()
# usethis::use_testthat()
# usethis::use_test("api")
# usethis::use_git()
# usethis::use_git_config()
# usethis::use_github(auth_token = "")
# usethis::use_package("httr")
# usethis::use_pkgdown()
usethis::use_travis()
usethis::use_coverage()

devtools::document()
pkgdown::build_site()


# ----------------------------------------------------------------------------
# header information
SetAPI(host = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com",
       key = "3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")
# getOption("API")


### Create session - Live Flight Search
resp.post <- CreateSession(orig = "SEA", dest = "PVG", startDate = "2019-06-15")
# resp.post


### Poll session - Live Flight Search
resp.get <- PollSession(respondPOST = resp.post)
# resp.get

res <- httr::content(resp.get)


### Browse Quotes - Browse Flight Prices
resp.quote <- BrowseFlight("quotes", orig = "DSM", dest = "DTW", start = "2019-05-01", return = NULL)
resp.quote
httr::headers(resp.quote)
glimpse(resp.quote)
httr::content(resp.quote)


### Browse Routes - Browse Flight Prices
resp.route <- BrowseFlight("routes", orig = "DSM", dest = "DTW", start = "2019-05-01", return = NULL)
resp.route
httr::headers(resp.route)
glimpse(resp.route)
httr::content(resp.route)


### Browse Dates - Browse Flight Prices
resp.date <- BrowseFlight("dates", orig = "DSM", dest = "DTW", start = "2019-05-01", return = NULL)
resp.date
httr::headers(resp.date)
glimpse(resp.date)
httr::content(resp.date)
