rm(list = ls())
library(flightscanner)

# usethis::use_readme_rmd()
# usethis::use_gpl3_license("Dapeng Hu;Yang Qiao;Min Zhang;Xin Zhang;Zerui Zhang")
# usethis::use_r("")
# usethis::use_pipe()
# usethis::use_testthat()
# usethis::use_test("")
# usethis::use_git()
# usethis::use_git_config()
# usethis::use_github(auth_token = "")
# usethis::use_package("httr")
# usethis::use_vignette("use_flightscanner", "How to use flightscanner")
# usethis::use_pkgdown()
# usethis::use_travis()
# usethis::use_coverage()

devtools::document()
pkgdown::build_site()

# ----------------------------------------------------------------------------
apiSetKey("3e85a0e43cmshac6dba6fde57066p1c1145jsn1e6f8c3d0e33")
apiGetKey()

### Create session - Live Flight Search
resp.post <- apiCreateSession(orig = "SEA", dest = "PVG", startDate = "2019-06-15")

### Poll session - Live Flight Search
resp.get <- apiPollSession(resp.post)
res <- httr::content(resp.get)

### Browse Quotes - Browse Flight Prices
resp.quote <- apiBrowseFlight("quotes", orig = "DSM", dest = "DTW", start = "2019-05-01", return = NULL)
httr::content(resp.quote)

### Browse Routes - Browse Flight Prices
resp.route <- apiBrowseFlight("routes", orig = "DSM", dest = "DTW", start = "2019-05-01", return = NULL)
httr::content(resp.route)

### Browse Dates - Browse Flight Prices
resp.date <- apiBrowseFlight("dates", orig = "DSM", dest = "DTW", start = "2019-05-01", return = NULL)
httr::content(resp.date)
