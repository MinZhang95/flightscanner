
flightscanner <img src='man/figures/logo.png' align="right" height="150" />
===========================================================================

[![996.icu](https://img.shields.io/badge/link-996.icu-red.svg)](https://996.icu) [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![Travis build status](https://travis-ci.org/MinZhang95/flightscanner.svg?branch=master)](https://travis-ci.org/MinZhang95/flightscanner) [![Coverage status](https://codecov.io/gh/MinZhang95/flightscanner/branch/master/graph/badge.svg)](https://codecov.io/github/MinZhang95/flightscanner?branch=master) [![HitCount](http://hits.dwyl.io/MinZhang95/flightscanner.svg)](http://hits.dwyl.io/MinZhang95/flightscanner)

Topic
-----

Web scraping/crawling flight information from SkyScanner API. (<https://www.skyscanner.com/>). Keeping track of flight price will help us choose best flights and save money without spending too much time on searching.

Website
-------

<https://minzhang95.github.io/flightscanner/>

Main Function
-------------

Given departure, destination and a range of acceptable departure date,

-   scrape and filter flights by user-defined constraints such as price, duration, number of stops, etc.
-   search flight data automatically based on a schedule.
-   visualize flight information by shiny.

User constraints may include *Price*, *Duration*, *Departure Time*, *Arrival Time*, *Stops*, *Layover*, *Airline*, etc.

Installation
------------

**Important Notice**: Now this package can only be installed on **Unix/Linux/MacOS**, since we use package `cronR` which can't be installed on Windows. Sorry about the inconvenience.

You can install the released version of flightscanner with:

``` r
devtools::install_github("MinZhang95/flightscanner")
```

Example
-------

To buy a ticket from Des Moines to Detroit for an adult on 2019-06-01 (today is 2019-05-09):

``` r
dsm2dtw_session <- apiCreateSession(origin = "DSM", destination = "DTW", startDate = "2019-06-01", adults = 1)

dsm2dtw_res <- apiPollSession(response = dsm2dtw_session, sortType = "price", sortOrder = "asc")

dsm2dtw_df <- dsm2dtw_res %>% flightGet()
```

To filter the available flights with a budget of $1,000, no more than 1 stop during the trip, and departure time not earlier than 8:00 am:

``` r
flightFilter(dsm2dtw_df, max_price = 1000, max_stops = 1, out_departure = c("08:00","24:00")) 
```

You can also run the Shiny App,

``` r
shiny::runApp(system.file(package = "flightscanner", "shiny"))
```

For more details, see the [vignette](https://minzhang95.github.io/flightscanner/articles/vignette.html).
