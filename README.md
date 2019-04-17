
flightscanner
=============

[![996.icu](https://img.shields.io/badge/link-996.icu-red.svg)](https://996.icu) [![LICENSE](https://img.shields.io/badge/license-Anti%20996-blue.svg)](https://github.com/996icu/996.ICU/blob/master/LICENSE)

Topic
-----

Web scraping/crawling flight information from Google Flights (<https://www.google.com/flights>). Keeping track of flight price will help us choose best flights and save money without spending too much time on searching.

Main Function
-------------

Given departure, destination and a range of acceptable departure date,

-   scrape flight information and price every day or every several hours
-   arrange the flights by price and user-provided constraints.
-   visualize flight information

Useful information may include *Price*, *Duration*, *Departure Time*, *Arrival Time*, *Stops*, *Layover*, *Airline*, Flight Number, Class, Bags, etc.

Installation
------------

You can install the released version of flightscanner from [CRAN](https://CRAN.R-project.org) with:

``` r
devtools::install_github("MinZhang95/flightscanner")
```

Example
-------
