#' Get data from source.
#' @description Get data from source.
#' Includes: price, itineraries, legs, segments, carriers, agents, and places.
#' Data are in \code{\link[tibble:tibble]{tibble()}} form.
#'
#' @param x An object to get data from.
#' @param ... Further arguments passed to methods.
#' @param lazy If \code{TRUE}, return lazy tibbles.
#'
#' @return A list of tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get data from API
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' GetData(resp)
#'
#' # Get data from SQLite database
#' con <- dbCreateDB(dbname = "flight.db")
#' GetData(con)
#' }
GetData <- function(x, ...) UseMethod("GetData")


#' @describeIn GetData Get data from API response.
#' @export
GetData.response <- function(x, ...) {
  list(price = GetPrice(x),
       itineraries = GetItineraries(x),
       legs = GetLegs(x),
       segments = GetSegments(x),
       carriers = GetCarriers(x),
       agents = GetAgents(x),
       places = GetPlaces(x))
}


#' @describeIn GetData Get data from SQLite connection.
#' @export
GetData.SQLiteConnection <- function(x, lazy = F, ...) {
  if (lazy) {
    list(price = tbl(x, from = "price"),
         itineraries = tbl(x, from = "itinerary"),
         legs = tbl(x, from = "leg"),
         segments = tbl(x, from = "segment"),
         carriers = tbl(x, from = "carrier"),
         agents = tbl(x, from = "agent"),
         places = tbl(x, from = "place"))
  } else {
    list(price = dbReadTable(x, "price") %>% as.tbl() %>%
           ListPack(mutate = T, vars = "PricingOptions", tz = NULL),
         itineraries = dbReadTable(x, "itinerary") %>% as.tbl(),
         legs = dbReadTable(x, "leg") %>% as.tbl() %>%
           ListPack(mutate = T, vars = c("SegmentIds", "Stops")),
         segments = dbReadTable(x, "segment") %>% as.tbl() %>%
           ListPack(mutate = T),
         carriers = dbReadTable(x, "carrier") %>% as.tbl(),
         agents = dbReadTable(x, "agent") %>% as.tbl(),
         places = dbReadTable(x, "place") %>% as.tbl())
  }
}
