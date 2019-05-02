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
GetData.SQLiteConnection <- function(x, lazy = FALSE, ...) {
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
           ListPack(mutate = TRUE, vars = "PricingOptions", tz = NULL),
         itineraries = dbReadTable(x, "itinerary") %>% as.tbl(),
         legs = dbReadTable(x, "leg") %>% as.tbl() %>%
           ListPack(mutate = TRUE, vars = c("SegmentIds", "Stops")),
         segments = dbReadTable(x, "segment") %>% as.tbl() %>%
           ListPack(mutate = TRUE),
         carriers = dbReadTable(x, "carrier") %>% as.tbl(),
         agents = dbReadTable(x, "agent") %>% as.tbl(),
         places = dbReadTable(x, "place") %>% as.tbl())
  }
}


#' Does the time fall in specified range?
#' @description This is a compare function of time.
#' It extracts the hours, minutes and seconds to check if it falls in specified time range.
#'
#' @param x A \code{POSIXt} class vector.
#' @param interval A character vector of boundary values,
#' each element should be in format: \code{"\%H:\%M"}.
#' 
#' @return \code{TRUE} or \code{FALSE}.
#' @export
#'
#' @examples
#' x <- lubridate::ymd_hms("2019-01-02 07:30:00")
#' BetweenTime(x, c("7:30", "8:00"))
#' BetweenTime(x, c("6:00", "7:29"))
BetweenTime <- function(x, interval) {
  x <- lubridate::as.duration(x - lubridate::floor_date(x, unit = "day"))
  x >= lubridate::hm(interval[1]) & x <= lubridate::hm(interval[2])
}


#' Is there any duplicate rows?
#' @description Check if there is any duplicate rows indexed by given columns.
#' If YES, it will give warning.
#'
#' @param .data A data.frame.
#' @param .vars Names of Columns to group by. If missing, use the first column name.
#'
#' @return \code{TRUE} if there exists duplicate rows, otherwise \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' # Get data from API
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' data <- GetData(resp)
#' sapply(data, flightscanner:::CheckDuplicate)
#' }
CheckDuplicate <- function(.data, .vars) {
  name <- names(.data)
  
  if (!missing(.vars)) {
  } else if ((.vars <- "Id") %in% name) {
  } else if (all((.vars <- c("OutboundLegId", "InboundLegId")) %in% name)) {
  } else .vars <- name[1]
  
  x <- filter(group_by(.data, !!!syms(.vars)), n() > 1)
  if (NROW(x)) {
    warning(deparse(substitute(.data)), " has duplicate ",
            paste(.vars, collapse = ", "), ".", call. = FALSE)
    TRUE
  } else FALSE
}


#' Filter flights with matching conditions.
#' @description Choose flight itineraries that match the filter conditions.
#'
#' @param x A list of data.frame.
#' @param max_price Maximum price.
#' @param max_duration Maximum duration in minutes, applied to both outbound and inbound legs.
#' @param max_stops Maximum number of stops, applied to both outbound and inbound legs.
#' @param layover Range of layover in minutes, applied to each stop.
#' @param carrier_include Include specified carriers, applied to both outbound and inbound legs.
#' Must be IATA codes.
#' @param carrier_exclude Exclude specified carriers, applied to both outbound and inbound legs.
#' Must be IATA codes.
#' @param out_departure Range of outbound departure time, \code{"hh:mm"} or numeric values in minutes.
#' @param out_arrival Range of outbound arrival time, \code{"hh:mm"} or numeric values in minutes.
#' @param in_departure Range of inbound departure time, \code{"hh:mm"} or numeric values in minutes.
#' @param in_arrival Range of inbound arrival time, \code{"hh:mm"} or numeric values in minutes.
#'
#' @return A tibble of flight itineraries.
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR",
#'                       startDate = "2019-07-01", returnDate = "2019-07-10")
#' resp <- PollSession(respondPOST = resp)
#' data <- GetData(resp)
#' FilterFlight(data, max_price = 1000, max_duration = 60 * 24,
#'              max_stops = 2, layover = c(60, 240),
#'              carrier_include = c("UA", "AA", "DL", "CX", "NH"),
#'              carrier_exclude = c("MH", "KE"))
#' }
FilterFlight <- function(x, max_price = Inf, max_duration = Inf,
                         max_stops = Inf, layover = c(0, Inf),
                         carrier_include = unique(x$carriers$Code), carrier_exclude = NULL,
                         out_departure = c("00:00", "24:00"), out_arrival = c("00:00", "24:00"),
                         in_departure = c("00:00", "24:00"), in_arrival = c("00:00", "24:00")) {
  f <- function(x) {
    if (is.numeric(x)) {
      x <- format(as.POSIXct(60 * x, origin = "1970-01-01", tz = "UTC"), "%H:%M")
      if (x[2] == "00:00") x[2] <- "24:00"
    }
    x
  }
  
  # check duplicate Id
  dup <- sapply(x, CheckDuplicate)
  if (any(dup)) warning("Duplicate Ids are found in table: ",
                        paste(names(which(dup)), collapse = ", "), ".", call. = FALSE)
  
  # add carrier information
  segments_info <- x$segments %>%
    inner_join(rename_all(x$carriers, ~ paste0("Carrier", .)), by = "CarrierId") %>%
    inner_join(rename_all(x$carriers, ~ paste0("OperatingCarrier", .)), by = "OperatingCarrierId")
  
  # filter legs by duration, no.stops, layover
  legs_info <- x$legs %>%
    select(-"OriginId", -"DestinationId") %>%
    filter(!!sym("Duration") <= max_duration
           & !!sym("No.Stops") <= max_stops
           & map_lgl(!!sym("Stops"), ~ all(between(as.numeric(.$Layover), layover[1], layover[2]))))
  
  # filter legs by carrier
  legs_info <- legs_info %>%
    mutate_at("SegmentIds", ~ map(., ~ left_join(data.frame(Id = ., stringsAsFactors = FALSE),
                                                 segments_info, by = "Id"))) %>%
    rename(Segments = "SegmentIds") %>%
    filter(map_lgl(!!sym("Segments"), ~ any(.$CarrierCode %in% toupper(carrier_include))
                   & all(!.$CarrierCode %in% toupper(carrier_exclude))))
  
  # filter itineraries by time
  my_join <- if (all(x$itineraries$InboundLegId == "")) left_join else inner_join
  itineraries_info <- x$itineraries %>%
    inner_join(rename_all(legs_info, ~ paste0("OutboundLeg", .)), by = "OutboundLegId") %>%
    my_join(rename_all(legs_info, ~ paste0("InboundLeg", .)), by = "InboundLegId") %>%
    filter(BetweenTime(!!sym("OutboundLegDepartureTime"), f(out_departure))
           & BetweenTime(!!sym("OutboundLegArrivalTime"), f(out_arrival))
           & (BetweenTime(!!sym("InboundLegDepartureTime"), f(in_departure))
              & BetweenTime(!!sym("InboundLegArrivalTime"), f(in_arrival))
              | !!sym("InboundLegId") == ""))
  
  # filter price
  price_info <- x$price %>%
    select(-"SearchTime") %>%
    filter(map_lgl(!!sym("PricingOptions"), ~ min(.$Price) <= max_price)) %>%
    mutate_at("PricingOptions", ~ map(., ~ filter(., !!sym("Price") <= max_price)))
  
  inner_join(price_info, itineraries_info, by = c("OutboundLegId", "InboundLegId"))
}
