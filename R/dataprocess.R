#' Get flight data from source.
#' @description Get flight data from source. Includes: price, itineraries, legs, segments, carriers,
#' agents, and places. Data are in \code{\link[tibble:tibble]{tibble()}} form.
#'
#' @param x An object to get data from.
#' @param ... Further arguments passed to methods.
#'
#' @return A list of tibbles.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get data from API
#' apiSetKey("YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- apiPollSession(resp)
#' flightGet(resp)
#'
#' # Get data from SQLite database
#' con <- dbCreateDB(dbname = "flight.db")
#' flightGet(con)
#' dbDisconnect(con)
#' }
flightGet <- function(x, ...) UseMethod("flightGet")


#' @describeIn flightGet Get flight data from API response.
#' @export
flightGet.response <- function(x, ...) {
  list(price = GetPrice(x),
       itineraries = GetItineraries(x),
       legs = GetLegs(x),
       segments = GetSegments(x),
       carriers = GetCarriers(x),
       agents = GetAgents(x),
       places = GetPlaces(x))
}


#' @describeIn flightGet Get flight data from SQLite connection.
#' @export
flightGet.SQLiteConnection <- function(x, ...) {
  list(
    price = dbReadTable(x, "price") %>% ListPack(mutate = TRUE, vars = "PricingOptions", tz = NULL),
    itineraries = dbReadTable(x, "itinerary"),
    legs = dbReadTable(x, "leg") %>% ListPack(mutate = TRUE, vars = c("SegmentIds", "Stops")),
    segments = dbReadTable(x, "segment") %>% ListPack(mutate = TRUE),
    carriers = dbReadTable(x, "carrier"),
    agents = dbReadTable(x, "agent"),
    places = dbReadTable(x, "place")
  ) %>% lapply(as.tbl)
}


#' Does the time fall in specified range?
#' @description This is a compare function of time. It extracts the hours, minutes and seconds to
#' check if it falls in specified time range.
#'
#' @param x A \code{POSIXt} class vector.
#' @param interval A character vector of boundary values, each element should be in format
#' \code{"hh:mm"}.
#' 
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' x <- lubridate::ymd_hms("2019-01-02 07:30:00")
#' flightscanner:::BetweenTime(x, c("7:30", "8:00"))
#' flightscanner:::BetweenTime(x, c("6:00", "7:29"))
BetweenTime <- function(x, interval) {
  checkmate::assert_posixct(x)
  checkmate::assert_character(interval, len = 2)
  x <- lubridate::as.duration(x - lubridate::floor_date(x, unit = "day"))
  x >= lubridate::hm(interval[1]) & x <= lubridate::hm(interval[2])
}


#' Is there any duplicate rows?
#' @description Check if there is any duplicate rows indexed by given columns. If YES, it will give
#' warning.
#'
#' @param .data A data.frame.
#' @param .vars Names of Columns to group by. If missing, use the first column name.
#'
#' @return \code{TRUE} if there exists duplicate rows, otherwise \code{FALSE}.
#'
#' @examples
#' df <- data.frame(Id = c(1, 1), name = c("A", "B"))
#' flightscanner:::CheckDuplicateRow(df)
#' flightscanner:::CheckDuplicateRow(df, "name")
CheckDuplicateRow <- function(.data, .vars) {
  checkmate::assert_data_frame(.data)
  if (missing(.vars)) .vars <- names(.data)[1]
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
#' @param out_departure Range of outbound departure time, \code{"hh:mm"} or numeric values in
#' minutes.
#' @param out_arrival Range of outbound arrival time, \code{"hh:mm"} or numeric values in minutes.
#' @param in_departure Range of inbound departure time, \code{"hh:mm"} or numeric values in minutes.
#' @param in_arrival Range of inbound arrival time, \code{"hh:mm"} or numeric values in minutes.
#'
#' @return A tibble of flight itineraries.
#' @export
#'
#' @examples
#' \dontrun{
#' apiSetKey("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR",
#'                          startDate = "2019-07-01", returnDate = "2019-07-10")
#' resp <- apiPollSession(resp)
#' data <- flightGet(resp)
#' flightFilter(data, max_price = 1000, max_duration = 60 * 24,
#'              max_stops = 2, layover = c(60, 240),
#'              carrier_include = c("UA", "AA", "DL", "CX", "NH"),
#'              carrier_exclude = c("MH", "KE"))
#' }
flightFilter <- function(x, max_price = Inf, max_duration = Inf,
                         max_stops = Inf, layover = c(0, Inf),
                         carrier_include = unique(x$carriers$Code), carrier_exclude = NULL,
                         out_departure = c("00:00", "24:00"), out_arrival = c("00:00", "24:00"),
                         in_departure = c("00:00", "24:00"), in_arrival = c("00:00", "24:00")) {
  checkmate::assert_numeric(max_price, lower = 0, len = 1)
  checkmate::assert_numeric(max_duration, lower = 0, len = 1)
  checkmate::assert_numeric(max_stops, lower = 0, len = 1)
  checkmate::assert_numeric(layover, lower = 0, len = 2)
  checkmate::assert_character(carrier_include)
  checkmate::assert_character(carrier_exclude, null.ok = T)
  checkmate::assert_vector(out_departure, len = 2)
  checkmate::assert_vector(out_arrival, len = 2)
  checkmate::assert_vector(in_departure, len = 2)
  checkmate::assert_vector(in_arrival, len = 2)
  
  f <- function(x) {
    if (is.numeric(x)) {  # if x is in unit of minutes
      x <- format(lubridate::ymd("1970-01-01") + lubridate::minutes(x), "%H:%M")
      if (x[2] == "00:00") x[2] <- "24:00"
    }
    x
  }
  
  dup <- mapply(CheckDuplicateRow, x, rep(list(c("OutboundLegId", "InboundLegId"), "Id"), c(2, 5)))
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
  
  res <- inner_join(price_info, itineraries_info, by = c("OutboundLegId", "InboundLegId"))
  checkmate::assert_tibble(res)
  res
}
