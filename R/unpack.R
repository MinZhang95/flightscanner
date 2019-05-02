#' Extract price information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the SearchTime, ItineraryId (OutboundLegId, InboundLegId), and PricingOptions.
#'
#' PricingOptions contains the AgentId, Price, and LinkURL.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' flightscanner:::GetPrice(resp)
#' }
GetPrice <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  tab <- GetItineraries(x, price = TRUE)
  tab$SearchTime <- lubridate::with_tz(lubridate::ymd_hms(x$date, tz = "GMT"))
  select(tab, "SearchTime", everything())
}


#' Extract itinerary information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the ItineraryId (OutboundLegId, InboundLegId).
#'
#' @param x A request object.
#' @param price If \code{TRUE}, also includes PricingOptions information.
#'
#' @return A tibble.
#' @import dplyr purrr
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' flightscanner:::GetItineraries(resp)
#' }
GetItineraries <- function(x, price = FALSE) {
  if (inherits(x, "response")) x <- content(x)
  
  x$Itineraries %>% map_df(function(y) {
    tab <- tibble(OutboundLegId = y$OutboundLegId,
                  InboundLegId = ifelse(is.null(y$InboundLegId), "", y$InboundLegId))
    if (price)
      tab$PricingOptions <- y$PricingOptions %>% map_df(function(z) {
        tibble(AgentId = z$Agents[[1]], Price = z$Price, LinkURL = z$DeeplinkUrl)
      }) %>% arrange(!!sym("Price")) %>% list()
    tab
  })
}


#' Extract leg information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the LegId, SegmentIds, OriginId, DestinationId,
#' DepartureTime, ArrivalTime, Duration, No.Stops, and Stops.
#'
#' Stops contains the StopId, and Layover.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr purrr
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' flightscanner:::GetLegs(resp)
#' }
GetLegs <- function(x) {
  if (inherits(x, "response")) x <- content(x)
  
  info <- GetSegments(x)
  
  x$Legs %>% map_df(function(y) {
    n <- length(y$SegmentIds)
    idx <- unlist(y$SegmentIds) + 1
    
    stopId <- if (n == 1) {
      integer(0)
    } else if (length(y$Stops) != n - 1) {
      warning("Unmatch of Segments and Stops: [LegId] = ", y$Id, call. = FALSE)
      # return(NULL)
      unlist(y$Stops)[1:(n - 1)]
    } else unlist(y$Stops)
    
    layover <- lubridate::interval(info$ArrivalTime[idx][-n], info$DepartureTime[idx][-1]) %>%
      lubridate::as.duration() %>% as.numeric("minute") %>% as.integer()
    
    tibble(Id = y$Id,
           SegmentIds = list(info$Id[idx]),
           OriginId = y$OriginStation,
           DestinationId = y$DestinationStation,
           DepartureTime = lubridate::ymd_hms(y$Departure),
           ArrivalTime = lubridate::ymd_hms(y$Arrival),
           Duration = y$Duration,
           No.Stops = n - 1L,
           Stops = list(data.frame(StopId = stopId, Layover = layover)))
  }) %>% group_by(!!sym("Id")) %>% filter(row_number() == n()) %>% ungroup()
  # select the last row of duplicated Ids
}


#' Extract segment information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the SegmentId, OriginId, DestinationId, DepartureTime, ArrivalTime,
#' Duration, CarrierId, OperatingCarrierId, and FlightNumber.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr purrr
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' flightscanner:::GetSegments(resp)
#' }
GetSegments <- function(x) {
  if (inherits(x, "response")) x <- content(x)
  
  fmt <- "%y%m%d%H%M"
  x$Segments %>% map_df(as_tibble) %>%
    select("Id":"ArrivalDateTime", "Duration", everything(), -"JourneyMode", -"Directionality") %>%
    mutate_at(c("DepartureDateTime", "ArrivalDateTime"), lubridate::ymd_hms) %>%
    mutate(Id = paste(!!sym("OriginStation"), format(!!sym("DepartureDateTime"), fmt), !!sym("Carrier"),
                      !!sym("DestinationStation"), format(!!sym("ArrivalDateTime"), fmt), sep = "-")) %>%
    rename(OriginId = "OriginStation", DestinationId = "DestinationStation",
           DepartureTime = "DepartureDateTime", ArrivalTime = "ArrivalDateTime",
           CarrierId = "Carrier", OperatingCarrierId = "OperatingCarrier")
}


#' Extract carrier information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the CarrierId, Code, Name, and ImageURL.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr purrr
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' flightscanner:::GetCarriers(resp)
#' }
GetCarriers <- function(x) {
  if (inherits(x, "response")) x <- content(x)
  
  x$Carriers %>% map_df(as_tibble) %>%
    select(-"DisplayCode") %>% rename(ImageURL = "ImageUrl") %>%
    group_by(!!sym("Id")) %>% filter(row_number() == 1) %>% ungroup()
}


#' Extract agent information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the AgentId, Name, ImageURL, and Type.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr purrr
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' flightscanner:::GetAgents(resp)
#' }
GetAgents <- function(x) {
  if (inherits(x, "response")) x <- content(x)
  
  x$Agents %>% map_df(as_tibble) %>%
    select(-"ImageUrl", everything(), -"Status", -"OptimisedForMobile") %>%
    rename(ImageURL = "ImageUrl")
}


#' Extract place information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the PlaceId, ParentId, Code, Type and Name.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr purrr
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' flightscanner:::GetPlaces(resp)
#' }
GetPlaces <- function(x) {
  if (inherits(x, "response")) x <- content(x)
  
  x$Places %>% map_df(as_tibble) %>% select("Id", "ParentId", everything())
}
