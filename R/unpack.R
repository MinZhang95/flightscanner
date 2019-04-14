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
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' GetPrice(resp)
#' }
GetPrice <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")

  tab <- GetItineraries(x, price = T)
  tab$SearchTime <- lubridate::with_tz(lubridate::ymd_hms(x$date, tz = "GMT"))
  select(tab, "SearchTime", everything())
}


#' Extract itinerary information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the ItineraryId (OutboundLegId, InboundLegId).
#'
#' @param x A request object.
#' @param price If TRUE, also includes PricingOptions information.
#'
#' @return A tibble.
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' GetItineraries(resp)
#' }
GetItineraries <- function(x, price = F) {
  if (inherits(x, "response")) x <- content(x)

  x$Itineraries %>% lapply(function(y) {
    tab <- tibble(OutboundLegId = y$OutboundLegId,
                  InboundLegId = ifelse(is.null(y$InboundLegId), as.character(NA), y$InboundLegId))
    if (price)
      tab$PricingOptions <- y$PricingOptions %>% lapply(function(z) {
        tibble(AgentId = z$Agents[[1]], Price = z$Price, LinkURL = z$DeeplinkUrl)
      }) %>% do.call(rbind, .) %>% arrange(!!sym("Price")) %>% list()
    tab
  }) %>% do.call(rbind, .)
}


#' Extract leg information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the LegId, SegmentIds, OriginId, DestinationId,
#' DepartureTime, ArrivalTime, Duration, No.Stops, Directionality, Stops, and FlightNumbers.
#'
#' Stops contains the StopId, and Layover.
#' FlightNumbers contains the FlightNumber, and CarriersId.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' GetLegs(resp)
#' }
GetLegs <- function(x) {
  if (inherits(x, "response")) x <- content(x)

  segments_info <- GetSegments(x)

  x$Legs %>% lapply(function(y) {
    n <- length(y$SegmentIds)
    time_info <- segments_info[unlist(y$SegmentIds) + 1, c("DepartureTime", "ArrivalTime")]
    layover <- lubridate::interval(time_info$ArrivalTime[-n], time_info$DepartureTime[-1]) %>%
      lubridate::as.duration() %>% as.numeric("minute")

    temp <- if (length(y$Stops) != n - 1) {
      warning("Unmatch of Segments and Stops: [LegId] = ", y$Id, call. = F)
      # return(NULL)
      unlist(y$Stops)[1:(n - 1)]
    } else unlist(y$Stops)

    tibble(Id = y$Id,
           SegmentIds = list(unlist(y$SegmentIds)),
           OriginId = y$OriginStation,
           DestinationId = y$DestinationStation,
           DepartureTime = lubridate::ymd_hms(y$Departure),
           ArrivalTime = lubridate::ymd_hms(y$Arrival),
           Duration = y$Duration,
           No.Stops = n - 1,
           Directionality = y$Directionality,
           Stops = data.frame(StopId = if (n - 1) temp else integer(0),
                              Layover = as.integer(layover)) %>% list(),
           FlightNumbers = lapply(y$FlightNumbers, data.frame, stringsAsFactors = F) %>%
             do.call(rbind, .) %>% list())
  }) %>% do.call(rbind, .)
}


#' Extract segment information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the SegmentId, OriginId, DestinationId, DepartureTime, ArrivalTime,
#' Duration, CarrierId, OperatingCarrierId, FlightNumber and Directionality.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' GetSegments(resp)
#' }
GetSegments <- function(x) {
  if (inherits(x, "response")) x <- content(x)

  x$Segments %>% lapply(as_tibble) %>% do.call(rbind, .) %>%
    select(-"JourneyMode") %>%
    rename(OriginId = "OriginStation", DestinationId = "DestinationStation",
           DepartureTime = "DepartureDateTime", ArrivalTime = "ArrivalDateTime",
           CarrierId = "Carrier", OperatingCarrierId = "OperatingCarrier") %>%
    mutate_at(vars("DepartureTime", "ArrivalTime"), lubridate::ymd_hms) %>%
    select("Id":"ArrivalTime", "Duration", everything())
}


#' Extract carrier information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the CarrierId, Code, Name, and ImageURL.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' GetCarriers(resp)
#' }
GetCarriers <- function(x) {
  if (inherits(x, "response")) x <- content(x)

  x$Carriers %>% lapply(as_tibble) %>% do.call(rbind, .) %>% select(-"DisplayCode")
}


#' Extract agent information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the AgentId, Name, ImageURL, and Type.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' GetAgents(resp)
#' }
GetAgents <- function(x) {
  if (inherits(x, "response")) x <- content(x)

  x$Agents %>% lapply(as_tibble) %>% do.call(rbind, .) %>% select(-"Status", -"OptimisedForMobile")
}


#' Extract place information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the PlaceId, ParentId, Code, Type and Name.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' SetAPI("skyscanner-skyscanner-flight-search-v1.p.rapidapi.com", "YOUR_API_KEY")
#' resp <- CreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- PollSession(respondPOST = resp)
#' GetPlaces(resp)
#' }
GetPlaces <- function(x) {
  if (inherits(x, "response")) x <- content(x)

  x$Places %>% lapply(function(y) {
    if (is.null(y$ParentId)) y$ParentId <- NA
    as_tibble(y)
  }) %>% do.call(rbind, .) %>% select("Id", "ParentId", everything())
}
