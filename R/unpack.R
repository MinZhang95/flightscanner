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

  x$Itineraries %>% purrr::map_df(function(y) {
    tab <- tibble(OutboundLegId = y$OutboundLegId,
                  InboundLegId = ifelse(is.null(y$InboundLegId), "", y$InboundLegId))
    if (price)
      tab$PricingOptions <- y$PricingOptions %>% purrr::map_df(function(z) {
        tibble(AgentId = z$Agents[[1]], Price = z$Price, LinkURL = z$DeeplinkUrl)
      }) %>% arrange(!!sym("Price")) %>% list()
    tab
  })
}


#' Extract leg information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the LegId, SegmentIds, OriginId, DestinationId,
#' DepartureTime, ArrivalTime, Duration, No.Stops, Directionality, and Stops.
#'
#' Stops contains the StopId, and Layover.
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

  x$Legs %>% purrr::map_df(function(y) {
    n <- length(y$SegmentIds)

    stopId <- if (n == 1) {
      integer(0)
    } else if (length(y$Stops) != n - 1) {
      warning("Unmatch of Segments and Stops: [LegId] = ", y$Id, call. = F)
      # return(NULL)
      unlist(y$Stops)[1:(n - 1)]
    } else unlist(y$Stops)

    time_info <- segments_info[unlist(y$SegmentIds) + 1, c("DepartureTime", "ArrivalTime")]
    layover <- lubridate::interval(time_info$ArrivalTime[-n], time_info$DepartureTime[-1]) %>%
      lubridate::as.duration() %>% as.numeric("minute")

    tibble(Id = y$Id,
           SegmentIds = list(segments_info$Id[unlist(y$SegmentIds) + 1]),
           OriginId = y$OriginStation,
           DestinationId = y$DestinationStation,
           DepartureTime = lubridate::ymd_hms(y$Departure),
           ArrivalTime = lubridate::ymd_hms(y$Arrival),
           Duration = y$Duration,
           No.Stops = n - 1L,
           Directionality = y$Directionality,
           Stops = list(data.frame(StopId = stopId, Layover = as.integer(layover))))
  })
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

  fmt <- "%y%m%d%H%M"

  x$Segments %>% purrr::map_df(as_tibble) %>%
    select("Id":"ArrivalDateTime", "Duration", everything(), -"JourneyMode") %>%
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

  x$Carriers %>% purrr::map_df(as_tibble) %>%
    select(-"DisplayCode") %>% rename(ImageURL = "ImageUrl")
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

  x$Agents %>% purrr::map_df(as_tibble) %>%
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

  x$Places %>% purrr::map_df(as_tibble) %>% select("Id", "ParentId", everything())
}
