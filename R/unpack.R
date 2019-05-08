#' Extract price information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' @describeIn GetPrice Includes the SearchTime, ItineraryId (OutboundLegId, InboundLegId), and
#' PricingOptions. PricingOptions includes the AgentId, Price, and LinkURL.
#'
#' @param x A request object.
#' @param price If \code{TRUE}, also includes PricingOptions information.
#'
#' @return A tibble.
#' @import dplyr purrr
#'
#' @examples
#' \dontrun{
#' apiSetKey("YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- apiPollSession(resp)
#' flightscanner:::GetPrice(resp)
#' flightscanner:::GetItineraries(resp)
#' flightscanner:::GetLegs(resp)
#' flightscanner:::GetSegments(resp)
#' flightscanner:::GetCarriers(resp)
#' flightscanner:::GetAgents(resp)
#' flightscanner:::GetPlaces(resp)
#' }
GetPrice <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  df <- GetItineraries(x, price = TRUE)
  df$SearchTime <- lubridate::with_tz(lubridate::ymd_hms(x$date, tz = "GMT"))
  df <- select(df, "SearchTime", everything())
  checkmate::assert_tibble(df)
  df
}


#' @describeIn GetPrice Includes the ItineraryId (OutboundLegId, InboundLegId).
GetItineraries <- function(x, price = FALSE) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  df <- content(x)$Itineraries %>% map_df(function(y) {
    tab <- tibble(OutboundLegId = y$OutboundLegId,
                  InboundLegId = ifelse(is.null(y$InboundLegId), "", y$InboundLegId))
    if (price)
      tab$PricingOptions <- y$PricingOptions %>% map_df(function(z) {
        tibble(AgentId = z$Agents[[1]], Price = z$Price, LinkURL = z$DeeplinkUrl)
      }) %>% arrange(!!sym("Price")) %>% list()
    tab
  })
  checkmate::assert_tibble(df)
  df
}


#' @describeIn GetPrice Includes the LegId, SegmentIds, OriginId, DestinationId, DepartureTime,
#' ArrivalTime, Duration, No.Stops, and Stops. Stops includes the StopId, and Layover.
GetLegs <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  info <- GetSegments(x)
  x <- content(x)
  df <- x$Legs %>% map_df(function(y) {
    n <- length(y$SegmentIds)
    idx <- unlist(y$SegmentIds) + 1L
    
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
  
  checkmate::assert_tibble(df)
  df
}


#' @describeIn GetPrice Includes the SegmentId, OriginId, DestinationId, DepartureTime, ArrivalTime,
#' Duration, CarrierId, OperatingCarrierId, and FlightNumber.
GetSegments <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  fmt <- "%y%m%d%H%M"
  df <- content(x)$Segments %>% map_df(as_tibble) %>%
    select("Id":"ArrivalDateTime", "Duration", everything(), -"JourneyMode", -"Directionality") %>%
    mutate_at(c("DepartureDateTime", "ArrivalDateTime"), lubridate::ymd_hms) %>%
    mutate(Id = paste(!!sym("OriginStation"), format(!!sym("DepartureDateTime"), fmt),
                      !!sym("Carrier"), !!sym("DestinationStation"),
                      format(!!sym("ArrivalDateTime"), fmt), sep = "-")) %>%
    rename(OriginId = "OriginStation", DestinationId = "DestinationStation",
           DepartureTime = "DepartureDateTime", ArrivalTime = "ArrivalDateTime",
           CarrierId = "Carrier", OperatingCarrierId = "OperatingCarrier")
  
  checkmate::assert_tibble(df)
  df
}


#' @describeIn GetPrice Includes the CarrierId, Code, Name, and ImageURL.
GetCarriers <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  df <- content(x)$Carriers %>% map_df(as_tibble) %>%
    select(-"DisplayCode") %>% rename(ImageURL = "ImageUrl") %>%
    group_by(!!sym("Id")) %>% filter(row_number() == 1) %>% ungroup()
  checkmate::assert_tibble(df)
  df
}


#' @describeIn GetPrice Includes the AgentId, Name, ImageURL, and Type.
GetAgents <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  df <- content(x)$Agents %>% map_df(as_tibble) %>%
    select(-"ImageUrl", everything(), -"Status", -"OptimisedForMobile") %>%
    rename(ImageURL = "ImageUrl")
  checkmate::assert_tibble(df)
  df
}


#' @describeIn GetPrice Includes the PlaceId, ParentId, Code, Type and Name.
GetPlaces <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  df <- content(x)$Places %>% map_df(as_tibble) %>% select("Id", "ParentId", everything())
  checkmate::assert_tibble(df)
  df
}
