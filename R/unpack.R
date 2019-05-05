#' Extract price information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the SearchTime, ItineraryId (OutboundLegId, InboundLegId), and
#' PricingOptions.
#'
#' PricingOptions contains the AgentId, Price, and LinkURL.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr
#'
#' @seealso \code{\link{GetPrice}}, \code{\link{GetItineraries}}, \code{\link{GetLegs}},
#' \code{\link{GetSegments}}, \code{\link{GetCarriers}}, \code{\link{GetAgents}},
#' \code{\link{GetPlaces}}.
#'
#' @examples
#' \dontrun{
#' SetAPI("YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- apiPollSession(resp)
#' flightscanner:::GetPrice(resp)
#' }
GetPrice <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  df <- GetItineraries(x, price = TRUE)
  df$SearchTime <- lubridate::with_tz(lubridate::ymd_hms(x$date, tz = "GMT"))
  df <- select(df, "SearchTime", everything())
  checkmate::assert_tibble(df)
  df
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
#' @seealso \code{\link{GetPrice}}, \code{\link{GetItineraries}}, \code{\link{GetLegs}},
#' \code{\link{GetSegments}}, \code{\link{GetCarriers}}, \code{\link{GetAgents}},
#' \code{\link{GetPlaces}}.
#'
#' @examples
#' \dontrun{
#' SetAPI("YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- apiPollSession(resp)
#' flightscanner:::GetItineraries(resp)
#' }
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


#' Extract leg information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the LegId, SegmentIds, OriginId, DestinationId, DepartureTime, ArrivalTime,
#' Duration, No.Stops, and Stops.
#'
#' Stops contains the StopId, and Layover.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr purrr
#' 
#' @seealso \code{\link{GetPrice}}, \code{\link{GetItineraries}}, \code{\link{GetLegs}},
#' \code{\link{GetSegments}}, \code{\link{GetCarriers}}, \code{\link{GetAgents}},
#' \code{\link{GetPlaces}}.
#'
#' @examples
#' \dontrun{
#' SetAPI("YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- apiPollSession(resp)
#' flightscanner:::GetLegs(resp)
#' }
GetLegs <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  info <- GetSegments(x)
  x <- content(x)
  df <- x$Legs %>% map_df(function(y) {
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
  
  checkmate::assert_tibble(df)
  df
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
#' @seealso \code{\link{GetPrice}}, \code{\link{GetItineraries}}, \code{\link{GetLegs}},
#' \code{\link{GetSegments}}, \code{\link{GetCarriers}}, \code{\link{GetAgents}},
#' \code{\link{GetPlaces}}.
#'
#' @examples
#' \dontrun{
#' SetAPI("YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- apiPollSession(resp)
#' flightscanner:::GetSegments(resp)
#' }
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


#' Extract carrier information from request response.
#' @description Extract information from request response of live flight search endpoint.
#' Information includes the CarrierId, Code, Name, and ImageURL.
#'
#' @param x A request object.
#'
#' @return A tibble.
#' @import dplyr purrr
#' 
#' @seealso \code{\link{GetPrice}}, \code{\link{GetItineraries}}, \code{\link{GetLegs}},
#' \code{\link{GetSegments}}, \code{\link{GetCarriers}}, \code{\link{GetAgents}},
#' \code{\link{GetPlaces}}.
#'
#' @examples
#' \dontrun{
#' SetAPI("YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- apiPollSession(resp)
#' flightscanner:::GetCarriers(resp)
#' }
GetCarriers <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  df <- content(x)$Carriers %>% map_df(as_tibble) %>%
    select(-"DisplayCode") %>% rename(ImageURL = "ImageUrl") %>%
    group_by(!!sym("Id")) %>% filter(row_number() == 1) %>% ungroup()
  checkmate::assert_tibble(df)
  df
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
#' @seealso \code{\link{GetPrice}}, \code{\link{GetItineraries}}, \code{\link{GetLegs}},
#' \code{\link{GetSegments}}, \code{\link{GetCarriers}}, \code{\link{GetAgents}},
#' \code{\link{GetPlaces}}.
#'
#' @examples
#' \dontrun{
#' SetAPI("YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- apiPollSession(resp)
#' flightscanner:::GetAgents(resp)
#' }
GetAgents <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  df <- content(x)$Agents %>% map_df(as_tibble) %>%
    select(-"ImageUrl", everything(), -"Status", -"OptimisedForMobile") %>%
    rename(ImageURL = "ImageUrl")
  checkmate::assert_tibble(df)
  df
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
#' @seealso \code{\link{GetPrice}}, \code{\link{GetItineraries}}, \code{\link{GetLegs}},
#' \code{\link{GetSegments}}, \code{\link{GetCarriers}}, \code{\link{GetAgents}},
#' \code{\link{GetPlaces}}.
#'
#' @examples
#' \dontrun{
#' SetAPI("YOUR_API_KEY")
#' resp <- apiCreateSession(origin = "SFO", destination = "LHR", startDate = "2019-07-01")
#' resp <- apiPollSession(resp)
#' flightscanner:::GetPlaces(resp)
#' }
GetPlaces <- function(x) {
  if (!inherits(x, "response")) stop("x should be a response() object.")
  
  df <- content(x)$Places %>% map_df(as_tibble) %>% select("Id", "ParentId", everything())
  checkmate::assert_tibble(df)
  df
}
