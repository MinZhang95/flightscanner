library(lubridate)
library(dplyr)
library(tidyr)

### data is the result from the scraping data <- GetData(resp.get)
### need to be improve into one table


Out <- data$legs %>% filter(Directionality == 'Outbound') %>% 
  select("Id","DepartureTime","ArrivalTime","Duration","No.Stops","Stops") %>% 
  magrittr::set_names(c("OutboundLegId","Out_DepartureTime","Out_ArrivalTime","Out_Duration","Out_No.Stops","Out_Stops"))


In <- data$legs %>% filter(Directionality == 'Inbound') %>% 
  select("Id","DepartureTime","ArrivalTime","Duration","No.Stops","Stops") %>% 
  magrittr::set_names(c("InboundLegId","In_DepartureTime","In_ArrivalTime","In_Duration","In_No.Stops","In_Stops"))

seg_carrier <- left_join(data$segments %>% 
                   select(Id, CarrierId),
                 data$carriers %>% 
                   select(Id,Code,Name) %>% 
                   magrittr::set_names(c("CarrierId",'Code','Name')),by = "CarrierId")

segment_info <- data$legs %>% select(Id,SegmentIds) %>% 
                    unnest(SegmentIds) %>% 
                    left_join(seg_carrier,by=c("SegmentIds"= "Id")) %>% 
                    select(Id, CarrierId,Code,Name) %>% 
                    magrittr::set_names(c("LegId","CarrierId","CarrierCode","CarrierName")) %>% 
                    nest(-LegId) %>% magrittr::set_names(c('LegId','CarrierInfo'))

flight <- data$itineraries %>% 
  left_join(Out,by="OutboundLegId") %>% 
  left_join(In,by="InboundLegId") %>% 
  left_join(data$price %>% select(-SearchTime),by=c("OutboundLegId","InboundLegId")) %>% 
  left_join(segment_info,by=c("OutboundLegId"="LegId", "InboundLegId"="LegId"))

#default options
filter_Options <- list(Out_Stops = Inf,
                       In_Stops = Inf,
                       Out_Duration = Inf,
                       In_Duration = Inf,
                       Out_DepartureTime = c(hm('00:00'),hm('24:00')),
                       Out_ArrivalTime = c(hm('00:00'),hm('24:00')),
                       In_DepartureTime = c(hm('00:00'),hm('24:00')),
                       In_ArrivalTime = c(hm('00:00'),hm('24:00')),
                       price = Inf,
                       Airline_ex = NULL)

# flight %>%
#   filter(Out_Stops <= filter_Options$Out_Stops,
#   In_Stops <= filter_Options$In_Stops,
#   Out_Duration <= filter_Options$Out_Duration,
#   In_Duration <= filter_Options$In_Duration) %>%
#   filter_at("Out_DepartureTime",
#             ~ . >= filter_Options$Out_DepartureTime[1] &
#               . <= filter_Options$Out_DepartureTime[2]) %>%
#   filter_at("Out_ArrivalTime",
#             ~ . >= filter_Options$Out_ArrivalTime[1] &
#               . <= filter_Options$Out_ArrivalTime[2]) %>%
#   filter_at("In_DepartureTime",
#             ~ . >= filter_Options$In_DepartureTime[1] &
#               . <= filter_Options$In_DepartureTime[2]) %>%
#   filter_at("In_ArrivalTime",
#             ~ . >= filter_Options$In_ArrivalTime[1] &
#               . <= filter_Options$In_ArrivalTime[2]) ->
#   filtered_time_flight

#default options
filter_Options <- list(Out_Stops = 2,
                       In_Stops = 2,
                       Out_Duration = 1500,
                       In_Duration = 1500,
                       Out_DepartureTime = c(hm('00:00'),hm('24:00')),
                       Out_ArrivalTime = c(hm('00:00'),hm('24:00')),
                       In_DepartureTime = c(hm('00:00'),hm('24:00')),
                       In_ArrivalTime = c(hm('00:00'),hm('24:00')),
                       price = Inf,
                       Airline_ex = c('HU','MU'))

filter_Options <- 
  c(filter_Options,
       list(Airline_in = unique(seg_carrier$Code)))

flight %>% filter(Out_No.Stops <= filter_Options$Out_Stops,
                  Out_Duration <= filter_Options$Out_Duration, 
                  Out_DepartureTime >= filter_Options$Out_DepartureTime[1], 
                  Out_DepartureTime <= filter_Options$Out_DepartureTime[2]) %>% 
  unnest(PricingOptions,.drop=FALSE) %>% 
  unnest(CarrierInfo,.drop=FALSE) %>% 
  filter(Price <= filter_Options$price,
         !CarrierCode %in% filter_Options$Airline_ex)->final_filtered

if (trip_type == 2) {
  final_filtered %>% filter(In_No.Stops <= filter_Options$In_Stops,
                            In_Duration <= filter_Options$In_Duration,
                            In_DepartureTime >= filter_Options$In_DepartureTime[1], 
                            In_DepartureTime <= filter_Options$In_DepartureTime[2]) -> final_filtered
}

