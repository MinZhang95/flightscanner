

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

flight %>% filter(Out_Stops <= filter_Options$Out_Stops,
                  In_Stops <= filter_Options$In_Stops,
                  Out_Duration <= filter_Options$Out_Duration,
                  In_Duration <= filter_Options$In_Duration) -> 
                  
