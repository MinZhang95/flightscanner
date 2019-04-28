#' Download data
#' @description Download data
#' @param trip_type The trip type 
#' @param from Where from
#' @param to Where to
#' @param date date
#' @import dplyr
#' @import tidyr
#' @export res a data frame about search result
#'
download_data<- function(trip_type, from, to, date){
      if(trip_type==1) {
        resp.post <- CreateSession(orig = from, 
                                   dest = to, 
                                   startDate = date[1] )
      }else{
        resp.post <- CreateSession(orig = from, 
                                   dest = to, 
                                   startDate = date[1], 
                                   returnDate = date[2] )
      }
      resp.get <- PollSession(respondPOST = resp.post)
      data <- GetData(resp.get)
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
      flight %>%
        unnest(PricingOptions,.drop=FALSE) %>% 
        unnest(CarrierInfo,.drop=FALSE)  -> res
      return(res)
}
