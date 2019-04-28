#' Download data
#' @description Download data
#' @param trip_type The trip type 
#' @param from Where from
#' @param to Where to
#' @param date date
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' @return a data frame
#' @export
#'
download_data<- function(trip_type, from, to, date){
      if(trip_type==1) {
        resp.post <- CreateSession(origin = from, 
                                   destination = to, 
                                   startDate = date[1] )
      }else{
        resp.post <- CreateSession(origin = from, 
                                   destination = to, 
                                   startDate = date[1], 
                                   returnDate = date[2] )
      }
      resp.get <- PollSession(respondPOST = resp.post)
      data <- GetData(resp.get)
      Out <- data$legs %>% filter(.data$Directionality == 'Outbound') %>% 
        select("Id","DepartureTime","ArrivalTime","Duration","No.Stops","Stops") %>% 
        magrittr::set_names(c("OutboundLegId","Out_DepartureTime","Out_ArrivalTime","Out_Duration","Out_No.Stops","Out_Stops"))
      In <- data$legs %>% filter(.data$Directionality == 'Inbound') %>% 
        select("Id","DepartureTime","ArrivalTime","Duration","No.Stops","Stops") %>% 
        magrittr::set_names(c("InboundLegId","In_DepartureTime","In_ArrivalTime","In_Duration","In_No.Stops","In_Stops"))
      seg_carrier <- left_join(data$segments %>% 
                                 select(.data$Id, .data$CarrierId),
                               data$carriers %>% 
                                 select(.data$Id, .data$Code, .data$Name) %>% 
                                 magrittr::set_names(c("CarrierId",'Code','Name')),by = "CarrierId")
      
      segment_info <- data$legs %>% select(.data$Id, .data$SegmentIds) %>% 
        unnest(.data$SegmentIds) %>% 
        left_join(seg_carrier,by=c("SegmentIds"= "Id")) %>% 
        select(.data$Id, .data$CarrierId,.data$Code,.data$Name) %>% 
        magrittr::set_names(c("LegId","CarrierId","CarrierCode","CarrierName")) %>% 
        nest(-.data$LegId) %>% magrittr::set_names(c('LegId','CarrierInfo'))
      flight <- data$itineraries %>% 
        left_join(.data$Out,by="OutboundLegId") %>% 
        left_join(.data$In,by="InboundLegId") %>% 
        left_join(data$price %>% select(-.data$SearchTime),by=c("OutboundLegId","InboundLegId")) %>% 
        left_join(segment_info,by=c("OutboundLegId"="LegId", "InboundLegId"="LegId"))
      flight %>%
        unnest(.data$PricingOptions,.drop=FALSE) %>% 
        unnest(.data$CarrierInfo,.drop=FALSE)  -> res
      
      ## Change POSIXct type of time in Out_DepartureTime, Out_ArrivalTime, In_DepartureTime,
      ## In_ArrivalTime to period type.
      # res %>% mutate(Out_DepartureTime_period = format(Out_DepartureTime, format="%H:%M:%S") %>% hms(),
      #                Out_ArrivalTime_period = format(Out_ArrivalTime, format="%H:%M:%S") %>% hms()) -> res
      # if(trip_type != 1){
      #   res %>% mutate(In_DepartureTime_period = format(In_DepartureTime, format="%H:%M:%S") %>% hms(),
      #                  In_ArrivalTime_period = format(In_ArrivalTime, format="%H:%M:%S") %>% hms()) -> res
      #   }
      return(res)
}
