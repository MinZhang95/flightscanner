library(dplyr)
library(DBI)

# initialize SQLite database
con <- dbCreateDB(dbname = "inst/flight.db")

con <- dbConnect(RSQLite::SQLite(),dbname = "inst/flight.db")

dbDisconnect(con)
dbListTables(con)

# remove all tables
sapply(dbListTables(con), dbRemoveTable, conn = con)

# write data into database
SaveData(con, x = resp.get)

# extract data
data <- GetData(resp.get)  # from API response
data <- GetData(con)  # from SQLite database
GetData(con, lazy = T)

src <- src_sqlite("inst/flight.db")
tbl(con, from = "price")





max_price <- 4000
max_stops <- 3
max_duration <- 60*40
layover <- c(60, 60*24)

carrier_include <- unique(data$carriers$Code)  # c("MU", "HU", "DL")
carrier_exclude <- NULL  # c("AS", "KE", "EK")

out_departure <- c("0:00", "24:00")
out_arrival <- c("0:00", "24:00")
in_departure <- c("0:00", "24:00")
in_arrival <- c("0:00", "24:00")

tally()
# add carrier information, 没问题
carriers <- data$carriers %>% group_by(Id) %>% filter(n() == 1) %>% ungroup()
segments <- data$segments %>% group_by(Id) %>% filter(n() == 1) %>% ungroup()
segments_info <- segments %>%
  inner_join(rename_all(carriers, ~ paste0("Carrier", .)), by = "CarrierId") %>%
  inner_join(rename_all(carriers, ~ paste0("OperatingCarrier", .)), by = "OperatingCarrierId")

# filter legs by duration, no.stops, layover
legs_info <- data$legs %>%
  select(-"OriginId", -"DestinationId", -"Directionality") %>%
  filter(!!sym("Duration") <= max_duration
         & !!sym("No.Stops") <= max_stops
         & purrr::map_lgl(!!sym("Stops"), ~ all(between(as.numeric(.$Layover), layover[1], layover[2]))))
# filter_at(vars("OutboundStops", "InboundStops"),
# ~ purrr::map_lgl(., ~ all(between(as.numeric(.$Layover), layover[1], layover[2]))))


# filter legs by carrier
legs_info <- legs_info %>%
  mutate_at("SegmentIds", ~ purrr::map(., ~ data.frame(Id = ., stringsAsFactors = F) %>%
                                         left_join(segments_info, by = "Id"))) %>%
  rename(Segments = "SegmentIds") %>%
  filter(purrr::map_lgl(!!sym("Segments"),
                        ~ any(.$CarrierCode %in% carrier_include) & all(!.$CarrierCode %in% carrier_exclude)))

# filter itineraries by time
itineraries_info <- data$itineraries %>%
  inner_join(rename_all(legs_info, ~ paste0("OutboundLeg", .)), by = "OutboundLegId") %>%
  inner_join(rename_all(legs_info, ~ paste0("InboundLeg", .)), by = "InboundLegId") %>%
  filter(BetweenTime(!!sym("OutboundLegDepartureTime"), out_departure)
         & BetweenTime(!!sym("OutboundLegArrivalTime"), out_arrival)
         & (BetweenTime(!!sym("InboundLegDepartureTime"), in_departure)
            & BetweenTime(!!sym("InboundLegArrivalTime"), in_arrival)
            | !!sym("InboundLegId") == ""))

# filter price
price_info <- data$price %>%
  select(-"SearchTime") %>%
  filter(purrr::map_lgl(!!sym("PricingOptions"), ~ min(.$Price) <= max_price))


price_info %>% dim
price_info %>% distinct(OutboundLegId, InboundLegId) %>% dim

itineraries_info %>% dim
itineraries_info %>% distinct(OutboundLegId, InboundLegId) %>% dim

data$itineraries %>% dim
data$itineraries %>% distinct(OutboundLegId, InboundLegId) %>% dim


legs_info %>% dim
legs_info %>% distinct(Id) %>% dim

data$legs %>% dim
data$legs %>% distinct(Id) %>% dim
data$legs %>% group_by(Id) %>% count() %>% filter(n > 1)
data$legs %>% filter(Id == "15641-1907010040--32132-2-16177-1907011310") %>% .$SegmentIds

segments_info %>% dim
segments_info %>% distinct(Id) %>% dim

data$segments %>% dim
data$segments %>% distinct(Id) %>% dim



price_info %>% inner_join(itineraries_info, by = c("OutboundLegId", "InboundLegId"))

price_info %>% head
itineraries_info %>% head %>% select(1:2)
price_info %>% head %>% inner_join(itineraries_info %>% head, by = c("OutboundLegId", "InboundLegId")) %>% select(1:2)











carrier_include; carrier_exclude

data$carriers %>% filter(!(!!sym("Code") %in% carrier_exclude))



!c("MU", "HU", "AA") %in% carrier_include


60
180
m = data$legs %>% filter(No.Stops == 0)
m = tibble(id = 1:4,
           OutboundStops = list(data.frame(Layover = c(80, 90), 1),
                                data.frame(Layover = c(80, 90), 1),
                                data.frame(Layover = c(20, 90), 1),
                                data.frame(Layover = c(20, 90), 1)),
           InboundStops = list(data.frame(Layover = c(80, 90), 1),
                               data.frame(Layover = c(80, 190), 1),
                               data.frame(Layover = c(80, 90), 1),
                               data.frame(Layover = c(80, 190), 1)))

m %>% 
  filter_at(vars("OutboundStops", "InboundStops"), ~ map_lgl(., ~ print(.$Layover)))
filter_at(vars("OutboundStops", "InboundStops"), ~ map_lgl(., ~ all(between(as.numeric(.$Layover), layover[1], layover[2]))))


