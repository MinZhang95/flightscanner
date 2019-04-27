library(leaflet)
library(shiny)

data("airports")

## all input variables
# trip_type
# from/to
# date
# price
# Incl. Airline/Excl. Airline
# Leave/Back + _ + Duration/Stops/Dep_Time/Arr_Time

shinyServer(function(input, output) {
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(data = airports ) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(~ Longitude,
                                ~ Latitude,
                                popup = ~ Name,
                                clusterOptions = leaflet::markerClusterOptions())%>% setView(lng=-98,lat=38,zoom=4)
  })
  
})
