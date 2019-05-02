library(flightscanner)
library(dplyr)
library(leaflet)
library(tidyr)
library(lubridate)
library(shiny)

data("airports")

## all input variables
# trip_type
# from/to
# date
# price
# Incl. Airline/Excl. Airline
# Leave/Back + _ + Duration/Stops/Dep_Time/Arr_Time

shinyServer(function(input, output, session) {
  output$ui_date <- renderUI({
    dateInput("date3", "Arr. Date", 
              min = input$date2,
              value = input$date2 + 7, 
              format = "mm/dd/yy") 
  })
  
  
  dataset <- eventReactive(
    input$goButton, {
      withProgress({
        setProgress(message = "Working Really Hard...")
        resp.get <- CreateSession(orig = input$from, dest = input$to,
                                  startDate = input$date1, 
                                  returnDate = input$date2) %>%
          PollSession(respondPOST = .)
        GetData(resp.get)
      })
    })
  
  
  output$ui <- renderUI({ 
    wellPanel(
      sliderInput("price", label = "Price (USD)", 
                  min = min(bind_rows(data$price$PricingOptions)$Price),
                  max = max(bind_rows(data$price$PricingOptions)$Price), 
                  value = max(bind_rows(data$price$PricingOptions)$Price),
                  ticks = FALSE)
      ,
      tabsetPanel(
        tabPanel("Incl. Airline",
                 selectInput("Airline_In", label = "",
                             choices = unique(dataset()$carriers$Code),
                             selected = unique(dataset()$carriers$Code),
                             multiple = TRUE)),
        tabPanel("Excl. Airline",
                 selectInput("Airline_Ex", label = "",
                             choices = unique(dataset()$carriers$Code),
                             multiple = TRUE)))
      ,
      sliderInput("Duration",
                  label = "Duration (hours)", ticks = FALSE, step = 0.5,
                  min = floor(min(dataset()$legs$Duration) / 60),
                  max = ceiling(max(dataset()$legs$Duration) / 60),
                  value = ceiling(max(dataset()$legs$Duration) / 60)),
      radioButtons("Stops", label = "Stops",
                   choices = list("Any number of stops" = Inf, "Nonstop only" = 0,
                                  "1 stop or fewer" = 1, "2 stops or fewer" = 2),
                   selected = Inf
      ),
      tabsetPanel(
        tabPanel("Leave",
                 br(),
                 sliderInput("Leave_Dep_Time", label = "Departure Time", step = 0.5, ticks = FALSE,
                             min = 0, max = 24, value = c(0, 24)),
                 sliderInput("Leave_Arr_Time", label = "Arrival Time", step = 0.5, ticks = FALSE,
                             min = 0, max = 24, value = c(0, 24))),
        tabPanel("Back",
                 br(),
                 sliderInput("Back_Dep_Time", label = "Departure Time", step = 0.5, ticks = FALSE,
                             min = 0, max = 24, value = c(0, 24)),
                 sliderInput("Back_Arr_Time", label = "Arrival Time", step = 0.5, ticks = FALSE,
                             min = 0, max = 24, value = c(0, 24)))
      ))
  })
  
  output$map <- leaflet::renderLeaflet({
    from_data = airports %>% filter(IATA != "")%>% 
      filter(IATA == toupper(input$from)) 
    to_data = airports %>% filter(IATA != "")%>% 
      filter(IATA == toupper(input$to)) 
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addMarkers(data = from_data,
                          ~ Longitude,
                          ~ Latitude,
                          popup = ~ Name)%>%
      leaflet::addMarkers(data = to_data,
                          ~ Longitude,
                          ~ Latitude,
                          popup = ~ Name)
  })
  
  output$Search_res <- renderText({
    t <- dataset()
    if (!is.list(t)) {
      "Error happened: Check the input information and try again!"
    } else {
      "Search succeeded! Click Flight Tab For More Details:)"
    }
  })
  
  output$table <- renderDataTable({
    data <- dataset()
    FilterFlight(data, max_price = input$price, max_duration = input$Duration,
                 max_stops = input$Stops, layover = c(60, 240),
                 carrier_include = input$`Incl. Airline`,
                 carrier_exclude = input$`Excl. Airline`,
                 out_departure = input$Leave_Dep_Time, out_arrival = input$Leave_Arr_Time,
                 in_departure = input$Back_Dep_Time, in_arrival = input$Back_Arr_Time) %>%
      select(-ends_with("LegSegments"), -ends_with("LegStops")) %>%
      tidyr::unnest(!!sym("PricingOptions"))
  }, escape = FALSE, options = list(pageLength = 10))
  
  output$IATAtable <- renderDataTable({
    airports %>% filter(IATA != "") %>% select(Name, City, Country, IATA, Latitude, Longitude)
  },options = list(pageLength = 10))
})