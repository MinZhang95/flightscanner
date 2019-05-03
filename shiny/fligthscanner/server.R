library(flightscanner)
library(dplyr)
library(leaflet)
library(tidyr)
library(lubridate)
library(shiny)

data("airports", package = "flightscanner")

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
        
        Date <- switch(input$trip_type, "1" = list(input$date1, NULL),
                       "2" = list(input$date2, input$date3))
        resp.post <- CreateSession(orig = input$from, dest = input$to,
                                   startDate = Date[[1]], returnDate = Date[[2]])
        resp.get <- PollSession(respondPOST = resp.post)
        GetData(resp.get)
      })
    })
  
  
  output$ui <- renderUI({
    price <- bind_rows(dataset()$price$PricingOptions)$Price
    airline <- unique(dataset()$carriers$Code)
    duration <- dataset()$legs$Duration / 60  # minutes to hours
    layover <- bind_rows(dataset()$legs$Stops)$Layover / 60  # minutes to hours
    if (length(layover) == 0) layover <- c(0, Inf)
    
    wellPanel(
      sliderInput("Price", label = "Price", 
                  min = min(price), max = max(price), value = max(price),
                  round = 2L, ticks = FALSE, pre = "$")
      ,
      tabsetPanel(
        tabPanel("Airlines Includes",
                 selectInput("Airline_In", label = "",
                             choices = airline, selected = airline, multiple = TRUE)),
        tabPanel("Airlines Excludes",
                 selectInput("Airline_Ex", label = "",
                             choices = airline, multiple = TRUE))
      )
      ,
      sliderInput("Duration", label = "Duration",
                  min = floor(min(duration)), max = ceiling(max(duration)),
                  value = ceiling(max(duration)),
                  step = 0.5, ticks = FALSE, post = "h")
      ,
      radioButtons("Stops", label = "Stops",
                   choices = list("Any number of stops" = Inf, "Nonstop only" = 0,
                                  "1 stop or fewer" = 1, "2 stops or fewer" = 2))
      ,
      sliderInput("Layover", label = "Layover",
                  min = floor(min(layover)), max = ceiling(max(layover)),
                  value = c(floor(min(layover)), ceiling(max(layover))),
                  step = 0.5, ticks = FALSE, post = "h")
      ,
      tabsetPanel(
        tabPanel("Outbound",
                 br(),
                 sliderInput("Leave_Dep_Time", label = "Departure", step = 0.5, ticks = FALSE,
                             min = 0, max = 24, value = c(0, 24)),
                 sliderInput("Leave_Arr_Time", label = "Arrival", step = 0.5, ticks = FALSE,
                             min = 0, max = 24, value = c(0, 24))
        ),
        tabPanel("Inbound",
                 br(),
                 sliderInput("Back_Dep_Time", label = "Departure", step = 0.5, ticks = FALSE,
                             min = 0, max = 24, value = c(0, 24)),
                 sliderInput("Back_Arr_Time", label = "Arrival", step = 0.5, ticks = FALSE,
                             min = 0, max = 24, value = c(0, 24))
        )
      ))
  })
  
  output$map <- leaflet::renderLeaflet({
    from_data <- airports %>% filter(IATA != "")%>% 
      filter(IATA == toupper(input$from)) 
    to_data <- airports %>% filter(IATA != "")%>% 
      filter(IATA == toupper(input$to)) 
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addMarkers(data = from_data, ~ Longitude, ~ Latitude, popup = ~ Name) %>%
      leaflet::addMarkers(data = to_data, ~ Longitude, ~ Latitude, popup = ~ Name)
  })
  
  output$Search_res <- renderText({
    t <- dataset()
    if (!is.list(t)) {
      "Error happened: Check the input information and try again!"
    } else {
      "Search succeeded! Click Flight Tab For More Details:)"
    }
  })
  
  #output$value <- renderPrint({c(input$date1, input$date2, input$date3)})
  
  output$table <- renderDataTable({
    data <- dataset()
    
    # 用来debug
    cat("DURATION:", input$Duration, "\tPRICE: ", input$Price, "\n")
    cat("STOPS: ", input$Stops, "\tLAYOVER: ", input$Layover, "\n")
    cat("AIRLINE: In: ", input$`Airline_In`, "\tEx: ", input$`Airline_Ex`, "\n")
    cat("TIME: ", input$Leave_Dep_Time, "\t", input$Leave_Arr_Time, "\t",
        input$Back_Dep_Time, "\t", input$Back_Arr_Time, "\n")
    
    temp <- FilterFlight(data,
                         max_price = input$Price,
                         max_duration = input$Duration * 60,
                         max_stops = as.numeric(input$Stops),
                         layover = input$Layover * 60,
                         # carrier_include = input$`Airline_In`,  # 有问题
                         # carrier_exclude = input$`Airline_Ex`,
                         out_departure = input$Leave_Dep_Time * 60,
                         out_arrival = input$Leave_Arr_Time * 60,
                         in_departure = input$Back_Dep_Time * 60,
                         in_arrival = input$Back_Arr_Time * 60
    ) %>%
      select(-ends_with("LegId"), -ends_with("LegSegments"), -ends_with("LegStops"))
    
    # 用来debug
    print(FilterFlight(data) %>% nrow)
    print(temp %>% nrow)
    
    temp %>% tidyr::unnest(PricingOptions) %>% {
      if (nrow(.) == 0) . else select(., Price, everything(), -AgentId, -LinkURL)
    }
  }, escape = FALSE, options = list(pageLength = 10))
  
  output$IATAtable <- renderDataTable({
    airports %>% filter(IATA != "") %>% select(Name, City, Country, IATA, Latitude, Longitude)
  }, options = list(pageLength = 10))
})