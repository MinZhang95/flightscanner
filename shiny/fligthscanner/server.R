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

shinyServer(function(input, output,session) {
  output$ui_date <- renderUI({
    dateInput("date3", "Arr. Date", 
              min = input$date2,
              value = input$date2+7, 
              format = "mm/dd/yy") 
  })
  
  
  dataset <- eventReactive(
    input$goButton, 
    {withProgress({
      setProgress(message = "Working Really Hard...")
      resp.get <- CreateSession(orig = input$from, dest = input$to,
                                startDate = input$date1, 
                                returnDate = input$data2) %>%
        PollSession(respondPOST = .)
      GetData(resp.get)
    })
  })

  
  output$ui <- renderUI({ 
      wellPanel(
        sliderInput("price", label = "Price (USD)", 
                    min = data$price$PricingOptions %>%
                      sapply(function(x) min(x$Price)) %>% min,
                    max = data$price$PricingOptions %>%
                      sapply(function(x) max(x$Price)) %>% max, 
                    value = data$price$PricingOptions %>%
                      sapply(function(x) max(x$Price)) %>% max,
                    ticks = FALSE)
        ,
        tabsetPanel(
          tabPanel("Incl. Airline",
                   selectInput("Airline_In", label = '',
                               choices = unique(dataset()$carriers$Code),
                               selected = unique(dataset()$carriers$Code),
                               multiple = TRUE)),
          tabPanel("Excl. Airline",
                   selectInput("Airline_Ex", label = "",
                               choices = unique(dataset()$carriers$Code),
                               multiple = TRUE)))
        ,
        sliderInput("Duration",
                    label = "Duration (hours)",ticks = FALSE,step = 0.1,
                    min = round((dataset()$legs$Duration %>% min)/60,1),
                    max = round((dataset()$legs$Duration %>% max)/60,1),
                    value = round((dataset()$legs$Duration %>% max)/60),1),
        radioButtons("Back_Stops", label = 'Stops',
                     choices =list("Nonstops only" = 0, "1 stop or fewer" = 1, "2 stops or fewer" = 2, "I don't care"=Inf),
                     selected = Inf
        ),
        tabsetPanel(
          tabPanel("Leave",
                   br(),
                     sliderInput("Leave_Dep_Time", label = "Departure Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24)),
                   sliderInput("Leave_Arr_Time", label = "Arrival Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24))),
          tabPanel("Back",
                   br(),
                   sliderInput("Back_Dep_Time", label = "Departure Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24)),
                   sliderInput("Back_Arr_Time", label = "Arrival Time",step = 0.5,ticks = FALSE,
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
    t<- dataset()
    if(!is.list(t)){
      "Error happened: Check the input information and try again!"
    }else{
      "Search Successed! Click Flight Tab For More Details:)"
    }
  })
  
   output$table <- renderDataTable({
     dataset()$segment
   }, escape = FALSE,options = list(pageLength =10))
   
  
  output$IATAtable <- renderDataTable({
    airports %>% filter(IATA!="") %>% 
      select(Name, City, Country, IATA, Latitude, Longitude)
   
  },options = list(pageLength =10))
})