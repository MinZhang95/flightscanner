library(leaflet)
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)

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
      if(input$trip_type==1){
        try(download_data(input$trip_type,
                          toupper(input$from),
                          toupper(input$to),
                          input$date1))
      }else{
        try(download_data(input$trip_type,
                          toupper(input$from),
                          toupper(input$to),
                          c(input$date2,input$date3)))
      }
    })
  })

  
  output$ui <- renderUI({ 
    if(class(dataset())[3]=='data.frame'){
      wellPanel(
        sliderInput("price", label = "Price ($)", min = min(dataset()$Price),
                    max = max(dataset()$Price), 
                    value = max(dataset()$Price),ticks = FALSE)
        ,
        tabsetPanel(
          tabPanel("Incl. Airline",
                   selectInput("Airline_In", label = '',
                               choices = unique(dataset()$CarrierName),
                               selected = unique(dataset()$CarrierName),
                               multiple = TRUE)),
          tabPanel("Excl. Airline",
                   selectInput("Airline_Ex", label = "",
                               choices = unique(dataset()$CarrierName),
                               multiple = TRUE)))
        ,
        tabsetPanel(
          tabPanel("Leave",
                   br(),
                   sliderInput("Leave_Duration",
                               label = "Duration (hours)",ticks = FALSE,step = 0.1,
                               min = round(min(dataset()$Out_Duration)/60,1), 
                               max = round(max(dataset()$Out_Duration)/60,1), 
                               value = round(max(dataset()$Out_Duration)/60),1),
                   radioButtons("Leave_Stops", label = 'Stops',
                                choices = list("Nonstops only" = 0, "1 stop or fewer" = 1, "2 stops or fewer" = 2, "I don't care"=Inf),
                                selected = Inf),
                   sliderInput("Leave_Dep_Time", label = "Departure Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24)),
                   sliderInput("Leave_Arr_Time", label = "Arrival Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24))),
          tabPanel("Back",
                   br(),
                   sliderInput("Back_Duration",
                               label = "Duration (hours)",ticks=F,step = 0.1,
                               min = round(min(dataset()$In_Duration)/60,1), 
                               max = round(max(dataset()$In_Duration)/60,1),
                               value = round(max(dataset()$In_Duration)/60,1)),
                   radioButtons("Back_Stops", label = 'Stops', 
                                choices =list("Nonstops only" = 0, "1 stop or fewer" = 1, "2 stops or fewer" = 2, "I don't care"=Inf),
                                selected = Inf
                   ),
                   sliderInput("Back_Dep_Time", label = "Departure Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24)),
                   sliderInput("Back_Arr_Time", label = "Arrival Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24)))
        )
      )
    }else{
      wellPanel(
        sliderInput("price", label = "Price ($)", min = 0,
                    max = Inf, value = 0,ticks = FALSE)
        ,
        tabsetPanel(
          tabPanel("Incl. Airline",
                   selectInput("Airline_In", label = '',
                               choices = list(),
                               multiple = TRUE)),
          tabPanel("Excl. Airline",
                   selectInput("Airline_Ex", label = "",
                               choices = list(),
                               multiple = TRUE)))
        ,
        tabsetPanel(
          tabPanel("Leave",
                   br(),
                   sliderInput("Leave_Duration",
                               label = "Durationn (hours)",ticks = FALSE,
                               min = 0, max = Inf, value = 0),
                   radioButtons("Leave_Stops", label = 'Stops',
                                choices = list("Enter Something" = 0),
                                selected = 0),
                   sliderInput("Leave_Dep_Time", label = "Departure Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24)),
                   sliderInput("Leave_Arr_Time", label = "Arrival Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24))),
          tabPanel("Back",
                   br(),
                   sliderInput("Back_Duration",
                               label = "Duration (hours)",ticks = FALSE,
                               min = 0, max = Inf, value = 0),
                   radioButtons("Back_Stops", label = 'Stops',
                                choices = list("没有我，哈哈，傻逼了吧；）" = 0),
                                selected = 0),
                   sliderInput("Back_Dep_Time", label = "Departure Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24)),
                   sliderInput("Back_Arr_Time", label = "Arrival Time",step = 0.5,ticks = FALSE,
                               min = 0, max = 24, value = c(0, 24)))
        )
      )
    }
    
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
    
    if(!grepl("tbl",as.character(class(t)[2]))){
      "Error happened: Check the input information and try again!"
    }else{
      "Search Successed! Click Flight Tab For More Details:)"
    }
  })
  
   output$table <- renderDataTable({
     if(input$trip_type==1){
       # filter for single trip
       dataset() %>% 
         select(-c(Out_Stops, In_Stops)) %>% 
         filter(Out_No.Stops <= input$Leave_Stops,
                Out_Duration <= input$Leave_Duration * 60, 
                hour(Out_DepartureTime)+minute(Out_DepartureTime)/60 >= input$Leave_Dep_Time[1], 
                hour(Out_DepartureTime)+minute(Out_DepartureTime)/60 <= input$Leave_Dep_Time[2],
                hour(Out_ArrivalTime)+minute(Out_ArrivalTime)/60 >= input$Leave_Arr_Time[1], 
                hour(Out_ArrivalTime)+minute(Out_ArrivalTime)/60 <= input$Leave_Arr_Time[2],
                Price <= input$price, 
                CarrierName %in% input$Airline_In, 
                !CarrierName %in% input$Airline_Ex)  %>% 
         mutate(Out_DepartTime = Out_DepartureTime,
                Out_Duration_Hr = round(Out_Duration/60, 1),
                Price_USD = Price,
                Link = sprintf('<a href="%s" target="_blank" class="btn btn-primary">Book</a>',LinkURL)) %>%
         select(Price_USD, Out_DepartTime, Out_ArrivalTime, Out_Duration_Hr, Out_No.Stops,
                CarrierName, Link)
     }else{
       # filter for round trip
       dataset() %>% 
         select(-c(Out_Stops, In_Stops)) %>% 
         filter(Out_No.Stops <= input$Leave_Stops,
                Out_Duration <= input$Leave_Duration * 60, 
                hour(Out_DepartureTime)+minute(Out_DepartureTime)/60 >= input$Leave_Dep_Time[1], 
                hour(Out_DepartureTime)+minute(Out_DepartureTime)/60 <= input$Leave_Dep_Time[2], 
                hour(Out_ArrivalTime)+minute(Out_ArrivalTime)/60 >= input$Leave_Arr_Time[1], 
                hour(Out_ArrivalTime)+minute(Out_ArrivalTime)/60 <= input$Leave_Arr_Time[2],
                Price <= input$price, 
                CarrierName %in% input$Airline_In,
                !CarrierName %in% input$Airline_Ex) %>% 
         filter(In_No.Stops <= input$Back_Stops,
                In_Duration <= input$Back_Duration * 60,
                hour(In_DepartureTime)+minute(In_DepartureTime)/60 >= input$Back_Dep_Time[1], 
                hour(In_DepartureTime)+minute(In_DepartureTime)/60 <= input$Back_Dep_Time[2], 
                hour(In_ArrivalTime)+minute(In_ArrivalTime)/60 >= input$Back_Arr_Time[1], 
                hour(In_ArrivalTime)+minute(In_ArrivalTime)/60 <= input$Back_Arr_Time[2] ) %>%
         mutate(Out_DepartTime = Out_DepartureTime,
                Out_Duration_Hr = round(Out_Duration/60, 1),
                In_DepartTime = In_DepartureTime,
                In_Duration_Hr = round(In_Duration/60, 1),
                Price_USD = Price,
                Link = sprintf('<a href="%s" target="_blank" class="btn btn-primary">Book</a>',LinkURL)) %>% 
         select(Price_USD, Out_DepartTime, Out_ArrivalTime, Out_Duration_Hr, Out_No.Stops,
                In_DepartTime, In_ArrivalTime, In_Duration_Hr, In_No.Stops,
                CarrierName, Link)
     }
   }, escape = FALSE)
   
  
  output$IATAtable <- renderDataTable({
    airports %>% filter(IATA!="") %>% 
      select(Name, City, Country, IATA, Latitude, Longitude)
   
  },options = list(pageLength =10))
})