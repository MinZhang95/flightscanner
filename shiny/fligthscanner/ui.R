library(leaflet)
library(MUCflights)
library(shiny)

shinyUI <- fluidPage(
  titlePanel("Hello FlightScanner!"),
  ## the first filter for trip type, places and dates
  fluidRow(
    # filter for trip type
    column(2,
           selectInput("trip_type",label='',
                          choices = list("One Way" = 1, "Round Trip" = 2), 
                          selected = 1)
            )    ,
    # filter for places
    column(2,
           textInput("from", label = '', value = "Where from")
    ),
    column(2,
           textInput("to", label = '', value = "Where to")
    ),
    #condtional panel for dates
    column(4,
           conditionalPanel(
             condition="input.trip_type=='1' ",
             column(10,
                            dateInput("date1", "", 
                                      min = Sys.Date(),
                                      value = Sys.Date(), 
                                      format = "mm/dd/yy"))
                            
           
           ),
           conditionalPanel(
             condition="input.trip_type== '2' ",
             column(5,
                    dateInput("date2", "", 
                              min = Sys.Date(),
                              value = Sys.Date(), 
                              format = "mm/dd/yy")),
             
             column(5,
                    dateInput("date3", "", 
                              min = Sys.Date(),
                              value = Sys.Date()+1, 
                              format = "mm/dd/yy")) 
             ),
           uiOutput('varselect')       
    )
  ),
  ## filter for price, airline, stops, time ... 
  fluidRow(
    column(3,
           wellPanel(
             sliderInput("price", label = "Price", min = 0, 
                         max = 100, value = 50)
              ,
           tabsetPanel(
             tabPanel("Incl. Airline", 
             selectInput("Airline_In", label = '', 
                         choices = list("AA" , "BB","CC",'DD'), 
                         multiple = TRUE)),
             tabPanel("Excl. Airline",
             selectInput("Airline_Ex", label = "", 
                         choices = list("AA" , "BB","CC",'DD'), 
                         multiple = TRUE)))
             ,
           tabsetPanel(
             tabPanel("Leave", 
                      br(),
                      sliderInput("Leave_Duration", 
                                  label = "Duration/min",ticks = FALSE,
                                  min = 0, max = 100, value = 50),
                      checkboxGroupInput("Leave_Stops", label = 'Stops', 
                                         choices = list("Nonstops" = 0, "1 Stop" = 1, "2 Stops" = 2, 'More than 2 Stops'=3),
                                         selected = 0),
                      sliderInput("Leave_Dep_Time", label = "Departure Time",step = 0.5,ticks = FALSE,
                                  min = 0, max = 24, value = c(0, 24)),
                      sliderInput("Leave_Arr_Time", label = "Arrival Time",step = 0.5,ticks = FALSE, 
                                  min = 0, max = 24, value = c(0, 24))),
             tabPanel("Back", 
                      br(),
                      sliderInput("Back_Duration", 
                                  label = "Duration/min",ticks = FALSE,
                                  min = 0, max = 100, value = 50),
                      checkboxGroupInput("Back_Stops", label = 'Stops', 
                                         choices = list("Nonstops" = 0, "1 Stop" = 1, "2 Stops" = 2, 'More than 2 Stops'=3),
                                         selected = 0),
                      sliderInput("Back_Dep_Time", label = "Departure Time",step = 0.5,ticks = FALSE,
                                  min = 0, max = 24, value = c(0, 24)),
                      sliderInput("Back_Arr_Time", label = "Arrival Time",step = 0.5,ticks = FALSE, 
                                  min = 0, max = 24, value = c(0, 24)))
           )  
        )
    ),
    
    column(9,
           tabsetPanel(
             tabPanel("Airport Map", leafletOutput("map")),
             tabPanel("Flights", verbatimTextOutput("flights"))
           )
    )
  )
)


