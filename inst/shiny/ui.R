library(flightscanner)
library(dplyr)
library(leaflet)
library(tidyr)
library(lubridate)
library(shiny)

data("airports")

shinyUI <- fluidPage(
  titlePanel("Hello FlightScanner!"),
  ## the first filter for trip type, places and dates
  fluidRow(
    # filter for trip type
    column(2,
           selectInput("trip_type", label = "Trip Type",
                       choices = list("One Way" = 1, "Round Trip" = 2))
    )    ,
    # filter for places
    column(2,
           textInput("from", label = "From", value = "DSM")
    ),
    column(2,
           textInput("to", label = "To", value = "DTW")
    ),
    #condtional panel for dates
    column(4,
           conditionalPanel(
             condition = "input.trip_type=='1'",
             column(10,
                    dateInput("date1", "Dept. Date", 
                              min = Sys.Date() + 1,
                              value = Sys.Date() + 1, 
                              format = "mm/dd/yy"))
           ),
           conditionalPanel(
             condition = "input.trip_type== '2'",
             column(5,
                    dateInput("date2", "Dept. Date", 
                              min = Sys.Date() + 1,
                              value = Sys.Date() + 1, 
                              format = "mm/dd/yy")),
             column(5,
                    uiOutput("ui_date")
             ) 
           )      
    ),
    column(2, align = "left",
           actionButton("goButton", "Go!", icon("telegram-plane",
                                                #"accessible-icon",
                                                "fa-3x","font-awesome")))
  ),
  ## filter for price, airline, stops, time ... 
  fluidRow(
    tabsetPanel(
      tabPanel("Airport Map", 
               leafletOutput("map"),
               h3(textOutput("Search_res"), align = "center"),
               h3(textOutput("value"), align = "center")
      ),
      tabPanel("Flights", 
               column(3, uiOutput("ui")),
               column(9, dataTableOutput("table"))
      ),
      tabPanel("IATA Code", 
               column(12, dataTableOutput("IATAtable"))
      )
    )
  )
)