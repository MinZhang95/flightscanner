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

shinyServer(function(input, output,session) {
  output$ui_date <- renderUI({
    dateInput("date3", "Arr. Date", 
              min = input$date2,
              value = input$date2+7, 
              format = "mm/dd/yy") 
  })
  
  dataset <- reactive({
    if(!(input$from==""&input$to==""))
    {# Change when the button is pressed...
    input$goButton
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "哗啦啦啦哗啦啦...")
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
    })}
  })

  output$ui <- renderUI({ 
    if(class(dataset())!='try-error'){
      wellPanel(
        sliderInput("price", label = "Price ($)", min = min(dataset()$Price),
                    max = max(dataset()$Price), 
                    value = max(dataset()$Price),ticks = FALSE)
        ,
        tabsetPanel(
          tabPanel("Incl. Airline",
                   selectInput("Airline_In", label = '',
                               choices = unique(dataset()$CarrierName),
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
                                selected = 0),
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
                                selected = 0
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
                                choices = list("没有我，哈哈，傻逼了吧；）" = 0),
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
    leaflet::leaflet(data = airports %>% filter(IATA != "")%>% 
                       filter(IATA %in% toupper(c(input$from,input$to))) ) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(~ Longitude,
                                ~ Latitude,
                                popup = ~ Name)
  })
  
  
  output$table <- renderTable({
    dataset() %>% 
      select(Price ,CarrierName) %>% arrange(Price)
  })
  
})
