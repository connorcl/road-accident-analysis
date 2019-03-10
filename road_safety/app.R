library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)

filename_05_14 <- "road_accidents_2016.csv"
filename_16 <- "road_accidents_2005-14.csv"
url_05_14 <- "https://data.dundeecity.gov.uk/dataset/592cec6c-3628-43f8-a804-57144668e12f/resource/c3e7a3fc-5e81-4010-994d-93077c543721/download/road-safety-data-accidents-2016.csv"
url_16 <- "https://data.dundeecity.gov.uk/dataset/592cec6c-3628-43f8-a804-57144668e12f/resource/6eeb1c88-178b-489e-a017-60d0431d6355/download/road-safety-data-accidents-2005-2014-.csv"
if(!file.exists(filename_05_14)) {
  download.file(url_05_14, filename_05_14)
}
if(!file.exists(filename_16)) {
  download.file(url_16, filename_16)
}

road_accidents_2005_14 <- read.csv(filename_05_14, stringsAsFactors = FALSE)
road_accidents_2016 <- read.csv(filename_16, stringsAsFactors = FALSE)
road_accidents = rbind(road_accidents_2005_14, road_accidents_2016)
road_accidents$datetime = as.POSIXct(strptime(paste(road_accidents$Date, road_accidents$Time), "%d/%m/%Y %H:%M"))
road_accidents <- road_accidents %>% mutate(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude))

days <- c("1 Sunday", "2 Monday", "3 Tuesday", "4 Wednesday", "5 Thursday", "6 Friday", "7 Saturday")
months <- c("01 Jan", "02 Feb", "03 Mar", "04 Apr", "05 May", "06 Jun", "07 Jul", "08 Aug", "09 Sep", "10 Oct", "11 Nov", "12 Dec")

ui <- navbarPage("Dundee Road Accidents",
  
  tabPanel("Map",
      radioButtons("radio_sev", label = "Accident Severity", choices = c("All", "Slight", "Serious", "Fatal"), inline = TRUE),
      leafletOutput("accident_locations", height = 512)
  ),
  tabPanel("Accidents by Weekday",
      selectInput("day_split_by", "Facet Variable", choices = c("None", "Year", "Month")),
      plotOutput("dow"), height = 512),
  tabPanel("Accidents by Month",
           plotOutput("mon"), height = 512),
  tabPanel("Accidents by Hour",
           selectInput("hour_split_by", "Facet Variable", choices = c("None", "Weekday")),
           plotOutput("hr"), height = 512)
)

server <- function(input, output) {
  
  get_loc_data <- reactive({
    if (input$radio_sev == "All") {
      road_accidents
    } else {
      road_accidents %>% filter(Accident_Severity == input$radio_sev)
    }
  })
  
  output$accident_locations <- renderLeaflet({
    leaflet(get_loc_data()) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) #addCircleMarkers(color = ~get_pal()(severity), fillOpacity = 0.1)#addMarkers(data = get_loc_data(), label = get_lab(), clusterOptions = markerClusterOptions())
  })
  
  output$dow <- renderPlot({
    plot <- ggplot(road_accidents) + aes(x = days[wday(datetime)], fill = Accident_Severity) + geom_bar() + xlab("Weekday")
    if(input$day_split_by == "Year") {
      plot + facet_wrap(. ~ year(datetime))
    } else if (input$day_split_by == "Month") {
      plot + facet_wrap(. ~ month(datetime), scales = "free") + theme(legend.position = "bottom")
    } else {
      plot
    }
  })
  
  output$mon <- renderPlot({
    ggplot(road_accidents) + aes(x = months[factor(month(datetime))], fill = Accident_Severity) + geom_bar() + xlab("Month")
  })
  
  output$hr <- renderPlot({
    plot <- ggplot(road_accidents) + aes(x = hour(datetime), fill = Accident_Severity) + geom_bar() + xlab("Hour")
    if (input$hour_split_by == "Weekday") {
      plot + facet_wrap(. ~ days[wday(datetime)], scales = "free") + theme(legend.position = "bottom")
    } else {
      plot
    }
  })
}

shinyApp(ui, server)

