#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(lubridate)

load("acc.RData")

districts <- readxl::read_xlsx("districts.xlsx")

acc <- inner_join(acc, districts, by = c("local_authority_district" = "code"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Map of accidents"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "districtSelect",
                        label = "District:",
                        choices = sort(unique(acc$label))),
            sliderInput(inputId = "casualtiesSlider", 
                        label = "Number of casualties:",
                        min = min(acc$number_of_casualties),
                        max = max(acc$number_of_casualties),
                        value = min(acc$number_of_casualties),
                        step = 1),
            radioButtons(inputId = "speedRadio",
                         label = "Speed limit",
                         choices = sort(unique(acc$speed_limit))),
            selectInput(inputId = "vehiclesSelect",
                        label = "Number of vehicles",
                        choices = sort(unique(acc$number_of_vehicles)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput(outputId = "map"),
           verbatimTextOutput(outputId = "text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$text <- renderPrint({
        
        input$map_marker_click
        
    })

    output$map <- renderLeaflet({
        
        d <- acc %>%
            filter(label == input$districtSelect,
                   number_of_casualties == input$casualtiesSlider,
                   speed_limit == input$speedRadio,
                   number_of_vehicles == input$vehiclesSelect) %>%
            mutate(group = ifelse(accident_severity == 1, "red", 
                                  ifelse(accident_severity == 2, "orange", "green")))
        
        icons <- iconList(
            red = makeIcon(iconUrl = "skull_red.png", iconWidth = 24, iconHeight = 24),
            orange = makeIcon(iconUrl = "skull_orange.png", iconWidth = 24, iconHeight = 24),
            green = makeIcon(iconUrl = "skull_green.png", iconWidth = 24, iconHeight = 24)
        )
        
        leaflet(d) %>%
            addTiles() %>%
            addMarkers(lng = ~longitude, 
                       lat = ~latitude,
                       icon = ~icons[group], 
                       layerId = ~d_z_accident_index)
        
    })
    
    observeEvent(input$speedRadio, {
        
        d <- acc %>%
            filter(label == input$districtSelect,
                   speed_limit == input$speedRadio)
        
        updateSliderInput(session = session,
                          inputId = "casualtiesSlider",
                          min = min(d$number_of_casualties),
                          max = max(d$number_of_casualties),
                          value = min(d$number_of_casualties))
        
        updateSelectInput(session = session,
                          inputId = "vehiclesSelect",
                          choices = sort(unique(d$number_of_vehicles)))
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
