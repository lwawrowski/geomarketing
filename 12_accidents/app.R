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

load("acc.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Map of accidents"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "casualtiesSlider", 
                        label = "Number of casualties:",
                        min = min(acc$number_of_casualties),
                        max = max(acc$number_of_casualties),
                        value = max(acc$number_of_casualties),
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
           leafletOutput(outputId = "map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$map <- renderLeaflet({
        
        d <- acc %>%
            filter(local_authority_district == 1,
                   number_of_casualties == input$casualtiesSlider,
                   speed_limit == input$speedRadio,
                   number_of_vehicles == input$vehiclesSelect)
        
        leaflet(d) %>%
            addTiles() %>%
            addMarkers(lng = ~longitude, lat = ~latitude)
        
    })
    
    observeEvent(input$speedRadio, {
        
        d <- acc %>%
            filter(local_authority_district == 1,
                   speed_limit == input$speedRadio)
        
        updateSliderInput(session = session,
                          inputId = "casualtiesSlider",
                          min = min(d$number_of_casualties),
                          max = max(d$number_of_casualties),
                          value = max(d$number_of_casualties))
        
        updateSelectInput(session = session,
                          inputId = "vehiclesSelect",
                          choices = sort(unique(d$number_of_vehicles)))
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
