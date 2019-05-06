#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

load("dane.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dostępność przestrzenna"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("zasieg",
                        "Liczba kilometrów:",
                        min = 1,
                        max = 50,
                        value = 10)
        ),

        mainPanel(
           textOutput("pop_km")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$pop_km <- renderText({
        
        pop <- populacja_odl %>%
            filter(odl_km < input$zasieg) %>%
            summarise(zasieg=sum(pop_15_64)) %>%
            as.character
        
        paste("Liczba osób w wieku od 15-64 lat wynosi:", pop)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
