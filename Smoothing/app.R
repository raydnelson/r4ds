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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Smoothing Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("smooth",
                        "Smoothing Parameter",
                        min = 0.05,
                        max = 1,
                        step = .05,
                        value = .5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        faithful %>% 
            ggplot(aes(y = eruptions, x = waiting)) +
            geom_point() +
            geom_smooth(span = input$smooth)
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
