# MPG Shiny App
# Initial: 2 Apr 2020
# Revision: 6 Apr 2020
# Ray Nelson

# Libraries and helper code
library(tidyverse)
library(shiny)
source("helper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Highway and City Miles Per Gallon"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            # Slider input for span
            sliderInput(
                "span",
                "Loess Smoothing Parameter:",
                min = 0.2,
                max = 1,
                step = 0.1,
                value = 0.4
            ),
            # Input of variable (highway or city driving)
            selectInput(
                "driving",
                label = h3("Type of Driving"),
                choices = list("Highway" = "hwy",
                               "City" = "cty"),
                selected = "hwy"
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("scatterPlot"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Code for scatterplot
    output$scatterPlot <- renderPlot({
        scatterplot(input$driving, input$span)
    })
    # End of code for scatterplot
}

# Run the application
shinyApp(ui = ui, server = server)
