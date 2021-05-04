# Shiny
# Initial: April 1, 2020
# Revision: April 1, 2020
# Ray Nelson

# Libraries
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("span",
                        "Loess Smoothing Parameter:",
                        min = 0.01,
                        max = 1,
                        step = 0.05,
                        value = 0.25)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot"),
           plotOutput("scatterPlot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterPlot <- renderPlot({
        mpg %>%
            ggplot(aes(x = displ, y = hwy)) +
            geom_point(position = "jitter") +
            geom_smooth(span = input$span)
    })
    
    output$scatterPlot2 <- renderPlot({
        mpg %>%
            ggplot(aes(x = displ, y = hwy, color = drv)) +
            geom_point(position = "jitter") +
            geom_smooth(span = input$span)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
