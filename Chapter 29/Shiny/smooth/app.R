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
    titlePanel("MPG Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("smooth",
                        "Smoothing Paramter:",
                        min = 0.2,
                        max = 1,
                        step = 0.05,
                        value = 0.4)
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
        mpg %>% 
            ggplot(aes(x = displ, y = hwy))+
            geom_point(aes(color = drv), position = "jitter") +
            geom_smooth(span = input$smooth, se = FALSE)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
