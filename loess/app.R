# loess smoothing
# Initial: 30 Mar 2020
# Revision: 30 Mar 2020
# Ray Nelson

# Libraries
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MPG for the Last Time"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("span",
                        "Span for Loess",
                        min = 0.05,
                        max = 1,
                        step = 0.1,
                        value = 0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterPlot <- renderPlot({
        mpg %>% 
            ggplot(aes(x = displ, y = hwy, color = drv)) +
            geom_point(position = "jitter") +
            geom_smooth(color = "blue", span = input$span) +
            labs(
                title = "This is a nonlinear relationship",
                x = "Displacement",
                y = "Highway Miles Per Gallon"
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
