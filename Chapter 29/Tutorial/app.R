# Shiny Tutorial
# Initial: 31 Mar 2020
# Revision: 31 Mar 2020
# Ray Nelson

# Libraries
library(tidyverse)
library(fpp3)
library(Quandl)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
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
        # generate bins based on input$bins from ui.R
        faithful %>%
            ggplot(aes(x = waiting)) +
            geom_histogram(bins = input$bins,
                           fill = "lightblue",
                           color = "blue",
                           alpha = 0.3) +
            labs(title = "Waiting time Between eruptions of Old Faithful",
                 x = "Minutes") +
            theme(
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank()
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
