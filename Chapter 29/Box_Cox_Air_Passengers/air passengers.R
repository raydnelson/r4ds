# Box Cox Transformation of Air Passengers
# Initial: 8 April 2019
# Revision: 6 December 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(forecast)
library(shiny)

# Define UI for application completes a Box-Cox Transformation in time series data
ui <- fluidPage(

    # Application title
    titlePanel("Air Passengers"),

    # Sidebar with a slider input for the value of lambda 
    sidebarLayout(
        sidebarPanel(
            sliderInput("lambda",
                        "Lambda:",
                        min = -3,
                        max = 4,
                        step = 0.25,
                        value = 1)
        ),

        # Plot of the time series
        mainPanel(
           plotOutput("tsPlot")
        )
    )
)

# Define server logic required to draw the time series plot
server <- function(input, output) {

    output$tsPlot <- renderPlot({
        AirPassengers %>%
            BoxCox(lambda = input$lambda) %>%
            autoplot() +
            geom_smooth() +
            labs(
                title = "International Airline Passengers are nonlinear and heteroscedastic.",
                subtitle = ("Source: Box and Jenkins, 1976"),
                x = "",
                y = "Thousands"
            )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
