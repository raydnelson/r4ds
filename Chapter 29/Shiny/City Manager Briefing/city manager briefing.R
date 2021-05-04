# City Manager Briefing App
# Initial: 9 Apr 2019
# Revision: 6 December 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(forecast)
library(fImport)
library(shiny)

# Define UI for application that gets data from FRED and renders graphs
ui <- fluidPage(

    # Application title
    titlePanel("City Manager's Economic Environment"),

    # Sidebar with selection and numerical inputs 
    sidebarLayout(
        sidebarPanel(
            selectInput("indicator",
                        "Economic Indicator:",
                        choices = list(
                            "Provo-Orem Housing Prices" = "ATNHPIUS39340Q",
                            "Provo-Orem Nonfarm Employment" = "PROV349NA",
                            "US Retail Sales" = "RSAFSNA"
                        )),
            
            numericInput("horizon",
                         "Forecast Horizon in Months",
                         value = 12)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("forecastPlot")
        )
    )
)

# Define server logic required to draw a forecast plot
server <- function(input, output) {

    output$forecastPlot <- renderPlot({
        fredSeries(input$indicator, from = "1992-01-01") %>% 
            forecast(h = input$horizon) %>% 
            autoplot() +
            labs(
                title = "Forecast",
                x = "",
                y = "Indicator"
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
