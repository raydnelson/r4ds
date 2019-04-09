# Lowess Smoothing App
# Initial: 8 Apr 2019
# Revision: 8 Apr 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Car Mileage Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("span",
                        "Loess Smoothing Constant",
                        min = 0.05,
                        max = 0.95,
                        step = 0.05,
                        value = 0.4),
            
            selectInput("response", label = h3("Response Variable:"), 
                        choices = c("hwy", "cty")),
            
            selectInput("color_by", label = h3("Color Points by:"), 
                        choices = c("drv", "class"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("loess_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$loess_plot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        mpg %>% 
            ggplot(aes_string(x = "displ", y = input$response)) +
            geom_point(aes_string(color = input$color_by)) +
            geom_smooth(span = input$span, se = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
