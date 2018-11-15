# Shiny App to compare t and normal distributions

# Libraries --------------------------------------------------------------------
library(shiny)
library(tidyverse)

# Function definition ----------------------------------------------------------

dist_compare <- function(df){
    # Range of x values
    x <- seq(-5, 5, 0.1)
    
    # Normal Distribution
    normal <- dnorm(x)
    
    # Student's t distribution
    t <- dt(x, df = df)
    
    # Create the data frame used for plotting
    
    plot_data <- data.frame(x, normal, t) %>% 
        gather(normal, t, key = distribution, value = probability)
    
    # plot of  
    plot_data %>% 
        ggplot(aes(x = x, y = probability, fill = distribution)) +
        geom_ribbon(aes(ymin = 0, ymax = probability), alpha = 0.5) +
        geom_line() +
        labs(
            title = "Effect of Degress of Freedom on Student-t Distribution",
            subtitle = "Standard Normal versus the Student-t Distribution",
            x = paste0("Degress of Freedom: ", df),
            y = "Probability",
            fill = "Density Functions"
        ) +
        scale_fill_brewer(palette = "Set1")
}

# Define UI for application that draws the probability density functions -------
ui <- fluidPage(

    # Application title
    titlePanel("Comparison of t and normal distributions to show tail thickness"),

    # Sidebar with a slider input for number of degress of freedom 
    sidebarLayout(sidebarPanel(
        sliderInput(
            "df",
            "Degress of Freedom for t:",
            min = 2,
            max = 50,
            value = 30
        )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot")))
)

# Define server logic required to compare densities

server <- function(input, output) {

    output$distPlot <- renderPlot({
        # Draw the plot using the function dist_compare
        dist_compare(input$df)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
