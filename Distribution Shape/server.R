# shiny UI for the budgeting app

# Libraries
library(shiny)
library(mc2d)
library(tidyverse)

# PERT distribution functions
dPERT <- function(min, mode, max, shape, state_of_economy){
  precision <- 2
  x <- seq(min, max, 1 / 10 ^ precision)
  y <- dpert(x, min = min, mode = mode, max = max, shape = shape)
  business_cycle <- rep(state_of_economy, length(x))
  tibble(business_cycle, x, y)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$revenue_plot <- renderPlot({
    
  # Graph the Expansion PERT distribution
    
    ## Data for graph
    expansion <- dPERT(min = input$expansion_min,
                       mode = input$expansion_mode,
                       max = input$expansion_max,
                       shape = input$expansion_shape,
                       state_of_economy = "Expansion")
    recession <- dPERT(min = input$recession_min,
                       mode = input$recession_mode,
                       max = input$recession_max,
                       shape = input$recession_shape,
                       state_of_economy = "Recession")
    plot_data <- rbind(expansion, recession)
    colnames(plot_data) <- c("Cycle", "Quantiles", "Probability")
    plot_data$Cycle <- factor(plot_data$Cycle,
                              levels = c("Expansion", "Recession"),
                              ordered = TRUE)
    
    ## Comparison graph
    ggplot(data = plot_data, aes(x = Quantiles, y = Probability, fill = Cycle)) +
      geom_area(alpha = 0.25, show.legend = FALSE) +
      facet_grid(rows = vars(Cycle)) +
      labs(title = "Revenue Estimates",
           subtitle = "Business Cycle Regimes",
           x = "Percentage Change",
           y = "") +
      scale_y_continuous(breaks = NULL)
  })
  
})
