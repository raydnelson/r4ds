
# Libraries
library(shiny)
library(mc2d)
library(tidyverse)

# PERT distribution functions
# PERT distribution functions
dPERT <- function(min, mode, max, shape){
    precision <- 2
    x <- seq(min, max, 1 / 10 ^ precision)
    y <- dpert(x, min = min, mode = mode, max = max, shape = shape)
    tibble(x, y)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    # Graph of the PERT PDF
    output$pdf <- renderPlot({
        
        ## Data for graph
        plot_data <- dPERT(min = input$Minimum,
                           mode = input$Mode,
                           max = input$Maximum,
                           shape = input$Shape)
        colnames(plot_data) <- c("Quantiles", "Probability")
        
        ## Comparison graph
        ggplot(data = plot_data, aes(x = Quantiles, y = Probability)) +
            geom_area(alpha = 0.25, show.legend = FALSE) +
            labs(title = "Probability Distribution Function",
                 x = "X",
                 y = "") +
            scale_y_continuous(breaks = NULL)
    })
    
    ## Data for graph
    PERT_data <- reactive({
        data.frame(
        random_PERT = rpert(
            n = input$Sample_Size,
            min = input$Minimum,
            mode = input$Mode,
            max = input$Maximum,
            shape = input$Shape
        )
    )
    })
    
    ## Empirical Density
    
    output$empirical_density <- renderPlot({
        Mean <- mean(PERT_data()$random_PERT)
        Median <- median(PERT_data()$random_PERT)
        ggplot(data = PERT_data(), aes(x = random_PERT)) +
            geom_density(fill = "lightblue") +
            geom_vline(xintercept = Mean, color = "red", width = 2) + 
            geom_vline(xintercept = Median, color = "blue", width = 2) +
            labs( 
                title = "Empirical Distribution Function",
                x = "X",
                y = ""
            )
        
    })
    
    output$violin_plot <- renderPlot({
        
        ggplot(data = PERT_data(), aes(x = factor(0), y = random_PERT)) +
            geom_violin(fill = "lightblue") + 
            geom_boxplot(width = 0.25, fill = "lightgreen") +
            labs(
                title = "Empirical Distribution Function",
                x = "X",
                y = ""
            ) +
            coord_flip()
        
    })

})
