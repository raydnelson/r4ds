# Shape of Probability Distribution
# Initial:  1 Feb 2019
# Revision:  6 December 2019
# Ray Nelson

# Libraries
library(shiny)
library(mc2d)
library(tidyverse)

# PERT distribution functions
dPERT <- function(min, mode, max, shape){
    precision <- 2
    x <- seq(min, max, 1 / 10 ^ precision)
    y <- dpert(x, min = min, mode = mode, max = max, shape = shape)
    tibble(x, y)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Distribution Shape"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            numericInput(
                "Minimum",
                "Minimum:",
                value = 0,
                min = NA,
                max = NA,
                step = NA
            ),
            
            numericInput(
                "Mode",
                "Most Likely:",
                value = 4,
                min = NA,
                max = NA,
                step = NA
            ),
            
            numericInput(
                "Maximum",
                "Maximum:",
                value = 15,
                min = NA,
                max = NA,
                step = NA
            ),
            
            sliderInput(
                "Shape",
                "Shape:",
                value = 4,
                min = 1,
                max = 30,
                step = 1
            ),
            
            numericInput(
                "Sample_Size",
                "Sample Size:",
                value = 100,
                min = 1,
                max = 10000,
                step = 100
            )
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("pdf"),
            plotOutput("empirical_density"),
            helpText("Note: Red is the mean and blue is the median"),
            plotOutput("violin_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
}

# Run the application 
shinyApp(ui = ui, server = server)
