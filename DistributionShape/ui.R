# Shape of Probability Distribution
# Initial:  1 Feb 2019
# Revision:  1 Feb 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Distribution Shape"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            numericInput(
                "Minimum",
                "Minimum:",
                value = -1,
                min = NA,
                max = NA,
                step = NA
            ),
            
            numericInput(
                "Mode",
                "Most Likely:",
                value = 5,
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
))
