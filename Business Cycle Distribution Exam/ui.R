# Budgeting Under Uncertainty app UI

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  # Application title
  titlePanel("Distribution Shape: PERT"),
  
  # Sidebar distribution and budgeting decision inputs
  sidebarLayout(
    sidebarPanel(
      helpText(h4("Expansion Distribution Parameters", align = "Center")),
      
      numericInput(
        "expansion_min",
        "Expansion Minimum:",
        value = -1,
        min = NA,
        max = NA,
        step = NA
      ),
      
      numericInput(
        "expansion_mode",
        "Expansion Most Likely:",
        value = 5,
        min = NA,
        max = NA,
        step = NA
      ),
      
      numericInput(
        "expansion_max",
        "Expansion Maximum:",
        value = 15,
        min = NA,
        max = NA,
        step = NA
      ),
      
      sliderInput(
        "expansion_shape",
        "Expansion Shape:",
        value = 4,
        min = 1,
        max = 15,
        step = 1
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      helpText(h3("Revenue Growth Rates", align = "Center")),
      plotOutput("revenue_plot")
    )
  )
))
