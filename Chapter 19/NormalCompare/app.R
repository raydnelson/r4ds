# Comparison of normal distributions

library(shiny)

# Function to draw distributions

compare <- function(mu, sigma, minimum, maximum){
    # Create the pdfs
    x <- seq(minimum, maximum, 0.1)
    standard <- dnorm(x, mean = 0, sd = 1)
    normal <- dnorm(x, mean = mu, sd = sigma)
    
    # Plot the pdfs
    data.frame(x, standard, normal) %>% 
        ggplot(aes(x = x)) +
            geom_ribbon(aes(ymin = 0, ymax = standard), fill = "blue", alpha = 0.25) +
            geom_line(aes(y = standard), color = "blue") +
            geom_ribbon(aes(ymin = 0, ymax = normal), fill = "red", alpha = 0.25) +
            geom_line(aes(y = normal), color = "red") +
        labs(
            x = "",
            y = "Probability"
        )
}

# Define UI for application that draws distributions
ui <- fluidPage(

    # Application title
    titlePanel("Normal Distribution Comparison"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mu",
                        "Mean of Normal:",
                        min = -5,
                        max = 5,
                        value = 0),
            sliderInput("sigma",
                        "Standard Deviation of Normal:",
                        min = 0.1,
                        max = 5,
                        value = 1, 
                        step = 0.1),
            sliderInput("minimum",
                        "Minimum for x:",
                        min = -20,
                        max = 0,
                        value = -3),
            sliderInput("maximum",
                        "Maximum for x:",
                        min = 0,
                        max = 20,
                        value = 3)
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
        compare(input$mu, input$sigma, input$minimum, input$maximum)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
