# Helper file for MPG App
# Initial: 2 April 2020
# Revision: 2 April 2020
# Ray Nelson

## Create a factor with labels
mpg$drv <- factor(mpg$drv,
                  levels = c("4", "f", "r"),
                  labels = c("Four Wheel", "Front", "Rear"))

# Function definition for scatterplot
scatterplot <- function(driving, span) {
  mpg %>% 
    ggplot(aes_string(x = "displ",
                      y = driving,
                      color = "drv")) +
    geom_point(position = "jitter") +
    geom_smooth(colour = "blue", span = span) +
    labs(
      title = "Mileage and engine size are inversely related.",
      x = "Engine Displacement",
      y = paste("Type of Driving:", driving),
      color = "Drive Train"
    ) +
    theme(legend.position = "bottom")
}

