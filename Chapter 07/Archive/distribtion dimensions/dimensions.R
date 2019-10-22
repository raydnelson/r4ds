# Distributions Dimensions
# Initial: 30 May 2019
# Revision: 30 May 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(magrittr)
library(cowplot)

# Functions for creating information
plots <- function(observations) {
  # Create the data tibble for graphs
  plot_data <- tibble(category = rep("", length(observations)),
                      random_variate = observations)
  mean_data <- tibble(category = "", Mean = mean(observations))
  
  # Create the Violin Plot
  Violin_plot <- plot_data %>%
    ggplot(aes(x = category, y = random_variate)) +
    geom_violin(fill = "lightgreen") +
    geom_boxplot(width = 0.25, fill = "lightblue") +
    geom_point(
      data = mean_data,
      aes(x = category, y = Mean),
      shape = 23,
      fill = "red",
      size = 3
    ) +
    labs(title = "Violin Plot",
         x = "",
         y = "Random Variable") +
    coord_flip() +
    scale_x_discrete(breaks = NULL)
  
  Histogram <- plot_data %>%
    ggplot(aes(x = random_variate)) +
    geom_histogram(aes(y = ..density..), fill = "lightblue", color = "grey45") +
    geom_density(fill = "lightgreen", alpha = 0.6) +
    scale_y_continuous(breaks = NULL) +
    labs(title = "Histogram and Density Trace",
         x = "Random Variate")
  
  plot_grid(Histogram, Violin_plot, ncol = 1)
}

# Summary statistics
Summary_stats <- function(observations) {
  Mean <- mean(observations)
  Median <- median(observations)
  Standard_Deviation <- sd(observations)
  Interquartile_Range <- IQR(observations)
  Symmetry <- skewness(observations)
  Outliers <- kurtosis(observations)
  
  summary_stats <- rbind(Mean, Median, Standard_Deviation, Interquartile_Range, Symmetry, Outliers) %>%
    round(2)
  colnames(summary_stats) <- c("Summary_Statistics")
  return(summary_stats)
}

# Actual data

# Import that data from Excel
observations <- readClipboard() %>% as.numeric()
Summary_stats(observations)
plots(observations)
