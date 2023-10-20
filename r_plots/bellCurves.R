library(ggplot2)
library(dplyr)

# Load your dataset
df <- read.csv('C:/Users/tommy/Documents/GIS/crd_plots/wssc_sales.csv')

# Create separate data frames for each RF Sector
rf_sector_data <- split(df, df$RF_Sector)

# Create a list to store the plots
bell_curve_plots <- list()

# Loop through each RF Sector
for (sector in names(rf_sector_data)) {
  data <- rf_sector_data[[sector]]
  
  # Create the bell curve plot
  p <- ggplot(data, aes(x = dollar_acre)) +
    geom_density(fill = "#9daec8", alpha = 0.5) +
    labs(title = paste("Return Flow Sector", sector),
         x = "Price per Acre",
         y = "Number of Sales") +
    scale_x_continuous(labels = dollar_format(scale = 1, prefix = "$", big.mark = ","))+
    theme_minimal()
  
  # Calculate summary statistics for the current RF Sector
  median_value <- median(data$dollar_acre)
  lower_sd_limit <- median_value - sd(data$dollar_acre)
  upper_sd_limit <- median_value + sd(data$dollar_acre)
  
  # Add labels for Median, Upper SD, and Lower SD
  p <- p +
    annotate(
      "text",
      x = median_value, y = max(density(data$dollar_acre)$y) * 0.95,
      label = paste("Median:", round(median_value, 2)),
      color = "black",
      size = 2
    ) +
    annotate(
      "text",
      x = lower_sd_limit, y = max(density(data$dollar_acre)$y) * 1.0,
      label = paste("Lower SD:", round(lower_sd_limit, 2)),
      color = "black",
      size = 2
    ) +
    annotate(
      "text",
      x = upper_sd_limit, y = max(density(data$dollar_acre)$y) * 1.0,
      label = paste("Upper SD:", round(upper_sd_limit, 2)),
      color = "black",
      size = 2
    )
  
  bell_curve_plots[[sector]] <- p
}

# Print the bell curve plots
for (sector in names(bell_curve_plots)) {
  print(bell_curve_plots[[sector]])
}

