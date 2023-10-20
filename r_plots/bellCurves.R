library(ggplot2)
library(dplyr)
library(ggpubr)
library(cowplot)
options(scipen=999)
# Load your dataset
df <- read.csv('r_plots/wssc_sales.csv')

# Create separate data frames for each RF Sector
rf_sector_data <- split(df, df$RF_Sector)

# Create a list to store the plots
bell_curve_plots <- list()

# Loop through each RF Sector
for (sector in names(rf_sector_data)) {
  data <- rf_sector_data[[sector]]
  
  # Create the bell curve plot
  p <- ggplot(data, aes(x = dollar_acre)) +
    geom_histogram() +
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

########## Potential Update ################
# Loop through each RF Sector
for (sector in names(rf_sector_data)) {
  data <- rf_sector_data[[sector]]
  data$RF_Sector<- as.factor(data$RF_Sector)
  
  # Calculate summary statistics for the current RF Sector
  median_value <- median(data$dollar_acre)
  mean_value <- mean(data$dollar_acre)
  lower_sd_limit <- mean_value - sd(data$dollar_acre)
  upper_sd_limit <- mean_value + sd(data$dollar_acre)
  
  phist <- gghistogram(
    data, x = "dollar_acre", 
    rug = TRUE,
    color = "RF_Sector", 
    fill ="RF_Sector", 
    palette = c("#93d2cb"),
    alpha = 0.75
  ) +
  labs(title = paste("Return Flow Sector", sector),
         x = "Price per Acre",
         y = "Number of Sales") +
  scale_x_continuous(labels = dollar_format(scale = 1, prefix = "$", big.mark = ",")) +
  theme_minimal() +
  theme(legend.position = "none")
  
  pdensity <- ggdensity(
    data, x = "dollar_acre",
    add="median",
    color= "RF_Sector", fill = "RF_Sector",
    palette = c("#232e3f"),
    alpha = 0.4
  ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), position = "right")  +
    theme_half_open(11, rel_small = 1) +
    labs(y= "Density") +
    rremove("x.axis")+
    rremove("xlab") +
    rremove("x.text") +
    rremove("x.ticks") +
    rremove("legend") +
    geom_vline(data=data, xintercept=lower_sd_limit, color = "#232e3f",
               linetype = 'dashed', linewidth = 0.5) +
    geom_vline(data=data, xintercept=upper_sd_limit, color = "#232e3f",
               linetype = 'dashed', linewidth = 0.5) 
  
  pdensity.data <- ggplot_build(pdensity)$data[[1]]
  
  pdensity<- pdensity + 
    annotate('text', x = (0.90 * lower_sd_limit) , y = mean(pdensity.data$density), 
             label=paste0("Lower SD: $", prettyNum(round(lower_sd_limit, 2), big.mark=',')), 
             color = 'black', size = 2.5, angle = 90, fontface="bold") +
    annotate('text', x = (0.95 * median_value), y = mean(pdensity.data$density), 
             label=paste0("Median: $", prettyNum(round(median_value, 2), big.mark=',')), 
             color = 'black', size = 2.5, angle = 90, fontface="bold") +
    annotate('text', x = (1.05 * upper_sd_limit), y = mean(pdensity.data$density), 
             label=paste0("Upper SD: $", prettyNum(round(upper_sd_limit, 2), big.mark=',')), 
             color = 'black', size = 2.5, angle = 90, fontface="bold")
  
  # 3. Align the two plots and then overlay them.
  aligned_plots <- align_plots(phist, pdensity, align="hv", axis="tblr")
  
  j<- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
  
  print(j)
  
  ggsave(filename=paste0("r_plots/RF_Sector_density_",sector,".png"), 
         plot = j,
         height=6, width=6)
  
}
