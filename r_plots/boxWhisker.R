library(ggplot2)
library(scales)
library(extrafont)

# Load your dataset
df <- read.csv('C:/Users/tommy/Documents/GIS/crd_plots/wssc_sales.csv')

# Define a custom color palette (replace these colors with your preferred colors)
custom_colors <- c("#bce3df","#9daec8","#5791a7",
                   "#7a9d62","#2a6f68")

# Convert RF_Sector to a factor with custom colors
df$RF_Sector <- factor(df$RF_Sector, levels = unique(df$RF_Sector))
levels(df$RF_Sector) <- custom_colors

# Define custom labels for RF_Sector
custom_labels <- c("Sector 1", "Sector 2", "Sector 3", "Sector 4", "Sector 5")

p_box <- ggboxplot(df, x = "RF_Sector", y = "dollar_acre", 
                   fill = "RF_Sector",
                   add = "jitter") +
  scale_fill_identity() +  # Use the custom colors
  scale_x_discrete(labels = custom_labels) +  # Specify custom labels for the x-axis
  labs(title = "Box and Whisker Plot by RF Sector",
       x = "RF Sector",
       y = "Price per Acre")+
  scale_y_continuous(labels = dollar_format(scale = 1, prefix = "$", big.mark = ","))+
  theme(axis.text.x = element_text(size = 12, color = "black"),  
        axis.title = element_text(size = 14, color = "black"),  
        plot.title = element_text(size = 16, color = "black"),  
        axis.text = element_text(size = 10,color = "black"),
        )

# Print the box and whisker plot
print(p_box)