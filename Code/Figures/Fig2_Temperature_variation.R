#Figure of Within Leaf Temperature Variation (GLMMTMB)
library(ggplot2)
library(readr)

##### Load data----
#Data Import
Temp_data <- read_csv("~/Desktop/UNSW work relevant/Chapter 2 (indoor heatwave experiment)/Data analysis/Datasheets/Datasheets_in_use/Temp_variance_day_3_expand.csv") #data collected on all the plants on Day 3
#Variable Transformation
Temp_data$HW_duration <- as.factor(Temp_data$HW_duration)

##### Plot----
color_vec <- rev(palette.colors(n = 3, palette = "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")

Temp_data$HW_intensity <- factor(Temp_data$HW_intensity,
                                 levels = c("Control", "Mild", "Intense"))
Temp_data_plot <- ggplot(data=Temp_data, 
                         aes(x=Leaf_temperature, 
                             fill=HW_intensity)) +
  geom_density(alpha=0.6) +
  facet_grid(SWA ~ .) +
  labs(x = "Leaf temperature (°C)", 
       y = "Density",
       fill = "Heatwave intensity",        # Change text of legend
       caption = "Fig.1 Leaf temperature spatial variation in 3-day heatwaves") +
  scale_fill_manual(values = color_vec,  # Define colors
                    labels = c("Control"="Control (25°C)",
                               "Mild"="Mild (35°C)",
                               "Intense"="Intense (40°C)"
                    )) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 10),
        plot.caption = element_text(size = 12, hjust = 0.5))

Temp_data_plot
