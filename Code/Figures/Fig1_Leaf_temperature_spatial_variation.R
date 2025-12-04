#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2025

#Datasets needed
Leaf_temperature_day3.csv

#Load packages
library(ggplot2)
library(tidyverse)

#Set working directory to the folder containing code and data
setwd("~/Downloads")

#Import and prepare data
Temp_data <- read_csv("Leaf_temperature_day3.csv")
Temp_data <- Temp_data %>% mutate(HW_intensity = factor(HW_intensity, levels = c("Control", "Mild", "Intense")))

#Plotting
color_vec <- rev(palette.colors(n = 3, palette = "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")

Temp_data_plot <- ggplot(Temp_data, aes(x = Leaf_temperature, fill = HW_intensity)) +
  geom_density(alpha = 0.6) +
  facet_grid(SWA_level ~ .) +
  labs(
    x = "Leaf temperature (°C)",
    y = "Density",
    fill = "Heatwave intensity"
  ) +
  scale_fill_manual(
    values = color_vec,
    labels = c(
      "Control" = "Control (25°C)",
      "Mild"    = "Mild (35°C)",
      "Intense" = "Intense (40°C)"
    )
  ) +
  theme_bw() +
  theme(
    axis.text   = element_text(size = 10),
    axis.title  = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    strip.text  = element_text(size = 10),
    plot.caption = element_text(size = 12, hjust = 0.5)
  )

Temp_data_plot
