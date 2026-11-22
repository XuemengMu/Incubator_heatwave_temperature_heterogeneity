#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2026

#Datasets needed
Leaf_temperature_day10.csv
Leaf_temperature_variation_day10.csv

#R Packages needed
library(tidyverse)
library(ggpubr)

#Set working directory to the folder containing code and data
setwd("~/Downloads")



#### Appendix Figure A3a: within-leaf temperature range of each leaf individuals within 10-day, dry, control VS. 10-day, dry, intense heatwave groups ####
#Import and prepare data
Temp_range <- read_csv("Leaf_temperature_variation_day10.csv")
Temp_range <- Temp_range %>% mutate(HW_intensity = factor(HW_intensity, levels = c("Control", "Mild", "Intense")))

#Import and prepare data
Temp_range <- read_csv("Leaf_temperature_variation_10day.csv")
Temp_range <- Temp_range %>%
  dplyr::mutate(
    HW_intensity = dplyr::recode(
      HW_intensity,
      "C" = "Control",
      "I" = "Intense"
    )
  )
Temp_range <- Temp_range %>% mutate(HW_intensity = factor(HW_intensity, levels = c("Control", "Intense")))

#Plotting
color_vec <- palette.colors(n = 3, palette = "Set 1")[c(3, 1)]
names(color_vec) <- c("Control", "Intense")

Fig2a <- ggplot(Temp_range, aes(x = temp_range, fill = HW_intensity)) +
  geom_density(alpha = 0.6) +
  facet_grid(
    SWA_level ~ .,
    labeller = labeller(SWA_level = c("D" = "Dry"))
  ) +
  labs(
    x = "Within-leaf temperature range (°C)",
    y = "Density",
    fill = "Heatwave intensity"
  ) +
  scale_fill_manual(values = color_vec) +
  theme_bw()

Fig2a



#### Appendix Figure A3b: Pooled within-leaf temperature spatial distributions across individual leaves within 10-day, dry, control VS. 10-day, dry, intense heatwave groups ####

#Import data
Temp_data <- subset(read.csv("Leaf_temperature_day10.csv"))
Temp_data <- Temp_data %>%
  dplyr::mutate(across(c(HW_intensity, SWA_level, Plant_id), as.factor))

Temp_data <- Temp_data %>%
  dplyr::mutate(
    HW_intensity = dplyr::recode(
      HW_intensity,
      "C" = "Control",
      "I" = "Intense"
    )
  )

Temp_data <- Temp_data %>% mutate(HW_intensity = factor(HW_intensity, levels = c("Control", "Intense")))

#Plotting
color_vec <- palette.colors(n = 3, palette = "Set 1")[c(3, 1)]
names(color_vec) <- c("Control", "Intense")

Fig2b <- ggplot(Temp_data, aes(x = Leaf_temperature, fill = HW_intensity)) +
  geom_density(alpha = 0.6) +
  facet_grid(
    SWA_level ~ .,
    labeller = labeller(SWA_level = c("D" = "Dry"))
  ) +
  labs(
    x = "Within-leaf temperature (°C)",
    y = "Density",
    fill = "Heatwave intensity"
  ) +
  scale_fill_manual(
    values = color_vec,
    labels = c(
      "Control" = "Control (25°C)",
      "Intense" = "Intense (40°C)"
    )
  ) +
  theme_bw()

Fig2b



#### Layout of Fig2a + Fig2b ####
common_theme <- theme_classic() +
  theme(
    legend.title = element_blank(),
    plot.tag = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

Fig2a_legend <- Fig2a +
  common_theme +
  theme(legend.position = "none")

Fig2b_legend <- Fig2b +
  common_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.x = unit(0.4, "cm")
  ) +
  guides(
    colour = guide_legend(nrow = 1, override.aes = list(size = 1.5)),
    fill = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1, override.aes = list(size = 3))
  )

combined_plot <- ggarrange(
  Fig2a_legend,
  Fig2b_legend,
  ncol = 2,
  labels = c("a", "b"),
  common.legend = TRUE,
  legend = "bottom"
)

combined_plot
