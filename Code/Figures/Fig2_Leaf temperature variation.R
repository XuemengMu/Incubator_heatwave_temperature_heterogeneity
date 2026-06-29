#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2026

#Datasets needed
Leaf_temperature_day3.csv

#R Packages needed
library(ggplot2)
library(tidyverse)
library(ggpubr)

#Set working directory to the folder containing code and data
setwd("~/Downloads")

#### Figure 2a: within-leaf temperature range of each leaf individuals among treatment groups ####
#Import and prepare data
Temp_range <- read_csv("Leaf_temp_variation_summary.csv")
Temp_range <- Temp_range %>% mutate(HW_intensity = factor(HW_intensity, levels = c("Control", "Mild", "Intense")))

#Plotting
color_vec <- rev(palette.colors(n = 3, palette = "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")

Fig2a <- ggplot(Temp_range, aes(x = temp_range, fill = HW_intensity)) +
  geom_density(alpha = 0.6) +
  facet_grid(SWA_level ~ .) +
  labs(
    x = "Within-leaf temperature range (°C)",
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

Fig2a

#### Figure 2b: Pooled within-leaf temperature distributions across individual leaves ####

#Import data
Temp_data <- subset(read.csv("Leaf_temperature_day3.csv"))
Temp_data <- Temp_data %>%
  dplyr::mutate(across(c(HW_intensity, SWA_level, Plant_id), as.factor))

#Plotting
color_vec <- rev(palette.colors(n = 3, palette = "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")

Fig2b <- ggplot(Temp_data, aes(x = Leaf_temperature, fill = HW_intensity)) +
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
