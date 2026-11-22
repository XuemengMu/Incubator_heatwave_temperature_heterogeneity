#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2026

#Datasets needed
Leaf_temperature_day10.csv

#R Packages needed
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)

#Set working directory to the folder containing code and data
setwd("~/Downloads")

#Import data
Temp_data <- subset(read.csv("Leaf_temperature_day10_expanded.csv"))
Temp_data <- Temp_data %>%
  dplyr::mutate(across(c(HW_intensity, SWA_level, Plant_id), as.factor))

#### Find the leaf (and plant) with the max within-leaf temperature variation ####
leaf_temp_range <- Temp_data %>%
  group_by(Plant_id, Leaf_id, HW_intensity, SWA_level) %>%
  summarise(
    min_leaf_temp = ifelse(all(is.na(Leaf_temperature)), NA_real_, min(Leaf_temperature, na.rm = TRUE)),
    max_leaf_temp = ifelse(all(is.na(Leaf_temperature)), NA_real_, max(Leaf_temperature, na.rm = TRUE)),
    temp_difference = max_leaf_temp - min_leaf_temp,
    n_pixels = sum(!is.na(Leaf_temperature)),
    .groups = "drop"
  )

max_diff_leaf <- leaf_temp_range %>%
  slice_max(temp_difference, n = 1, with_ties = TRUE)

max_diff_leaf

#### Compare the max within-leaf temperature variation among treatment groups ####
# 1. Calculate temperature variation within each individual leaf
leaf_temp_variation <- Temp_data %>%
  group_by(
    Plant_id,
    Leaf_id,
    HW_intensity,
    SWA_level
  ) %>%
  summarise(
    min_leaf_temp = min(Leaf_temperature, na.rm = TRUE),
    max_leaf_temp = max(Leaf_temperature, na.rm = TRUE),
    temp_range = max_leaf_temp - min_leaf_temp,
    n_pixels = sum(!is.na(Leaf_temperature)),
    .groups = "drop"
  )

# 2. Calculate mean and maximum within-leaf temperature variation for each treatment group
treatment_temp_variation <- leaf_temp_variation %>%
  group_by(
    HW_intensity,
    SWA_level
  ) %>%
  summarise(
    mean_temp_variation = mean(temp_range, na.rm = TRUE),
    max_temp_variation = max(temp_range, na.rm = TRUE),
    n_leaves = n(),
    .groups = "drop"
  )

treatment_temp_variation

# write.csv(leaf_temp_variation, file = "~/Desktop/Leaf_temperature_variation_10day.csv", row.names = FALSE)

#### Figure 2a: within-leaf temperature range of each leaf individuals among treatment groups ####
#Load packages

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
  labs(
    x = "Within-leaf temperature range (°C)",
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
Temp_data <- Temp_data %>%
  dplyr::mutate(
    HW_intensity = dplyr::recode(
      HW_intensity,
      "C" = "Control",
      "I" = "Intense"
    )
  )

Temp_data <- Temp_data %>% mutate(HW_intensity = factor(HW_intensity, levels = c("Control", "Intense")))

color_vec <- palette.colors(n = 3, palette = "Set 1")[c(3, 1)]
names(color_vec) <- c("Control", "Intense")

Fig2b <- ggplot(Temp_data, aes(x = Leaf_temperature, fill = HW_intensity)) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Within-leaf temperature (°C)",
    y = "Density",
    fill = "HW_intensity"
  ) +
  scale_fill_manual(
    values = color_vec,
    labels = c(
      "Control" = "Control (25°C)",
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

##Layout of Fig2a + Fig2b

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

