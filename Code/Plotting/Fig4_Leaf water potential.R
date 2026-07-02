#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2025

#Dataset needed
Main_data.csv

#Load packages
#install.packages(c("rstatix", "ggpubr"))
library(tidyverse)
library(rstatix)
library(ggpubr)

#Set working directory to the folder containing code and data
setwd("~/Downloads")

#Import data
all_data <- 
  read_csv("Main_data.csv")
all_data$Intensity_level <-
  factor(all_data$Intensity_level,
         levels = c("Control", "Mild", "Intense"))


LWP_data <- all_data %>% filter(Day <11)
LWP_data <- LWP_data[!is.na(LWP_data$Leaf_water_potential), ]

LWP_data <- LWP_data %>%
  mutate(
    Duration = factor(Duration, levels = c(3, 6, 10)),
    Intensity_level = factor(Intensity_level, levels = c("Control", "Mild", "Intense")),
    SWA_level = factor(SWA_level, levels = c("Dry", "Wet"))
  )
stat.test <- LWP_data %>%
  group_by(SWA_level, Duration) %>%
  tukey_hsd(Leaf_water_potential ~ Intensity_level)

stat.test <- stat.test %>%
  add_xy_position(
    x = "Duration",
    group = "Intensity_level",
    dodge = 0.8
  )

color_vec <- rev(palette.colors(3, "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")


LWP_fig <- ggplot(
  LWP_data,
  aes(x = Duration, y = Leaf_water_potential, colour = Intensity_level)
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.6,
    outlier.shape = NA
  ) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.8
    ),
    alpha = 0.5
  ) +
  facet_wrap(~ SWA_level) +
  scale_color_manual(values = color_vec) +
  stat_pvalue_manual(
    stat.test,
    label = "p.adj.signif",   # shows *, **, ***, ****
    tip.length = 0.01,
    hide.ns = TRUE            # hide non-significant comparisons
  ) +
  labs(
    x = "Heatwave duration (days)",
    y = expression("Leaf water potential, " * Psi[leaf] * " (MPa)"),
    colour = "Heatwave intensity"
  ) +
  theme_bw()

LWP_fig

ggsave("Leaf_water_potential.tiff", LWP_fig, width = 6, height = 3)
