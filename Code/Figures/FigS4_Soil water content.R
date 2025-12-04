#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2025

#Dataset needed
SWC_data.csv

#Load packages
library(tidyverse)
library(ggplot2)

#Set working directory to the folder containing code and data
setwd("~/Downloads")

#Import data
SWC_data <- 
  read_csv("SWC_data.csv")
SWC_data$Intensity_level <-
  factor(SWC_data$Intensity_level,
         levels = c("Control", "Mild", "Intense"))

#Plotting
color_vec <- rev(palette.colors(n = 3, palette = "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")

SWC_Plot <- 
  ggplot(data = SWC_data, 
         aes(x = SWC_level, y = SWC, fill = Intensity_level)) +
  geom_boxplot() +
  scale_fill_manual(values = color_vec) +
  scale_y_continuous(limits = c(0.1, 0.5), 
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5)) +
  geom_hline(yintercept = 0.277, 
             linetype = "dashed", color = "darkorange", linewidth = 0.8) +
  geom_hline(yintercept = 0.187, 
             linetype = "dashed", color = "brown", linewidth = 0.8) +
  labs(x = NULL, y = "Soil water content", fill = "Intensity level") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  theme_bw()

SWC_Plot

#T.test and Linear Regression Model
SWC_ttest <- t.test(SWC~SWC_level, data=SWC_data)
SWC_ttest
SWC_lm <- lm(SWC~SWC_level, data=SWC_data)
summary(SWC_lm)

SWC_mean_sd <- SWC_data %>%
  group_by(SWC_level) %>%
  summarise(avg = mean(SWC), SD= sd(SWC))
SWC_mean_sd
