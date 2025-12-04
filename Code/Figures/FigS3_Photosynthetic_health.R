#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2025

#Dataset needed
Main_data.csv

#Load packages
library(dplyr)
library(readr)
library(viridis)

#Set working directory to the folder containing code and data
setwd("~/Downloads")

#Import data
all_data <- 
  read_csv("Main_data.csv")
all_data$Intensity_level <-
  factor(all_data$Intensity_level,
         levels = c("Control", "Mild", "Intense"))

#Prepare data (Fv/Fm and Fv/Fm difference after treatment(<11days), NO recovery period(>=17days))
Fv_Fm_data <- all_data %>% select(Day,Fv_Fm, Fv_Fm_difference,Intensity_level, SWA_level)%>% 
  filter(!(is.na(Fv_Fm) & is.na(Fv_Fm_difference)))%>% 
  filter(Day < 11)%>%
  mutate(
    Fv_Fm = as.numeric(Fv_Fm),
    Fv_Fm_difference = as.numeric(Fv_Fm_difference),
    Day = as.factor(Day)) 

#Plotting 
color_vec <- rev(palette.colors(n = 3, palette = "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")

Fvfm_plot <- ggplot(Fv_Fm_data, aes(x = Day)) +
  geom_violin(aes(y = Fv_Fm)) +
  geom_jitter(aes(y = Fv_Fm, color = Intensity_level),
              size = 0.2, alpha = 0.7) +
  geom_boxplot(aes(y = Fv_Fm_difference, color = Intensity_level)) +
  geom_hline(yintercept = 0.83, color = "red", linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~SWA_level) +
  scale_x_discrete(breaks = c(0, 3, 6, 10)) +
  scale_y_continuous(
    breaks = c(0.83, 0.8, 0.7, 0.6, 0.5, 0.4),
    name = "Fv/Fm",
    sec.axis = sec_axis(
      ~ .,
      breaks = c(0.3, 0.2, 0.1, 0, -0.1, -0.2, -0.3, -0.4),
      name = "Fv/Fm difference (relative to Control)"
    )
  ) +
  scale_color_manual(values = color_vec) +
  labs(
    x = "Heatwave Duration (Day)",
    y = "Fv/Fm",
    color = "Heatwave intensity"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.caption = element_text(size = 12, hjust = 0.5)
  )

Fvfm_plot

ggsave("FvFm_plot.png", 
       plot = Fvfm_plot, 
       width = 6, 
       height = 6, 
       dpi = 300)

