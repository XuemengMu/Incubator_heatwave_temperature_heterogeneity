#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2025

#Dataset needed
Main_data.csv

#Load packages
library(tidyr)
library(grid)
library(gridExtra)

#Set working directory to the folder containing code and data
setwd("~/Downloads")

#Import data
all_data <- 
  read_csv("Main_data.csv")
all_data$Intensity_level <-
  factor(all_data$Intensity_level,
         levels = c("Control", "Mild", "Intense"))

#Calculate leaf number difference
Leaf_number_data <- all_data %>%
  dplyr::select(Pot, Day, Duration, Intensity_level, SWA_level, Leaf_number) %>%
  filter(!is.na(Leaf_number)) %>%
  group_by(Pot) %>%
  mutate(Leaf_number_difference = c(NA, diff(Leaf_number))) %>%
  ungroup() %>%
  filter(Day < 11) %>%
  dplyr::select(Duration, SWA_level, Intensity_level, Pot, Leaf_number_difference) %>%
  filter(!is.na(Leaf_number_difference))

#Prepare treatment & recovery datasets

LN_data_treatment <- Leaf_number_data %>%
  rename(LND_treatment = Leaf_number_difference)

LN_data_recovery <- all_data %>%
  filter(Day > 11) %>%
  dplyr::select(Pot, Leaf_number_difference) %>%
  rename(LND_recovery = Leaf_number_difference)

LN_data_combined <- LN_data_treatment %>%
  left_join(LN_data_recovery, by = "Pot") %>%
  mutate(LND_recovery_original = LND_recovery + LND_treatment)

#Long and wide formats
LN_data_long <- data.frame(
  Pot = LN_data_combined$Pot,
  Duration = LN_data_combined$Duration,
  SWA_level = LN_data_combined$SWA_level,
  Intensity_level = LN_data_combined$Intensity_level,
  Leaf_number_difference = c(LN_data_combined$LND_treatment,
                             LN_data_combined$LND_recovery),
  Stage = rep(c("Treatment", "Recovery"),
              each = nrow(LN_data_combined))
)

LN_data_short <- LN_data_long %>%
  pivot_wider(names_from = Stage, values_from = Leaf_number_difference)

##Plotting
color_vec <- rev(palette.colors(3, "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")

#Linear plot (After treatment)
LN_data_short2 <- LN_data_short %>%
  mutate(Duration = as.numeric(as.character(Duration)))

LN_linear <- ggplot(LN_data_short2,
                    aes(x = Duration, y = Treatment, color = Intensity_level)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_jitter(alpha = 0.5, width = 0.1) +
  scale_color_manual(values = color_vec) +
  scale_x_continuous(breaks = c(0, 3, 6, 10)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30), limits = c(-5, 35)) +
  labs(y = "Leaf number growth") +
  facet_grid(SWA_level ~ "Treatment") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),
    legend.position = "none",
    strip.text.x = element_text(size = 12),
    strip.background.y = element_blank(),
    strip.text.y = element_blank()
  )

#Boxplot (After recovery)
LN_data_short$Duration <- factor(LN_data_short$Duration)

LN_boxplot <- ggplot(LN_data_short, 
                     aes(x = Duration, y = Recovery, color = Intensity_level)) +
  geom_boxplot() +
  geom_jitter(position = position_dodge(0.75), alpha = 0.5) +
  scale_color_manual(values = color_vec) +
  scale_x_discrete(breaks = c(3, 6, 10)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30), limits = c(-5, 35)) +
  labs(color = "Heatwave intensity") +
  facet_grid(SWA_level ~ "Recovery") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    strip.text = element_text(size = 12)
  )

#Combine linear plot and boxplot
x_label <- textGrob("Heatwave Duration (day)", gp = gpar(fontsize = 12))

combined <- arrangeGrob(LN_linear, LN_boxplot, ncol = 2)

final_plot <- arrangeGrob(combined,
                          x_label,
                          ncol = 1,
                          heights = unit.c(unit(1, "null"),
                                           unit(1.5, "lines")))
ggsave("Leaf_number_difference.png", plot = final_plot, width = 9, height = 6.5)

