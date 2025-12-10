#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2025

#Dataset needed
Main_data.csv
Yellow_Green_data.csv

#Load packages
library(tidyverse)
library(lme4)
library(gridExtra)
library(MASS)
library(gt)
library(glmnet)
library(emmeans)
library(effectsize)
library(performance)
library(sjPlot)

#Set working directory to the folder containing code and data
setwd("~/Downloads")

#Import data
all_data <- 
  read_csv("Main_data.csv")
all_data$Intensity_level <-
  factor(all_data$Intensity_level,
         levels = c("Control", "Mild", "Intense"))


### LEAF NUMBER DIFFERENCE-----
## First Time of Measurement: After treatment
# 1.Prepare Data (calculate 'leaf number difference')
Leaf_number_data <- all_data %>%
  dplyr::select(Pot, Day, Duration, Intensity_level, SWA_level, Leaf_number) %>%
  filter(!is.na(Leaf_number)) %>%
  group_by(Pot) %>%
  mutate(Leaf_number_difference = c(NA, diff(Leaf_number))) %>%
  ungroup() %>%
  filter(Day < 11) %>%
  dplyr::select(Duration, SWA_level, Intensity_level, Leaf_number_difference) %>%
  filter(!is.na(Leaf_number_difference))

# 2.Step-wise Model Selection
LN_lm <- lm(log(Leaf_number_data$Leaf_number_difference+10) ~ Duration * Intensity_level * SWA_level, data = Leaf_number_data)
step <- stepAIC(LN_lm, trace = 0, direction="backward")
step$anova
LN_lm_best <- lm(log(Leaf_number_data$Leaf_number_difference+10) ~ Duration * Intensity_level + 
                   Duration * SWA_level, data = Leaf_number_data) #The best model

# 3.Check Models + Anova
check_model(LN_lm_best)
anova(LN_lm_best)
summary(LN_lm_best)


# 4.Effect Size (Proportion of variance explained)
eta_squared(LN_lm_best)%>% 
  data.frame() %>% 
  gt() %>%
  fmt_number(decimals = 3) #Put the partial effect size of each parameters into a table


## Second/Last Time of Measurement: After recovery
# 1.Prepare Data
LN_recovery_data <- all_data %>% filter(Day >11)

# 2.Step-wise Model Selection
LN_recovery_lm <- lm(LN_recovery_data$Leaf_number_difference ~ Duration * Intensity_level * SWA_level, data = LN_recovery_data)
step <- stepAIC(LN_recovery_lm, trace = 0, direction="backward")
step$anova
LN_recovery_best <- lm(LN_recovery_data$Leaf_number_difference ~ 1) #The best model

# 3.Check Models + Anova
check_model(LN_recovery_best)
anova(LN_recovery_best)
summary(LN_recovery_best)

# 4.No need to test eta_squared(), as there is no factor in the model.



### PHOTOSYNTHESIS HEALTH----
## First Time of Measurement: After treatment
# 1.Prepare Data
Fv_Fm_data <- all_data %>% filter(Day <11)
Fv_Fm_data <- Fv_Fm_data[!is.na(Fv_Fm_data$Fv_Fm), ]

# 2.Step-wise Model Selection
Fv_Fm_lm <- lm(Fv_Fm_difference ~ Duration * Intensity_level * SWA_level, data = Fv_Fm_data)
step <- stepAIC(Fv_Fm_lm, trace = 0, direction="backward")
step$anova
Fv_Fm_best <- lm(Fv_Fm_difference ~ Duration + Intensity_level, data = Fv_Fm_data) #The best model

# 3.Check Models + Anova
check_model(Fv_Fm_best)
anova(Fv_Fm_best)
summary(Fv_Fm_best)

# 4.Effect Size (Proportion of variance explained)
eta_squared(Fv_Fm_best)%>% 
  data.frame() %>% 
  gt() %>%
  fmt_number(decimals = 3)


## Second/Last Time of Measurement: After recovery
# 1.Prepare Data
Fv_fm_recovery_data <- all_data %>% filter(Day >11)
Fv_fm_recovery_data <- Fv_fm_recovery_data[!is.na(Fv_fm_recovery_data$Fv_Fm), ]

# 2.Step-wise Model selection
Fv_Fm_recovery_lm <- lm(Fv_Fm_difference ~ Duration * Intensity_level * SWA_level, data = Fv_fm_recovery_data)
step <- stepAIC(Fv_Fm_recovery_lm, trace = 0, direction="backward")
step$anova
Fv_Fm_recovery_best <- lm(Fv_Fm_difference ~ Duration*Intensity_level, data=Fv_fm_recovery_data)

# 3.Check Models + Anova
check_model(Fv_Fm_recovery_best)
anova(Fv_Fm_recovery_best)
summary(Fv_Fm_recovery_best)


# 4.Effect Size (Proportion of variance explained)
eta_squared(Fv_Fm_recovery_best)%>% 
  data.frame() %>% 
  gt() %>%
  fmt_number(decimals = 3)



### LEAF WATER POTENTIAL----
## Only Time of Measurement: After treatment
# 1.Prepare Data
LWP_data <- all_data %>% filter(Day <11)
LWP_data <- LWP_data[!is.na(LWP_data$Leaf_water_potential), ]

# 2.Step-wise Model selection
LWP_lm <- lm(Leaf_water_potential ~ Duration * Intensity_level * SWA_level, data = LWP_data) #Duration * SWA_level +Intensity_level * SWA_level
step <- stepAIC(LWP_lm, trace = 0, direction="backward")
step$anova
LWP_lm_best <- lm(Leaf_water_potential ~ Duration * Intensity_level * SWA_level, data = LWP_data)

# 3.Check Models + Anova
check_model(LWP_lm_best)
anova(LWP_lm_best)
summary(LWP_lm_best) 

# 4.Effect size
eta_squared(LWP_lm_best)%>% 
  data.frame() %>% 
  gt() %>%
  fmt_number(decimals = 3)



### PLANT HEIGHT GROWTH----
## First Time of Measurement: After treatment
# 1.Prepare Data
Height_difference_data <- all_data %>% filter (Day < 11)
Height_difference_data <- Height_difference_data[!is.na(Height_difference_data$Height_difference),]

# 2.Step-wise model selection
HD_lm <- lm(Height_difference ~ Duration * Intensity_level * SWA_level, data = Height_difference_data)
step <- stepAIC(HD_lm, trace = 0, direction="backward")
step$anova
HD_lm_best <- lm(Height_difference ~ Duration*Intensity_level + Intensity_level*SWA_level, data = Height_difference_data) #The best model

# 3.Check Models + Anova
check_model(HD_lm_best)
anova(HD_lm_best)
summary(HD_lm_best)

# 4.Effect size
eta_squared(HD_lm_best)%>% 
  data.frame() %>% 
  gt() %>%
  fmt_number(decimals = 3)


## Second/Last Time of Measurement: After recovery
# 1.Prepare Data
HD_recovery_data <- all_data %>% filter(Day > 11)
HD_recovery_data <- HD_recovery_data[!is.na(HD_recovery_data$Height_difference),]

# 2.Step-wise model selection
HD_lm_recovery <- lm(Height_difference ~ Duration * Intensity_level * SWA_level, data = HD_recovery_data)
step <- stepAIC(HD_lm_recovery, trace = 0, direction="backward")
step$anova 
HD_recovery_lm_best <- lm(Height_difference ~ Intensity_level*SWA_level, data = HD_recovery_data) 

# 3.Check Models + Anova
check_model(HD_recovery_lm_best)
anova(HD_recovery_lm_best)
summary(HD_recovery_lm_best)

# 4.Effect size
eta_squared(HD_recovery_lm_best)%>% 
  data.frame() %>% 
  gt() %>%
  fmt_number(decimals = 3)



### YELLOW: GREEN LEAF BIOMASS RATIO----
# 1.Import Data
Yellow_Green_data <- read.csv("Yellow_Green_data.csv")

# 2.Step-wise Model Selection (Due to the data distribution, we used 'glm' rather than 'lm')
YG_glm <- glm(Dry_yellow_green_leaf_mass ~ Duration * Intensity_level * SWA_level, data = Yellow_Green_data,family=binomial())
step <- stepAIC(YG_glm, trace = 0, direction="backward")
step$anova 
YG_glm_best <- glm(Dry_yellow_green_leaf_mass ~ 1, data = Yellow_Green_data, family=binomial())

# 3. No need to test eta_squared(), as there is no factor in the model



###Create Summary Table
tab_model(
  Fv_Fm_best,
  LWP_lm_best,
  HD_lm_best,
  LN_lm_best,
  show.se = FALSE, 
  show.stat = FALSE,
  dv.labels = c("Photosynthetic health (Fv/Fm) difference", 
                "Leaf water potential", 
                "Plant height difference", 
                "Leaf number difference"),
  file = "summary_table_treatment.doc")

tab_model(
  Fv_Fm_recovery_best,
  HD_recovery_lm_best,
  show.se = FALSE, 
  show.stat = FALSE,
  dv.labels = c("Photosynthetic health (Fv/Fm) difference after recovery", 
                "Plant height difference after recovery"),
  file = "summary_table_recovery.doc")
