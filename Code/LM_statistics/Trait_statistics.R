##### LOAD THE DATASHEET FOR EACH MODEL BELOW----
all_data <- read_csv("Datasheet_measurement_day_onesheet.csv")
all_data$Intensity_level <- factor(all_data$Intensity_level, levels = c("Control", "Mild", "Intense")) 

###LEAF NUMBER DIFFERENCE-----
## Library Packages
library("dplyr")
library("readr")
library(lme4)
library("grid")
library(gridExtra)
library(MASS)

# 1.Prepare Data
Leaf_number_data <- all_data %>% 
  dplyr::select(Pot, Day, Duration, Intensity_level, SWA_level, Leaf_number)
Leaf_number_data <- Leaf_number_data[!is.na(Leaf_number_data_1$Leaf_number), ] %>% #Add leaf number difference (e.g., LN on Day 3 - LN on Day 0)
  group_by(Pot)%>%
  mutate(Leaf_number_difference = c(NA, diff(Leaf_number, lag = 1)))
Leaf_number_data <- Leaf_number_data %>% filter(Day <11) %>% 
  dplyr::select(Duration, SWA_level, Intensity_level, Leaf_number_difference)
Leaf_number_data <- Leaf_number_data[!is.na(Leaf_number_data$Leaf_number_difference), ]

# 2.Step-wise Model Selection
LN_lm <- lm(log(Leaf_number_data$Leaf_number_difference+10) ~ Duration * Intensity_level * SWA_level, data = Leaf_number_data)
step <- stepAIC(LN_lm, trace = 0, direction="backward")
step$anova
LN_lm_best <- lm(log(Leaf_number_data$Leaf_number_difference + 10) ~ Duration*Intensity_level + 
               Duration*SWA_level, data = Leaf_number_data)

# 3.Check Models + Anova
summary(LN_lm_best)
anova(LN_lm_best)
check_model(LN_lm_best)

# 4.Effect Size (Proportion of variance explained)
effect_size_LN




###PHOTOSYNTHESIS HEALTH----
## Load Libraries
library(dplyr)
library(readr)
library(MASS)
library(lme4)

## After treatment
# 1.Prepare Data
Fv_Fm_data <- all_data %>% filter(Day <11)
Fv_Fm_data <- Fv_Fm_data[!is.na(Fv_Fm_data$Fv_Fm), ]

# 2.Convert Data as necessary
Fv_Fm_data$Fv_Fm_difference <- as.numeric(Fv_Fm_data$Fv_Fm_difference)
Fv_Fm_data$Duration <- as.numeric(Fv_Fm_data$Duration)

# 3.Step-wise Model Selection
Fv_Fm_data$Intensity_level <- factor(Fv_Fm_data$Intensity_level, levels = c("Control", "Mild", "Intense", labels= c("Control","Mild","Intense" )))
hist((Fv_Fm_data$Fv_Fm_difference))
step <- stepAIC(FvFm_1, trace = 0, direction="backward")
step$anova
FvFm_best <- lm(Fv_Fm_difference ~ Duration + Intensity_level, data = Fv_Fm_data)

# 4.Check Models + Anova
check_model(FvFm_best)
summary(FvFm_best)
anova(FvFm_best)

# 5.Effect Size (Proportion of variance explained)
eta_squared(FvFm_best)


## After recovery

# 1.Prepare Data
Fvfm_recovery_data <- all_data %>% filter(Day >11)
Fvfm_recovery_data <- Fvfm_recovery_data[!is.na(Fvfm_recovery_data$Fv_Fm), ]

# 2.Convert Data as necessary
Fvfm_recovery_data$Fv_Fm_difference <- as.numeric(Fvfm_recovery_data$Fv_Fm_difference)
Fvfm_recovery_data$Duration <- as.numeric(Fvfm_recovery_data$Duration)
hist((Fvfm_recovery_data$Fv_Fm_difference))

# 3.Step-wise Model selection
FvFm_recovery_lm <- lm(Fv_Fm_difference ~ Duration * Intensity_level * SWA_level, data = Fvfm_recovery_data)
step <- stepAIC(FvFm_recovery_lm, trace = 0, direction="backward")
step$anova
FvFm_recovery_best <- lm(Fv_Fm_difference ~ Duration + Intensity_level + Duration:Intensity_level, data=Fvfm_recovery_data)

# 4.Check Models + Anova
check_model(FvFm_recovery_best)
anova(FvFm_recovery_best)
summary(FvFm_recovery_best)

# 5.Effect Size (Proportion of variance explained)
eta_squared(FvFm_recovery_best)




###LEAF WATER POTENTIAL----
## Load required package
library("lme4")
library("dplyr")
library("readr")
library("MASS")

##After treatment
# 1.Prepare Data
LWP_data <- all_data %>% filter(Day <11)
LWP_data <- LWP_data[!is.na(LWP_data$Leaf_water_potential), ]

# 2.Convert Data as necessary
LWP_data$Leaf_water_potential <- as.numeric(LWP_data$Leaf_water_potential)
LWP_data$Duration <- as.numeric(LWP_data$Duration)
hist(LWP_data$Leaf_water_potential)

# 3.Step-wise Model selection
LWP_lm <- lm(Leaf_water_potential ~ Duration * Intensity_level * SWA_level, data = LWP_data) #Duration * SWA_level +Intensity_level * SWA_level
step <- stepAIC(LWP_lm1, trace = 0, direction="backward")
step$anova
LWP_lm_best <- lm(Leaf_water_potential ~ Duration * Intensity_level * SWA_level, data = LWP_data)

# 4.Check Models + Anova
summary(LWP_lm1) %>% 
  coefficients()%>%
  data.frame()
anova(LWP_lm_best)
check_model(LWP_lm_best)

# 5.Effect size
eta_squared(LWP_lm3)


###HEIGHT GROWTH----
## Load libraries
library(dplyr)
library(readr)
library(MASS)
library(lme4)
library(glmnet)
library(gt)
library(emmeans)
library(effectsize)

## After treatment
# 1.Prepare Data
Height_difference_data <- all_data %>% filter (Day < 11)
Height_difference_data <- Height_difference_data[!is.na(Height_difference_data$Height_difference),]
hist(Height_difference_data$Height_difference)

# 2.Step-wise model selection
HD_lm <- lm(Height_difference ~ Duration * Intensity_level * SWA_level, data = Height_difference_data)
step <- stepAIC(HD_lm, trace = 0, direction="backward")
step$anova # The best is HD_lm3
HD_lm_best <- lm(Height_difference ~ Duration + Intensity_level + SWA_level + Duration:Intensity_level + Intensity_level:SWA_level, data = Height_difference_data)

# 3.Check Models + Anova
check_model(HD_lm_best)
summary(HD_lm_best)
anova(HD_lm_best)

# 4.Effect size
eta_squared(HD_lm_best)


## After recovery
# 1.Prepare Data
HD_recovery_data <- all_data %>% filter (Day > 11)
HD_recovery_data <- HD_recovery_data[!is.na(HD_recovery_data$Height_difference),]
HD_recovery_data$Intensity_level <- factor(HD_recovery_data$Intensity_level, levels = c("Control", "Mild", "Intense")) 
hist(HD_recovery_data$Height_difference)

# 2. Step-wise model selection
HD_lm_recovery <- lm(Height_difference ~ Duration * Intensity_level * SWA_level, data = HD_recovery_data)
step <- stepAIC(HD_lm_recovery, trace = 0, direction="backward")
step$anova 
HD_recovery_lm_best <- lm(Height_difference ~ Intensity_level + SWA_level + Intensity_level:SWA_level, data = HD_recovery_data) 

# 3.Check Models + Anova
summary(HD_recovery_lm_best)
check_model(HD_recovery_lm_best)
anova(HD_recovery_lm_best)

# 4.Effect size
eta_squared(HD_recovery_lm_best)




###YELLOW: GREEN LEAF BIOMASS RATIO----
## Load packages
library(MASS)
library(readr)
library(performance)
library(tidyverse)

# 1.Prepare Data
Yellow_Green_data <- read.csv("Yellow_Green_data.csv")
hist(Yellow_Green_data$Dry_yellow_green_leaf_mass) 

# 2.Step-wise Model Selection (Due to the data distribution, we used 'glm' rather than 'lm')
YG_glm <- glm(Dry_yellow_green_leaf_mass ~ Duration * Intensity_level * SWA_level, data = Yellow_Green_data,family=binomial())
step <- stepAIC(YG_glm1, trace = 0, direction="backward")
step$anova 
YG_glm_best <- glm(Dry_yellow_green_leaf_mass ~ 1, data = Yellow_Green_data, family=binomial())

# 3. No need to test eta_squared(), as there is no factor in the model.