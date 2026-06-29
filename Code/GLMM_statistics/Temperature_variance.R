#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2025

#Datasets needed
Leaf_temperature_day3.csv

#Load packages
library(gt)
library(car)
library(MASS)
library(readr)
library(dplyr)
library(gamlss)
library(ggplot2)
library(DHARMa)
library(glmmTMB)
library(effectsize)

#Set working directory to the folder containing code and data
setwd("~/Downloads")

#Import data
Temp_data <- subset(read.csv("Leaf_temperature_day3.csv"), select = -X)
Temp_data <- Temp_data %>%
    dplyr::mutate(across(c(HW_intensity, SWA_level, Plant_id), as.factor))


##### Table 1. glmmTMB conditional- and dispersion-model estimated effects #####----
#Model with leaf temperature log-transformed TWICE
Temp_data$log10_log10_Leaf_temperature <- log10(log10(Temp_data$Leaf_temperature))
Temp_dlnorm <- glmmTMB(
  log10_log10_Leaf_temperature ~
    HW_intensity * SWA_level + (1 | Plant_id/Leaf_id),
  dispformula = ~ HW_intensity * SWA_level,
  data = Temp_data,
  family = gaussian(link = "identity")
) 

plot(simulateResiduals(Temp_dlnorm))
summary_model <- summary(Temp_dlnorm)

coef_summary <- coef(summary_model)

#Model without log-transformation on leaf temperature (as a comparison only)
Temp_dlnorm_rm <- glmmTMB(
   Leaf_temperature ~
    HW_intensity * SWA_level + (1 | Plant_id / Leaf_id),
  dispformula = ~ HW_intensity * SWA_level,
  data = Temp_data,
  family = gaussian(link = "identity")
) 
plot(simulateResiduals(Temp_dlnorm_rm))


#Extract coefficients
fixed_coefs <- coef_summary$cond %>%
  as.data.frame() %>%
  gt() %>%
  tab_header(title = "Fixed coefficients in glmmTMB on leaf temperature") %>%
  fmt_number(decimals = 3)

disp_coefs <- coef_summary$disp %>%
  as.data.frame() %>%
  gt() %>%
  tab_header(title = "Dispersion coefficients in glmmTMB on leaf temperature variance") %>%
  fmt_number(decimals = 3)



##### Table S2. Pairwise Comparisons of Within-Leaf Temperature Variance #####----
#Create a combined grouping factor
Temp_data$HW_intensity <- factor(Temp_data$HW_intensity,levels=c("Control", "Mild", "Intense"))
Temp_data2 <- Temp_data %>%
  mutate(grp = interaction(HW_intensity, SWA_level, sep = "_"))

#Identify the unique group levels
groups <- levels(Temp_data2$grp)

#Generate all pairwise combinations of group names
pairwise_combos <- t(combn(groups, 2)) 

#Initialize a data.frame to store results
pairwise_results <- data.frame(
  group1     = character(nrow(pairwise_combos)),
  group2     = character(nrow(pairwise_combos)),
  var1       = numeric(nrow(pairwise_combos)),
  var2       = numeric(nrow(pairwise_combos)),
  ratio      = numeric(nrow(pairwise_combos)),
  p_value    = numeric(nrow(pairwise_combos)),
  test_stat  = numeric(nrow(pairwise_combos)),
  stringsAsFactors = FALSE
)

#Loop over each pair, run var.test(), and store the results
for (i in seq_len(nrow(pairwise_combos))) {
  g1 <- pairwise_combos[i, 1]
  g2 <- pairwise_combos[i, 2]
  
  # Subset Leaf_temperature for each group
  x1 <- Temp_data2$Leaf_temperature[Temp_data2$grp == g1]
  x2 <- Temp_data2$Leaf_temperature[Temp_data2$grp == g2]
  
  # Compute sample variances
  v1 <- var(x1, na.rm = TRUE)
  v2 <- var(x2, na.rm = TRUE)
  
  # Run an F‐test for equality of variances
  ft <- var.test(x1, x2)
  
  # Store results
  pairwise_results[i, ] <- list(
    group1    = g1,
    group2    = g2,
    var1      = v1,
    var2      = v2,
    ratio     = v1 / v2,                   # ratio of variances
    p_value   = ft$p.value,                # p-value of the F-test
    test_stat = ft$statistic               # F statistic
  )
}

#Inspect the assembled table
print(pairwise_results)















## NEW! 24 June 2026 Analysis table for Within-leaf temperature range (Fig 1 panel a). --> Didn't work, the model result was not reliable.

Temp_range <- read_csv("Leaf_temp_variation_summary.csv")
hist(Temp_range$temp_range)

Temp_range_dlnorm <- glmmTMB(
  Temp_range_log ~
    HW_intensity * SWA_level + (1 | Plant_id/Leaf_id),
  dispformula = ~ HW_intensity * SWA_level,
  data = Temp_range,
  family = gaussian(link = "identity")
) 

plot(simulateResiduals(Temp_range_dlnorm))
summary_model <- summary(Temp_range_dlnorm)

coef_summary <- coef(Temp_range_dlnorm)
