#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#2025

#Datasets needed: Data/Leaf_temperature_day3.csv
#Run from the project root (open the .Rproj file).

#Load packages
library(gt)
library(car)
library(MASS)
library(tidyverse)
library(gamlss)
library(DHARMa)
library(glmmTMB)
library(effectsize)

#Import data
Temp_data <- read.csv("Data/Leaf_temperature_day3.csv")
Temp_data <- Temp_data %>%
    dplyr::mutate(across(c(HW_intensity, SWA_level, Plant_id), as.factor))


##### Table 1. glmmTMB conditional- and dispersion-model estimated effects #####----
#Main model: leaf temperature on its original scale (degrees C).
#  - Conditional-model coefficients are differences in leaf temperature (degrees C)
#    relative to the reference group (Control, Dry).
#  - Dispersion-model coefficients are on the log(SD) scale for the gaussian family:
#    exp(beta) = multiplicative change in within-leaf SD; exp(2*beta) = change in variance.
Temp_glmm <- glmmTMB(
  Leaf_temperature ~
    HW_intensity * SWA_level + (1 | Plant_id / Leaf_id),
  dispformula = ~ HW_intensity * SWA_level,
  data = Temp_data,
  family = gaussian(link = "identity")
)

plot(simulateResiduals(Temp_glmm))
summary_model <- summary(Temp_glmm)

coef_summary <- coef(summary_model)

#Sensitivity analysis only: same model on log10(log10(leaf temperature)).
#Effects have the same sign and similar magnitude (see Transformation_diagnostics.R).
Temp_data$log10_log10_Leaf_temperature <- log10(log10(Temp_data$Leaf_temperature))
Temp_glmm_loglog <- glmmTMB(
  log10_log10_Leaf_temperature ~
    HW_intensity * SWA_level + (1 | Plant_id / Leaf_id),
  dispformula = ~ HW_intensity * SWA_level,
  data = Temp_data,
  family = gaussian(link = "identity")
)
plot(simulateResiduals(Temp_glmm_loglog))


#Extract coefficients
fixed_coefs <- coef_summary$cond %>%
  as.data.frame() %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "Conditional-model coefficients: leaf temperature (°C)") %>%
  fmt_number(decimals = 3)

disp_coefs <- coef_summary$disp %>%
  as.data.frame() %>%
  mutate(SD_ratio = exp(Estimate)) %>%   #within-leaf SD relative to reference group
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "Dispersion-model coefficients: within-leaf log(SD) of leaf temperature") %>%
  fmt_number(decimals = 3)

fixed_coefs
disp_coefs



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
