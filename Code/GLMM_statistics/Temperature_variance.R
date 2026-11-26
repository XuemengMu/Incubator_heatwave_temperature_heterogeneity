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
  fmt_number(decimals = 3) %>%
  tab_source_note(paste(
    "Dispersion model: log link on the residual standard deviation (sigma, degrees C),",
    "as parameterised by glmmTMB for the gaussian family; log(sigma) = X*beta.",
    "Estimates are differences in log(sigma) relative to the reference group (Control, Dry).",
    "exp(beta) = fold-change in residual SD; exp(2*beta) = fold-change in residual variance."
  ))

fixed_coefs
disp_coefs


#Back-transformed dispersion results: residual SD (degrees C) per treatment group,
#and fold-changes in SD and variance for heatwave vs control at each water level.
#Wald 95% CIs on the log(sigma) scale, then exponentiated. Pixels are treated as
#independent residuals, so these CIs are narrower than leaf-level replication supports.
b_disp <- fixef(Temp_glmm)$disp
V_disp <- vcov(Temp_glmm)$disp
grid_disp <- expand.grid(HW_intensity = levels(Temp_data$HW_intensity),
                         SWA_level    = levels(Temp_data$SWA_level))
X_disp <- model.matrix(~ HW_intensity * SWA_level, grid_disp)
eta    <- drop(X_disp %*% b_disp)
se_eta <- sqrt(diag(X_disp %*% V_disp %*% t(X_disp)))

group_sigma <- grid_disp %>%
  mutate(sigma_C = exp(eta),
         lower   = exp(eta - 1.96 * se_eta),
         upper   = exp(eta + 1.96 * se_eta))

disp_contrast <- function(hw, swa) {
  L  <- X_disp[grid_disp$HW_intensity == hw & grid_disp$SWA_level == swa, ] -
        X_disp[grid_disp$HW_intensity == "Control" & grid_disp$SWA_level == swa, ]
  est <- sum(L * b_disp)
  se  <- sqrt(drop(t(L) %*% V_disp %*% L))
  tibble(contrast  = paste0(hw, " vs Control (", swa, ")"),
         log_SD    = est,
         SD_ratio  = exp(est),     SD_lower  = exp(est - 1.96 * se),     SD_upper  = exp(est + 1.96 * se),
         var_ratio = exp(2 * est), var_lower = exp(2 * (est - 1.96 * se)), var_upper = exp(2 * (est + 1.96 * se)))
}
disp_contrasts <- bind_rows(disp_contrast("Mild", "Dry"),    disp_contrast("Intense", "Dry"),
                            disp_contrast("Mild", "Wet"),    disp_contrast("Intense", "Wet"))

group_sigma %>%
  gt() %>%
  tab_header(title = "Model-estimated within-leaf residual SD of leaf temperature (°C)") %>%
  fmt_number(decimals = 2)

disp_contrasts %>%
  gt() %>%
  tab_header(title = "Heatwave effects on within-leaf temperature dispersion, back-transformed") %>%
  fmt_number(decimals = 2)



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
