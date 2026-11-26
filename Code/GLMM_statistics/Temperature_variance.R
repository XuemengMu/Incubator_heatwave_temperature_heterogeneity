# Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
# 2026

# Datasets needed
# Leaf_temperature_day3.csv
# Leaf_temperature_day10.csv

# Load packages
library(gt)
library(car)
library(MASS)
library(tidyverse)
library(gamlss)
library(DHARMa)
library(glmmTMB)
library(effectsize)
library(moments)

setwd("~/Downloads")

##### Table 1. glmmTMB conditional- and dispersion-model estimated effects of within-leaf temperatures on Day 3 #####----
# Import data
Temp_data <- read.csv("Leaf_temperature_day3.csv") %>%
  dplyr::mutate(across(c(HW_intensity, SWA_level, Plant_id), as.factor))

# Primary model: single log10-transformation on leaf temperature
m_log1 <- glmmTMB(
  log10(Leaf_temperature) ~ HW_intensity * SWA_level + (1 | Plant_id / Leaf_id),
  dispformula = ~ HW_intensity * SWA_level,
  data = Temp_data,
  family = gaussian(link = "identity")
)

plot(simulateResiduals(m_log1))
summary_log1 <- summary(m_log1)
coef_log1    <- coef(summary_log1)

# Extract coefficients
fixed_coefs <- coef_log1$cond %>%
  as.data.frame() %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "Conditional-model coefficients: leaf temperature (°C)") %>%
  fmt_number(decimals = 3)

disp_coefs <- coef_log1$disp %>%
  as.data.frame() %>%
  mutate(SD_ratio = exp(Estimate)) %>% # within-leaf SD relative to reference group
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "Dispersion-model coefficients: within-leaf temperature variation") %>%
  fmt_number(decimals = 3)

fixed_coefs
disp_coefs

# Model outcome comparisons
# 1. Untransformed model
m_raw <- glmmTMB(
  Leaf_temperature ~ HW_intensity * SWA_level + (1 | Plant_id / Leaf_id),
  dispformula = ~ HW_intensity * SWA_level,
  data = Temp_data,
  family = gaussian(link = "identity")
)

plot(simulateResiduals(m_raw))
summary_raw <- summary(m_raw)
coef_raw    <- coef(summary_raw)

fixed_coefs_raw <- coef_raw$cond %>%
  as.data.frame() %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "Conditional-model coefficients: untransformed leaf temperature (°C)") %>%
  fmt_number(decimals = 3)

disp_coefs_raw <- coef_raw$disp %>%
  as.data.frame() %>%
  mutate(SD_ratio = exp(Estimate)) %>% # within-leaf SD relative to reference group
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "Dispersion-model coefficients: within-leaf temperature variation (untransformed)") %>%
  fmt_number(decimals = 3)

fixed_coefs_raw
disp_coefs_raw

# 2. Double log10-transformed model
m_log2 <- glmmTMB(
  log10(log10(Leaf_temperature)) ~ HW_intensity * SWA_level + (1 | Plant_id / Leaf_id),
  dispformula = ~ HW_intensity * SWA_level,
  data = Temp_data,
  family = gaussian(link = "identity")
)

plot(simulateResiduals(m_log2))
summary_log2 <- summary(m_log2)
coef_log2    <- coef(summary_log2)

fixed_coefs_log2 <- coef_log2$cond %>%
  as.data.frame() %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "Conditional-model coefficients: double-log10 leaf temperature (°C)") %>%
  fmt_number(decimals = 3)

disp_coefs_log2 <- coef_log2$disp %>%
  as.data.frame() %>%
  mutate(SD_ratio = exp(Estimate)) %>% # within-leaf SD relative to reference group
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "Dispersion-model coefficients: within-leaf temperature variation (double-log10)") %>%
  fmt_number(decimals = 3)

fixed_coefs_log2
disp_coefs_log2

##### Table A1. Diagnostic comparison of skewness, residual diagnostics, and model fit across candidate leaf temperature transformations #####----
# Extract response vectors and standardized (Pearson) residuals
T_raw  <- Temp_data$Leaf_temperature
T_log1 <- log10(T_raw)
T_log2 <- log10(log10(T_raw))

res_raw  <- residuals(m_raw,  type = "pearson")
res_log1 <- residuals(m_log1, type = "pearson")
res_log2 <- residuals(m_log2, type = "pearson")

# Compute Jacobian-corrected AIC on the °C scale
aic_raw  <- AIC(m_raw)
aic_log1 <- AIC(m_log1) - 2 * sum(log(1 / (T_raw * log(10))))
aic_log2 <- AIC(m_log2) - 2 * sum(log(1 / (T_raw * log10(T_raw) * (log(10)^2))))

aic_vec   <- c(aic_raw, aic_log1, aic_log2)
delta_aic <- aic_vec - min(aic_vec)

# Assemble summary table
comparison_table <- data.frame(
  Transformation = c("Untransformed", "log10", "log10(log10)"),
  Skew_raw_data  = round(c(skewness(T_raw), skewness(T_log1), skewness(T_log2)), 2),
  Residual_skew  = round(c(skewness(res_raw), skewness(res_log1), skewness(res_log2)), 2),
  Residual_excess_kurtosis = round(c(
    kurtosis(res_raw)  - 3,
    kurtosis(res_log1) - 3,
    kurtosis(res_log2) - 3
  ), 2),
  Delta_AIC_C = round(delta_aic, 0)
)

print(comparison_table)

##### Table A2. Pairwise Comparisons of Within-Leaf Temperature Variance #####----
# Create a combined grouping factor
Temp_data_grp <- Temp_data %>%
  mutate(
    HW_intensity = factor(HW_intensity, levels = c("Control", "Mild", "Intense")),
    grp = interaction(HW_intensity, SWA_level, sep = "_")
  )

# Identify unique group levels and generate all pairwise combinations
groups          <- levels(Temp_data_grp$grp)
pairwise_combos <- t(combn(groups, 2))

# Initialize data frame to store results
pairwise_results <- data.frame(
  group1    = character(nrow(pairwise_combos)),
  group2    = character(nrow(pairwise_combos)),
  var1      = numeric(nrow(pairwise_combos)),
  var2      = numeric(nrow(pairwise_combos)),
  ratio     = numeric(nrow(pairwise_combos)),
  p_value   = numeric(nrow(pairwise_combos)),
  test_stat = numeric(nrow(pairwise_combos)),
  stringsAsFactors = FALSE
)

# Loop over each pair, run var.test(), and store results
for (i in seq_len(nrow(pairwise_combos))) {
  g1 <- pairwise_combos[i, 1]
  g2 <- pairwise_combos[i, 2]
  
  x1 <- Temp_data_grp$Leaf_temperature[Temp_data_grp$grp == g1]
  x2 <- Temp_data_grp$Leaf_temperature[Temp_data_grp$grp == g2]
  
  v1 <- var(x1, na.rm = TRUE)
  v2 <- var(x2, na.rm = TRUE)
  ft <- var.test(x1, x2)
  
  pairwise_results[i, ] <- list(
    group1    = g1,
    group2    = g2,
    var1      = v1,
    var2      = v2,
    ratio     = v1 / v2,     # ratio of variances
    p_value   = ft$p.value,  # p-value of the F-test
    test_stat = ft$statistic # F statistic
  )
}

# Adjust p-values for multiple pairwise comparisons
pairwise_results$p_adj <- p.adjust(pairwise_results$p_value, method = "holm")

print(pairwise_results)


##### Table A3. glmmTMB conditional- and dispersion-model estimated effects of within-leaf temperatures on Day 10 #####----
# Import data
Temp_data_day10 <- read.csv("Leaf_temperature_day10.csv") %>%
  dplyr::mutate(across(c(HW_intensity, SWA_level, Plant_id), as.factor))

# Model with log10-transformation on leaf temperature
m_log1_day10 <- glmmTMB(
  log10(Leaf_temperature) ~ HW_intensity + (1 | Plant_id / Leaf_id),
  dispformula = ~ HW_intensity,
  data = Temp_data_day10,
  family = gaussian(link = "identity")
)

plot(simulateResiduals(m_log1_day10))
summary_log1_day10 <- summary(m_log1_day10)
coef_log1_day10    <- coef(summary_log1_day10)

# Extract coefficients
fixed_coefs_day10 <- coef_log1_day10$cond %>%
  as.data.frame() %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "Fixed coefficients in glmmTMB on leaf temperature") %>%
  fmt_number(decimals = 3)

disp_coefs_day10 <- coef_log1_day10$disp %>%
  as.data.frame() %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(title = "Dispersion coefficients in glmmTMB on leaf temperature variance") %>%
  fmt_number(decimals = 3)

fixed_coefs_day10
disp_coefs_day10
