#Leaf temperature variation

#Load libraries
library(gamlss)
library(dplyr)
library(MASS)
library(glmmTMB)
library(DHARMa)
library(readr)
library(gt)
library(effectsize)

#Import data

all_data <- read_csv("Datasheet_measurement_day_onesheet.csv")
all_data$Intensity_level <-
  factor(all_data$Intensity_level,
         levels = c("Control", "Mild", "Intense"))
Temp_data <-
  read.csv("Temp_variance_day_3_expand.csv") #data collected on all the plants on Day 3
Temp_data <- Temp_data[,-which(names(Temp_data) == "X")]

#Convert variable as necessary
Temp_data$HW_intensity <-
  factor(Temp_data$HW_intensity, levels = c("Control", "Mild", "Intense"))
hist(Temp_data$Leaf_temperature)
Temp_data$HW_intensity <- as.factor(Temp_data$HW_intensity)
Temp_data$HW_duration <- as.factor(Temp_data$HW_duration)
Temp_data$SWA <- as.factor(Temp_data$SWA)
Temp_data$Plant_id <- as.factor(Temp_data$Plant_id)

#Define the model
hist(log10(log10(Temp_data$Leaf_temperature)))
Temp_dlnorm <- glmmTMB(
  log10(log10(Leaf_temperature)) ~
    HW_intensity * SWA + (1 | Plant_id / Leaf_id),
  dispformula = ~ HW_intensity * SWA,
  data = Temp_data,
  family = gaussian(link = "identity")
) #indicating that you're fitting a model where the response variable follows a Gaussian (normal) distribution, and the link function used to relate the linear predictor to the mean of the response is the identity function (no need for any transformation). This is suitable for modeling continuous response variables when you expect a linear relationship between the predictor variables and the response.
plot(simulateResiduals(Temp_dlnorm)) #simulateResiduals() is used to simulate the residuals from the more complex models.

summary_model <- summary(Temp_dlnorm)
coef_summary <- coef(summary_model)

##Without double log()### DO WE NEED THIS?

hist(Temp_data$Leaf_temperature)  ## SHOULD WE REMOVE THE DOUBLE LOG10?
Temp_dlnorm_rm <- glmmTMB(
  Leaf_temperature ~
    HW_intensity * SWA + (1 | Plant_id / Leaf_id),
  dispformula = ~ HW_intensity * SWA,
  data = Temp_data,
  family = gaussian(link = "identity")
) #indicating that you're fitting a model where the response variable follows a Gaussian (normal) distribution, and the link function used to relate the linear predictor to the mean of the response is the identity function (no need for any transformation). This is suitable for modeling continuous response variables when you expect a linear relationship between the predictor variables and the response.
plot(simulateResiduals(Temp_dlnorm_rm)) #simulateResiduals() is used to simulate the residuals from the more complex models.

summary_model <- summary(Temp_dlnorm_rm)
coef_summary <- coef(summary_model)

#########

hist(Temp_data$Leaf_temperature)
Temp_norm <- glmmTMB(
  (Leaf_temperature) ~
    HW_intensity * SWA + (1 | Plant_id / Leaf_id),
  dispformula = ~ HW_intensity * SWA,
  data = Temp_data,
  family = gaussian(link = "identity"))

plot(simulateResiduals(Temp_norm)) #simulateResiduals() is used to simulate the residuals from the more complex models.

summary_model <- summary(Temp_norm)
coef_summary <- coef(summary_model)
# Extract fixed effects coefficients
fixed_coefs <- coef_summary$cond %>%
  as.data.frame() |>
  gt() |>
  tab_header(title = "Fixed coefficients in glmmTMB on leaf temperature") |>
  fmt_number(decimals = 3)

write.csv(fixed_coefs, "fixed_coefs.csv")

disp_coefs <- coef_summary$disp %>%
  as.data.frame() |>
  gt() |>
  tab_header(title = "Dispersion coefficients in glmmTMB on leaf temperature variance") %>%
  fmt_number(decimals = 3)

write.csv(disp_coefs, "disp_coefs.csv")







##### Compare the variation of the data directly ######
library(dplyr)
library(car)
library(ggplot2)

# 1. Create combined group factor and compute variance per group
grouped_vars <- Temp_data %>%
  mutate(grp = interaction(HW_intensity, SWA, sep = "_")) %>%
  group_by(grp) %>%
  summarise(
    n       = n(),
    mean_LT = mean(Leaf_temperature, na.rm = TRUE),
    var_LT  = var(Leaf_temperature, na.rm = TRUE),
    sd_LT   = sd(Leaf_temperature, na.rm = TRUE)
  )

print(grouped_vars)

# 2. Levene's test for equality of variances
Temp_data2 <- Temp_data %>%
  mutate(grp = interaction(HW_intensity, SWA, sep = "_"))

levene_result <- leveneTest(Leaf_temperature ~ grp, data = Temp_data2)
print(levene_result)

# 3. Visual check: boxplot of absolute deviations
Temp_data3 <- Temp_data2 %>%
  group_by(grp) %>%
  mutate(abs_dev = abs(Leaf_temperature - median(Leaf_temperature, na.rm = TRUE))) %>%
  ungroup()

ggplot(Temp_data3, aes(x = grp, y = abs_dev)) +
  geom_boxplot() +
  labs(
    x = "Group (HW_intensity_SWA)",
    y = "Absolute deviation from median (spread)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)










###### Pairwise comparison betweem groups ######

# 1. Create a combined grouping factor
Temp_data2 <- Temp_data %>%
  mutate(grp = interaction(HW_intensity, SWA, sep = "_"))

# 2. Identify the unique group levels
groups <- levels(Temp_data2$grp)
# e.g., groups might be: "Control_Wet", "Control_Dry", "Mild_Wet", "Mild_Dry", "Intense_Wet", "Intense_Dry"

# 3. Generate all pairwise combinations of group names
pairwise_combos <- t(combn(groups, 2)) 
# This is a matrix with two columns: the first and second element of each pair

# 4. Initialize a data.frame to store results
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

# 5. Loop over each pair, run var.test(), and store the results
for (i in seq_len(nrow(pairwise_combos))) {
  g1 <- pairwise_combos[i, 1]
  g2 <- pairwise_combos[i, 2]
  
  # Subset Leaf_temperature for each group
  x1 <- Temp_data2$Leaf_temperature[Temp_data2$grp == g1]
  x2 <- Temp_data2$Leaf_temperature[Temp_data2$grp == g2]
  
  # Compute sample variances
  v1 <- var(x1, na.rm = TRUE)
  v2 <- var(x2, na.rm = TRUE)
  
  # 5a. Run an F‐test for equality of variances
  ft <- var.test(x1, x2)
  # If you prefer Fligner–Killeen (nonparametric), replace the line above with:
  # ft <- fligner.test(Leaf_temperature ~ grp, data = Temp_data2 %>% filter(grp %in% c(g1, g2)))
  
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

# 6. Inspect the assembled table
print(pairwise_results)

write.csv(pairwise_results, "pairwise_results.csv")
