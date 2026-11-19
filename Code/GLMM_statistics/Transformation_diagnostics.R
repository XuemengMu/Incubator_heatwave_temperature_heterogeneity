#Title: Heatwaves increase sub-leaf-scale heterogeneity in leaf temperature
#Supplementary sensitivity analysis: the Table 1 model (fitted on degrees C) is
#refitted on log10 and log10(log10) transformed leaf temperature. Transformation
#improves residual shape only marginally, and conditional and dispersion effects
#are consistent across scales, so the untransformed model is reported in Table 1.

#Dataset needed: Data/Leaf_temperature_day3.csv
#Run from the project root (open the .Rproj file).

library(tidyverse)
library(glmmTMB)
library(patchwork)

Temp_data <- read.csv("Data/Leaf_temperature_day3.csv") %>%
  mutate(HW_intensity = factor(HW_intensity, levels = c("Control", "Mild", "Intense")),
         SWA_level = factor(SWA_level, levels = c("Dry", "Wet")),
         Plant_id = factor(Plant_id))

T_raw <- Temp_data$Leaf_temperature

##### 1. Candidate transformations ####
#Each entry: transformation and log|dy/dT| (Jacobian), so that log-likelihoods
#can be compared on the original (degrees C) scale.
ln10 <- log(10)
transforms <- list(
  "Untransformed" = list(f = function(x) x,
                         logJ = function(x) rep(0, length(x))),
  "log10"         = list(f = function(x) log10(x),
                         logJ = function(x) -log(x) - log(ln10)),
  "log10(log10)"  = list(f = function(x) log10(log10(x)),
                         logJ = function(x) -log(x) - log(log10(x)) - 2 * log(ln10))
)

skew <- function(x) { x <- x - mean(x); mean(x^3) / mean(x^2)^1.5 }
exkurt <- function(x) { x <- x - mean(x); mean(x^4) / mean(x^2)^2 - 3 }

##### 2. Fit the Table 1 model under each transformation ####
fit_one <- function(tr) {
  d <- Temp_data
  d$y <- tr$f(d$Leaf_temperature)
  glmmTMB(y ~ HW_intensity * SWA_level + (1 | Plant_id / Leaf_id),
          dispformula = ~ HW_intensity * SWA_level,
          data = d, family = gaussian(link = "identity"))
}
fits <- lapply(transforms, fit_one)

##### 3. Table S_x: distribution diagnostics ####
grp <- interaction(Temp_data$HW_intensity, Temp_data$SWA_level)
diag_table <- imap_dfr(transforms, function(tr, nm) {
  y <- tr$f(T_raw)
  r <- residuals(fits[[nm]], type = "pearson")   #scaled by the modelled dispersion
  tibble(
    Transformation          = nm,
    Skew_raw_data           = skew(y),
    Skew_within_group_mean  = mean(tapply(y, grp, skew)),
    Skew_residuals          = skew(r),
    Excess_kurtosis_resid   = exkurt(r),
    #log-likelihood on the degrees C scale (Jacobian-corrected) -> comparable AIC
    AIC_degC = -2 * (as.numeric(logLik(fits[[nm]])) + sum(tr$logJ(T_raw))) +
      2 * attr(logLik(fits[[nm]]), "df")
  )
}) %>%
  mutate(Delta_AIC_degC = AIC_degC - min(AIC_degC))

print(diag_table, width = Inf)
write.csv(diag_table, "Figures/TableS_transformation_diagnostics.csv", row.names = FALSE)

##### 4. Figure S_x: residual QQ plots and density ####
set.seed(1)
idx <- sample(seq_along(T_raw), 20000)   #subsample for plotting only; statistics use all data
qq_df <- imap_dfr(fits, function(m, nm) {
  r <- residuals(m, type = "pearson")[idx]
  tibble(Transformation = nm, resid = r,
         theoretical = qnorm(ppoints(length(r)))[rank(r, ties.method = "first")])
}) %>%
  mutate(Transformation = factor(Transformation, levels = names(transforms)))

lab_df <- diag_table %>%
  mutate(Transformation = factor(Transformation, levels = names(transforms)),
         label = sprintf("skew = %.2f\nexcess kurtosis = %.2f\nΔAIC (°C scale) = %.0f",
                         Skew_residuals, Excess_kurtosis_resid, Delta_AIC_degC))

p_qq <- ggplot(qq_df, aes(theoretical, resid)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 0.3, alpha = 0.3, colour = "grey20") +
  geom_text(data = lab_df, aes(x = -4.3, y = 7.5, label = label),
            hjust = 0, vjust = 1, size = 3) +
  facet_wrap(~ Transformation) +
  coord_cartesian(xlim = c(-4.5, 4.5), ylim = c(-5, 8)) +
  labs(x = "Theoretical normal quantile", y = "Pearson residual") +
  theme_bw()

p_dens <- tibble(
  Transformation = factor(rep(names(transforms), each = length(T_raw)), levels = names(transforms)),
  value = unlist(lapply(transforms, function(tr) as.numeric(scale(tr$f(T_raw)))))
) %>%
  ggplot(aes(value)) +
  geom_density(fill = "grey70", colour = "grey20") +
  facet_wrap(~ Transformation) +
  labs(x = "Leaf temperature (standardised, transformed scale)", y = "Density") +
  theme_bw()

FigS <- p_dens / p_qq + plot_annotation(tag_levels = "a")
ggsave("Figures/FigS_transformation_diagnostics.tiff", FigS,
       width = 8, height = 6, dpi = 300, compression = "lzw")

##### 5. Back-transform Table 1 conditional effects to degrees C ####
#Predicted value for a typical leaf (random effects = 0) in each treatment group.
#Because the transformation is monotonic, back-transforming the predicted mean on the
#transformed scale gives (approximately) the MEDIAN leaf temperature, not the mean.
inv_loglog <- function(y) 10^(10^y)
newd <- expand.grid(HW_intensity = levels(Temp_data$HW_intensity),
                    SWA_level = levels(Temp_data$SWA_level),
                    Plant_id = NA, Leaf_id = NA)
pr <- predict(fits[["log10(log10)"]], newdata = newd, re.form = NA, se.fit = TRUE)
pr_raw <- predict(fits[["Untransformed"]], newdata = newd, re.form = NA)

backtrans <- newd %>%
  dplyr::select(HW_intensity, SWA_level) %>%
  mutate(Median_degC = inv_loglog(pr$fit),
         Lower95 = inv_loglog(pr$fit - 1.96 * pr$se.fit),
         Upper95 = inv_loglog(pr$fit + 1.96 * pr$se.fit),
         Untransformed_model_degC = pr_raw) %>%
  group_by(SWA_level) %>%
  mutate(Difference_vs_Control_degC = Median_degC - Median_degC[HW_intensity == "Control"]) %>%
  ungroup()

print(backtrans, width = Inf)
write.csv(backtrans, "Figures/TableS_backtransformed_effects.csv", row.names = FALSE)

##### 6. Dispersion model: robustness to the transformation ####
#glmmTMB (gaussian) models log(sigma); exp(2*coef) is a ratio of residual variances.
#Compare the sign/size of the dispersion effects with and without transformation.
disp_compare <- imap_dfr(fits, function(m, nm) {
  cf <- fixef(m)$disp
  tibble(Transformation = nm, Term = names(cf), Estimate = cf)
}) %>%
  pivot_wider(names_from = Transformation, values_from = Estimate)
print(disp_compare, width = Inf)

#Local compression of the transformation, d(log10(log10 T))/dT, at the
#Control vs Intense median temperatures: a concave transformation shrinks hotter
#temperatures more, so higher dispersion in heated leaves on this scale is conservative.
dT <- function(x) 1 / (x * log10(x) * ln10^2)
print(round(c(at_Control = dT(min(backtrans$Median_degC)),
              at_Intense = dT(max(backtrans$Median_degC))), 5))
