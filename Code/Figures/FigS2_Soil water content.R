#Figure of Soil Water Content

##### Load data----
SWC_data <- read_csv("~/Desktop/UNSW work relevant/Chapter 2 (indoor heatwave experiment)/Data analysis/R_script/R_scripts_in_use/3. Models_R/Soil_water_condition_R/SWC_data.csv")
SWC_data$Intensity_level <- factor(SWC_data$Intensity_level, levels = c("Control", "Mild", "Intense"))

##### Plot----
color_vec <- rev(palette.colors(n = 3, palette = "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")

SWC_Plot <- 
  ggplot(data=SWC_data, 
         aes(x= SWC_level, y= SWC, fill=Intensity_level))+
  geom_boxplot()+
  scale_fill_manual(values = color_vec)+
  scale_y_continuous(limits=c(0.1,0.5), breaks= c(0.1, 0.2, 0.3, 0.4, 0.5))+
  labs(x=NULL, y="Soil water content", fill = "Intensity level")+
  theme_bw()

# T.test & Calculation ----
SWC_Dry <- filter(Combined_SWC, SWC_level=="Dry") %>% na.omit()
SWC_Wet <- filter(Combined_SWC, SWC_level=="Wet") %>% na.omit()
SWC_statistics <- t.test(SWC~SWC_level, data=Combined_SWC)
SWC_statistics <- t.test(SWC~SWC_level, data=Combined_SWC)
SWC_statistics2 <- lm(SWC~SWC_level, data=Combined_SWC)
summary(SWC_statistics2)

hist(Control_dry_SWC_longer$SWC)
hist(Control_wet_SWC_longer$SWC)
hist(Intense_wet_SWC_longer$SWC)
hist(Intense_dry_SWC_longer$SWC)
hist(Mild_wet_SWC_longer$SWC)
hist(Mild_dry_SWC_longer$SWC)
qqnorm(Control_dry_SWC_longer$SWC)

SWC_combined_means <- SWC_data %>%
  group_by(SWC_level) %>%
  summarise(avg = mean(SWC), SD= sd(SWC))

