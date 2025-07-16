#Figure of Photosynthesis health(Fv/Fm)

library("dplyr")
library("readr")
library("viridis")

##### Load data----
all_data <- read_csv("Datasheet_measurement_day_onesheet.csv")
all_data$Intensity_level <- factor(all_data$Intensity_level, levels = c("Control", "Mild", "Intense")) 

#Prepare data
Fv_Fm_data <- all_data %>% filter(Day <11)
Fv_Fm_data <- Fv_Fm_data[!is.na(Fv_Fm_data$Fv_Fm), ]
Fv_Fm_data$Fv_Fm_difference <- as.numeric(Fv_Fm_data$Fv_Fm_difference)
Fv_Fm_data$Duration <- as.numeric(Fv_Fm_data$Duration)
hist((Fv_Fm_data$Fv_Fm_difference))
Fv_Fm_data$Fv_Fm<-as.numeric(Fv_Fm_data$Fv_Fm)
Fv_Fm_data$Day<-as.factor(Fv_Fm_data$Day)

##### plot----
color_vec <- rev(palette.colors(n = 3, palette = "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")

Fvfm_plot <- ggplot(aes(x=Day), data=Fv_Fm_data)+ 
  geom_violin(aes(y=Fv_Fm))+
  geom_jitter(aes(y=Fv_Fm, color = Intensity_level), size=0.2, alpha=0.7)+
  geom_boxplot(aes(y=Fv_Fm_difference, color = Intensity_level))+
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0.83, color ="Red", linetype = "dashed", size= 0.5)+
  geom_hline(yintercept = 0, color ="black", linetype = "dashed", size=0.5)+
  labs(x= "Heatwave Duration (Day)", 
       y = "Fv/Fm",
       color = "Heatwave intensity")+
  #    caption = "Fig.2 Plant photosynthetic health (Fv/Fm) and the relative changes over time")+
  scale_x_discrete(breaks=c(0,3,6,10))+
  scale_y_continuous(breaks=c(0.83, 0.8, 0.7, 0.6, 0.5, 0.4), name="Fv/Fm",
                     sec.axis=sec_axis(~ ., breaks = c(0.3, 0.2, 0.1, 0, -0.1, -0.2, -0.3),name="Fv/Fm differece (relative to Control)"))+
  scale_color_manual(values = color_vec)+
  theme_bw()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 10),
        plot.caption = element_text(size = 12, hjust = 0.5))+
  facet_wrap(~SWA_level)

Fvfm_plot
