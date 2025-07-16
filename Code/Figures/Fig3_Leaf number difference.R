#Figure of LEAF NUMBER DIFFERENCE
library("dplyr")
library("readr")
library("grid")
library("gridExtra")

##### Load data----
all_data <- read_csv("Datasheet_measurement_day_onesheet.csv")
all_data$Intensity_level <- factor(all_data$Intensity_level, levels = c("Control", "Mild", "Intense")) 

#Calculate leaf number difference
Leaf_number_data_1 <- all_data
Leaf_number_data <- all_data %>% dplyr::select(Pot, Day, Duration, Intensity_level, SWA_level, Leaf_number)
Leaf_number_data <- Leaf_number_data[!is.na(Leaf_number_data_1$Leaf_number), ] %>% #Add leaf number difference (e.g., LN on Day 3 - LN on Day 0)
  group_by(Pot)%>%
  mutate(Leaf_number_difference = c(NA, diff(Leaf_number, lag = 1)))
Leaf_number_data <- Leaf_number_data %>% filter(Day <11) %>% 
  dplyr::select(Duration, SWA_level, Intensity_level, Leaf_number_difference)
Leaf_number_data <- Leaf_number_data[!is.na(Leaf_number_data$Leaf_number_difference), ]

#Dataframes prepared for doing the plots
LN_data_treatment <- Leaf_number_data
names(LN_data_treatment)[names(LN_data_treatment) == "Leaf_number_difference"] <- "LND_treatment"
LN_data_recovery <- Leaf_number_data_1 %>% filter(Day >11) %>% dplyr::select(Pot, Leaf_number_difference)
names(LN_data_recovery)[names(LN_data_recovery) == "Leaf_number_difference"] <- "LND_recovery"
LN_data_recovery$LND_recovery_original= LN_data_recovery$LND_recovery + LN_data_treatment$LND_treatment
LN_data_combined <- left_join(LN_data_treatment,LN_data_recovery, by="Pot")
hist(log(Leaf_number_data$Leaf_number_difference+10))
LN_data_long <- data.frame(Pot=LN_data_combined$Pot,
                           Duration=LN_data_combined$Duration,
                           SWA_level=LN_data_combined$SWA_level,
                           Intensity_level=LN_data_combined$Intensity_level,
                           Leaf_number_difference=c(LN_data_combined$LND_treatment, LN_data_combined$LND_recovery),
                           Stage = rep(c("Treatment", "Recovery"), each = length(LN_data_combined$LND_treatment)))
LN_data_long2 <- data.frame(Pot=LN_data_combined$Pot,
                            Duration=LN_data_combined$Duration,
                            SWA_level=LN_data_combined$SWA_level,
                            Intensity_level=LN_data_combined$Intensity_level,
                            Leaf_number_difference=c(LN_data_combined$LND_treatment, LN_data_combined$LND_recovery, LN_data_combined$LND_recovery_original),
                            Stage = rep(c("Treatment", "Recovery", "Recovery_original"), each = length(LN_data_combined$LND_treatment)))

LN_data_short <-LN_data_long %>% pivot_wider(names_from = Stage, values_from = Leaf_number_difference)

##### Plot----
color_vec <- rev(palette.colors(n = 3, palette = "Set 1"))
names(color_vec) <- c("Control", "Mild", "Intense")

#Boxplot
LN_data_short$Duration <- as.factor(LN_data_short$Duration)
LN_boxplot <- ggplot(data = LN_data_short, 
                     aes(x = Duration, y = Recovery, color = Intensity_level)) +
  geom_boxplot()+
  geom_jitter(position = position_dodge(width = 0.75), alpha = 0.5)+
  scale_color_manual(values = color_vec)+   #color-blindness-friendly
  labs(color = "Heatwave intensity")+      # Change text of legend
  #caption = "Fig.3 The change of leaf number after treatment and recovery"
  scale_x_discrete(breaks = c(3,6,10))+
  scale_y_continuous(breaks=c(0,10,20,30), limits=c(-5,35))+
  theme_bw()+
  facet_grid(SWA_level~"Recovery")+
  theme(axis.text=element_text(size=12),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text = element_text(size = 12))

#Linear plot
LN_data_short2<- LN_data_short
class(LN_data_short2$Duration)
LN_data_short2$Duration <- as.numeric(as.character(LN_data_short2$Duration))

LN_linear <- ggplot(data = LN_data_short2, 
                    aes(x=Duration, y=Treatment, color=Intensity_level)) +
  geom_smooth(formula = y~x, method="lm", se=F)+
  geom_jitter(alpha = 0.5, width = 0.1)+
  scale_color_manual(values = color_vec)+    #color-blindness-friendly
  scale_x_continuous(breaks = c(0,3,6,10))+
  scale_y_continuous(breaks=c(0,10,20,30), limits=c(-5,35))+
  labs(y= "Leaf number difference")+
  theme_bw()+
  facet_grid(SWA_level~"Treatment")+
  theme(axis.text=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=14),
        legend.position = "none",
        strip.text.x = element_text(size = 12),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())

#Two figures aboce combined in one
x_shared_label <- "Heatwave Duration (day)"
x_axis_grob <- textGrob(x_shared_label, gp = gpar(fontsize = 14), x = 0.5, hjust = 0.5)
#caption_LN <- textGrob("Fig.2 Leaf number differences during treatment and recovery periods", x=0.3, hjust=0.5, vjust = 0, gp = gpar(fontface = "italic"))
#combined_text <- arrangeGrob(x_axis_grob, caption_LN, ncol = 1, heights = unit.c(unit(1, "null"), unit(1, "null")))
ggsave("Leaf_number_difference.png", plot = arrangeGrob(LN_linear, LN_boxplot, ncol = 2, bottom = x_axis_grob), width = 9, height = 6) 
