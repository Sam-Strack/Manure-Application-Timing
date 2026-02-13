#Script for making figures from the 2025 manuretiming growing season
#Written By: Sam Strack :)
#Last Updated: 10/18/25

#load necessary packages
library(tidyverse)
library(scales)
library(dplyr)
library(ggplot2)


site
Trt_Index <- data.frame(Groups = c("Liquid","Liquid","Liquid","Liquid","Solid","Solid","Solid","Control","Urea","Urea","Urea","Urea"),
                        Arrange = c(8,11,12,9,10,3,1,2,4,7,5,6),
                        Arrange_SoilN = c(8,1,2,4,3,5,6,7,10,11,12,9),
                        Timing = c("Oct","Nov","April","June","Oct","Nov","April","Control","Urea","Urea","Urea","Urea"))

#------------------------------------------------------------------------------------#

#calculate avg grain yield, arrange by treatment, and add treatment groups
Avg_Grain_Yield_Treatment <- Growing_Season_2025$Waseca_R6 %>%
  group_by(Trt) %>%
  summarise_at(vars(Grain_Yield_Bu.acre),list(Avg_Grain_Yield = mean,StdDev_Grain_Yield = sd)) %>%
  mutate(Treat_Num = Trt_Index$Arrange) %>%
  arrange(Treat_Num) %>%
  mutate(Trt = fct_inorder(Trt),
         Group = Trt_Index$Groups,
         Timing = Trt_Index$Timing)

#Error bars, +/- 1 Standard Deviation
limits <- aes(ymin = Avg_Grain_Yield - StdDev_Grain_Yield,
              ymax = Avg_Grain_Yield + StdDev_Grain_Yield)

#Plot Avg Grain Yield by Treatment
Avg_Grain_Yield_Treatment_Plot <- ggplot(Avg_Grain_Yield_Treatment,
                                         aes(fill = Group,
                                            x = Trt,
                                            y = Avg_Grain_Yield)) +
                                            geom_col(color = "black") +
                                            geom_errorbar(limits, width = 0.2) +
                                            scale_fill_manual(values = c("Liquid" = "#ffcc33", "Solid" = "#7a0019", "Control" = "#141414", "Urea" = "#A9A9A9" )) +
                                            ggtitle("SROC Manure Timing Grain Yield by Treatment") +
                                            ylab("Grain Yield(Bu/Acre)") +
                                            xlab("Treatment")

plot(Avg_Grain_Yield_Treatment_Plot)

#------------------------------------------------------------------------------------#

#calculate avg Silage yield, arrange by treatment, and add treatment groups
Avg_Silage_Yield_Treatment <- Growing_Season_2025$Rosemount_Silage %>%
  group_by(Trt) %>%
  summarise_at(vars(Silage_Yield_65Percent_Moisture_Ton.acre),list(Avg_Wet_Yield = mean, StdDev_Wet_Yield = sd)) %>%
  mutate(Treat_Num = Trt_Index$Arrange)

%>%
  arrange(Treat_Num) %>%
  mutate(Trt = fct_inorder(Trt),
         Group = Trt_Index$Groups,
         Timing = Trt_Index$Timing)

#Error bars, +/- 1 Standard Deviation
limits <- aes(ymin = Avg_Wet_Yield - StdDev_Wet_Yield,
              ymax = Avg_Wet_Yield + StdDev_Wet_Yield)

#Plot Avg Grain Yield by Treatment
Avg_Silage_Yield_Treatment_Plot <- ggplot(Avg_Silage_Yield_Treatment,
                                         aes(fill = Group,
                                            x = Trt,
                                            y = Avg_Wet_Yield)) +
                                            geom_col(color = "black") +
                                            geom_errorbar(limits, width = 0.2) +
                                            scale_fill_manual(values = c("Liquid" = "#ffcc33", "Solid" = "#7a0019", "Control" = "#141414", "Urea" = "#A9A9A9" )) +
                                            ggtitle("RROC Manure Timing Silage Yield by Treatment") +
                                            ylab("Silage Yield(65%Moisture)(Ton/Acre)") +
                                            xlab("Treatment")


plot(Avg_Silage_Yield_Treatment_Plot)


#------------------------------------------------------------------------------------#

#Calculate Avg Soil N for each sample timing, arrange by treatment, add treatment groups

Avg_Soil_N_Timing_Treatment <- Growing_Season_2025$`Waseca_In-Season_Soil` %>%
  group_by(Treatment) %>%
  summarize(Avg_Soil_NO3_0_12 = mean(Soil_NO3_0_12),
            Avg_Soil_NO3_12_24 = mean(Soil_NO3_0_12),
            Avg_Soil_NH4_0_12 = mean(Soil_NH4_0_12),
            Avg_Soil_NH4_12_24 = mean(Soil_NH4_12_24),
            Sd_Soil_NO3_0_12 = sd(Soil_NO3_0_12),
            Sd_Soil_NO3_12_24 = sd(Soil_NO3_0_12),
            Sd_Soil_NH4_0_12 = sd(Soil_NH4_0_12),
            Sd_Soil_NH4_12_24 = sd(Soil_NH4_12_24)) %>%
            mutate(Treat_Num = Trt_Index$Arrange_SoilN) %>%
            arrange(Treat_Num) %>%
            mutate(Trt = fct_inorder(Treatment),
                  Group = Trt_Index$Groups,
                  Timing = Trt_Index$Timing)

Avg_Soil_0_12 <- data.frame(a <- Avg_Soil_N_Timing_Treatment$Avg_Soil_NO3_0_12 )


#Error bars, +/- 1 Standard Deviation
NO3_0_12_limits <- aes(ymin = Avg_Soil_N_Timing_Treatment$Avg_Soil_NO3_0_12 - Avg_Soil_N_Timing_Treatment$Sd_Soil_NO3_0_12,
                       ymax = Avg_Soil_N_Timing_Treatment$Avg_Soil_NO3_0_12 + Avg_Soil_N_Timing_Treatment$Sd_Soil_NO3_0_12)
NO3_12_24_limits <- aes(ymin = Avg_Soil_N_Timing_Treatment$Avg_Soil_NO3_12_24 - Avg_Soil_N_Timing_Treatment$Sd_Soil_NO3_12_24,
                       ymax = Avg_Soil_N_Timing_Treatment$Avg_Soil_NO3_12_24 + Avg_Soil_N_Timing_Treatment$Sd_Soil_NO3_12_24)
NH4_0_12_limits <- aes(ymin = Avg_Soil_N_Timing_Treatment$Avg_Soil_NH4_0_12 - Avg_Soil_N_Timing_Treatment$Sd_Soil_NH4_0_12,
                       ymax = Avg_Soil_N_Timing_Treatment$Avg_Soil_NH4_0_12 + Avg_Soil_N_Timing_Treatment$Sd_Soil_NH4_0_12)
NH4_12_24_limits <- aes(ymin = Avg_Soil_N_Timing_Treatment$Avg_Soil_NH4_12_24 - Avg_Soil_N_Timing_Treatment$Sd_Soil_NH4_12_24,
                       ymax = Avg_Soil_N_Timing_Treatment$Avg_Soil_NH4_12_24 + Avg_Soil_N_Timing_Treatment$Sd_Soil_NH4_12_24)


Avg_Soil_N_Timing_Treatment_Plot <- ggplot(Avg_Soil_N_Timing_Treatment,
                                          aes(fill = Group,
                                              x = Trt,
                                              y = Avg_Soil_NO3_0_12,
                                                Avg_Soil_NO3_12_24)) +
  geom_col(color = "black") +
  geom_errorbar(limits, width = 0.2) +
  scale_fill_manual(values = c("Liquid" = "#ffcc33", "Solid" = "#7a0019", "Control" = "#141414", "Urea" = "#A9A9A9" )) +
  ggtitle("RROC Manure Timing Silage Yield by Treatment") +
  ylab("Silage Yield(65%Moisture)(Ton/Acre)") +
  xlab("Treatment")






#------------------------------------------------------------------------------------#

SoilN_Yield_Treatment





PRS

