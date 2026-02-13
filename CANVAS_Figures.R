#Script for making figures from the 2025 manuretiming growing season
#Written By: Sam Strack :)
#Last Updated: 10/27/25

#load necessary packages
library(tidyverse)
library(scales)
library(dplyr)
library(ggplot2)
library(ggtext)

Trt_Index <- data.frame(Groups = c("Liquid","Liquid","Liquid","Liquid","Solid","Solid","Solid","Control","Urea","Urea","Urea","Urea"),
                        Groups_RROC = c("Liquid","Control","Liquid","Liquid","Solid","Solid","Solid","Control","Urea","Urea","Urea","Urea"),
                        Arrange = c(8,11,9,10,1,2,4,3,5,6,7,12),
                        Trt_Num = c(1,2,3,4,5,6,7,8,9,10,11,12),
                        Timing = c("Oct","Nov","April","June","Oct","Nov","April","Control","Urea","Urea","Urea","Urea"),
                        Names_RROC = c("Liquid Dairy Early Fall","Liquid Dairy Late Fall","Liquid Dairy Spring","Liquid Dairy Sidedress","Solid Beef Early Fall","Solid Beef Late Fall",
                                       "Solid Beef Spring","Control","Urea 98lbsN","Urea 146lbsN","Urea 195lbsN","Urea 293lbsN"),
                        Names_SROC = c("Liquid Swine Early Fall","Liquid Swine Late Fall","Liquid Swine Spring","Liquid Swine Sidedress","Solid Turkey Early Fall","Solid Turkey Late Fall",
                                        "Solid Turkey Spring","Control","Urea 75lbsN","Urea 112lbsN","Urea 150lbsN","Urea 225lbsN"))


#------------------------------------------------------------------------------------#

#calculate avg grain yield(SROC), arrange by treatment, and add treatment groups
Avg_Grain_Yield_Treatment_SROC <- Growing_Season_2025$Waseca_R6 %>%
  group_by(Trt) %>%
  summarise_at(vars(Grain_Yield_Bu.acre),list(Avg_Grain_Yield = mean,StdDev_Grain_Yield = sd)) %>%
  mutate(Treat_Num = Trt_Index$Arrange) %>%
  arrange(Treat_Num) %>%
  mutate(Trt = fct_inorder(Trt),
         Group = Trt_Index$Groups,
         Timing = Trt_Index$Timing)

#TEMP FOR CANVAS
#Avg_Grain_Yield_Treatment_SROC <- Avg_Grain_Yield_Treatment_SROC[c(1,2,4,3,5,6,7,8,12), ]


#Error bars, +/- 1 Standard Deviation
limits <- aes(ymin = Avg_Grain_Yield - StdDev_Grain_Yield,
              ymax = Avg_Grain_Yield + StdDev_Grain_Yield)

#Plot Avg Grain Yield by Treatment
Avg_Grain_Yield_Treatment_Plot <- ggplot(Avg_Grain_Yield_Treatment_SROC,
                                         aes(fill = Group,
                                            x = Trt,
                                            y = Avg_Grain_Yield)) +
                                            geom_col(color = "black") +
                                            geom_errorbar(limits, width = 0.2) +
                                            scale_fill_manual(values = c("Liquid" = "#ffcc33", "Solid" = "#7a0019", "Control" = "#141414", "Urea" = "#A9A9A9" )) +
                                            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
                                            ggtitle("Corn-Soybean Grain Yield by Treatment") +
                                            ylab("Grain Yield(Bu/Acre)") +
                                            xlab("Treatment") +
                                            theme_bw() +
                                            theme(
                                              panel.grid.major.x = element_blank(),
                                              panel.grid.minor.x = element_blank()) +
                                            theme(axis.text = element_text(size = 14)) +
                                            theme(legend.text = element_text(size = 14)) +
                                            theme(axis.title = element_text(size = 16)) +
                                            theme(plot.title = element_text(size = 18))

plot(Avg_Grain_Yield_Treatment_Plot)

#------------------------------------------------------------------------------------#

#calculate avg grain yield(RROC), arrange by treatment, and add treatment groups
Avg_Grain_Yield_Treatment_RROC <- Growing_Season_2025$Rosemount_R6 %>%
  group_by(Trt) %>%
  summarise_at(vars(Grain_Yield_Bu.acre),list(Avg_Grain_Yield = mean,StdDev_Grain_Yield = sd)) %>%
  mutate(Treat_Num = Trt_Index$Arrange) %>%
  arrange(Treat_Num) %>%
  mutate(Trt = fct_inorder(Trt),
         Group = Trt_Index$Groups_RROC,
         Timing = Trt_Index$Timing)

#TEMP FOR CANVAS
Avg_Grain_Yield_Treatment_RROC <- Avg_Grain_Yield_Treatment_RROC[c(1,4,3,5,6,7,8,12), ]

#Error bars, +/- 1 Standard Deviation
limits <- aes(ymin = Avg_Grain_Yield - StdDev_Grain_Yield,
              ymax = Avg_Grain_Yield + StdDev_Grain_Yield)

#Plot Avg Grain Yield by Treatment
Avg_Grain_Yield_Treatment_Plot <- ggplot(Avg_Grain_Yield_Treatment_RROC,
                                         aes(fill = Group,
                                             x = Trt,
                                             y = Avg_Grain_Yield)) +
                                             geom_col(color = "black") +
                                             geom_errorbar(limits, width = 0.2) +
                                             scale_fill_manual(values = c("Liquid" = "#ffcc33", "Solid" = "#7a0019", "Control" = "#141414", "Urea" = "#A9A9A9" )) +
                                             scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
                                             ggtitle("Continuous Corn Grain Yield by Treatment") +
                                             ylab("Grain Yield(Bu/Acre)") +
                                             xlab("Treatment") +
                                             theme_bw() +
                                             theme(
                                               panel.grid.major.x = element_blank(),
                                               panel.grid.minor.x = element_blank()) +
                                             theme(axis.text = element_text(size = 14)) +
                                             theme(legend.text = element_text(size = 14)) +
                                             theme(axis.title = element_text(size = 16)) +
                                             theme(plot.title = element_text(size = 18))

plot(Avg_Grain_Yield_Treatment_Plot)

#------------------------------------------------------------------------------------#

#calculate avg Silage yield, arrange by treatment, and add treatment groups
Avg_Silage_Yield_Treatment <- Growing_Season_2025$Rosemount_Silage %>%
  group_by(Trt) %>%
  summarise_at(vars(Silage_Yield_65Percent_Moisture_Ton.acre),list(Avg_Wet_Yield = mean, StdDev_Wet_Yield = sd)) %>%
  mutate(Treat_Num = Trt_Index$Arrange) %>%
  arrange(Treat_Num) %>%
  mutate(Trt = fct_inorder(Trt),
         Group = Trt_Index$Groups,
         Timing = Trt_Index$Timing)

#TEMP FOR CANVAS
Avg_Silage_Yield_Treatment <- Avg_Silage_Yield_Treatment[c(1,3,4,5,6,7,8,12), ]

#Error bars, +/- 1 Standard Deviation
limits <- aes(ymin = Avg_Wet_Yield - StdDev_Wet_Yield,
              ymax = Avg_Wet_Yield + StdDev_Wet_Yield)

#Plot Avg Silage Yield by Treatment
Avg_Silage_Yield_Treatment_Plot <- ggplot(Avg_Silage_Yield_Treatment,
                                         aes(fill = Group,
                                            x = Trt,
                                            y = Avg_Wet_Yield)) +
                                            geom_col(color = "black") +
                                            geom_errorbar(limits, width = 0.2) +
                                            scale_fill_manual(values = c("Liquid" = "#ffcc33", "Solid" = "#7a0019", "Control" = "#141414", "Urea" = "#A9A9A9" )) +
                                            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
                                            ggtitle("Continuous Corn Silage Yield by Treatment") +
                                            ylab("Silage Yield(65%Moisture)(Ton/Acre)") +
                                            xlab("Treatment") +
                                            theme_bw() +
                                            theme(
                                              panel.grid.major.x = element_blank(),
                                              panel.grid.minor.x = element_blank()) +
                                            theme(axis.text = element_text(size = 14)) +
                                            theme(legend.text = element_text(size = 14)) +
                                            theme(axis.title = element_text(size = 16)) +
                                            theme(plot.title = element_text(size = 18))




plot(Avg_Silage_Yield_Treatment_Plot)


#------------------------------------------------------------------------------------#


SoilN_Treatment






#------------------------------------------------------------------------------------#

SoilN_Yield_Treatment




#------------------------------------------------------------------------------------#

#want plots for NO3, NH4, P, K, S, Ca, Na
#line graph showing change in uptake over time(include soil temp/moisture?)
#bar graph showing cumulative Uptake over season

RROC_PRS_Arrange <- Trt_Index[c(1,3,4,5,6,7,8,9,11), ]


#get avg/sd of uptake rate for each treatment at for burial(starting w/ no3)
for (x in RROC_PRS_Arrange$Trt_Num) {

  #select plot dataframes by treatment column
  PRS_Treatment <-map(Growing_Season_2025$Rosemount_PRS_Data, ~.x[.x$Treatment == Trt_Index$Names_RROC[x],]) %>%
    keep( ~nrow(.) > 0)

  PRS_Treatment <- bind_rows(PRS_Treatment)

  Avg_PRS_Treatment <- PRS_Treatment %>%
  group_by(Burial) %>%
  summarize(Avg_NO3_Uptake = mean(as.numeric(`NO3-N`), na.rm = TRUE),
            Avg_NH4_Uptake = mean(as.numeric(`NH4-N`), na.rm = TRUE),
            Avg_P_Uptake = mean(as.numeric(`P`), na.rm = TRUE),
            Avg_K_Uptake = mean(as.numeric(`K`), na.rm = TRUE),
            Avg_S_Uptake = mean(as.numeric(`S`), na.rm = TRUE),
            Avg_Ca_Uptake = mean(as.numeric(`Ca`), na.rm = TRUE),
            Avg_Na_Uptake = mean(as.numeric(`Na`), na.rm = TRUE),
            Sd_NO3_Uptake = sd(as.numeric(`NO3-N`), na.rm = TRUE),
            Sd_NH4_Uptake = sd(as.numeric(`NH4-N`), na.rm = TRUE),
            Sd_P_Uptake = sd(as.numeric(`P`), na.rm = TRUE),
            Sd_K_Uptake = sd(as.numeric(`K`), na.rm = TRUE),
            Sd_S_Uptake = sd(as.numeric(`S`), na.rm = TRUE),
            Sd_Ca_Uptake = sd(as.numeric(`Ca`), na.rm = TRUE),
            Sd_Na_Uptake = sd(as.numeric(`Na`), na.rm = TRUE)) %>%
            mutate(Treatment = Trt_Index$Names_RROC[x],
                    Group = Trt_Index$Groups[x],
                    Timing = Trt_Index$Timing[x])

  if (x == 1) { #initialize dataframe

    Rosemount_PRS_Plot_Data <- Avg_PRS_Treatment

  } else {  #append to end of dataframe

    Rosemount_PRS_Plot_Data <- rbind(Rosemount_PRS_Plot_Data, Avg_PRS_Treatment)

  }
}


#add burial dates in excel
write_csv(Rosemount_PRS_Plot_Data, "Rosemount_PRS.csv")
Rosemount_PRS_Plot_Data_Cleaned <- read.csv("G:/Shared drives/ManureLabTeam/ManureResearch_Shared/Experiments/Timing/Data/Raw_Data/PRS/Rosemount_PRS_Cleaned.csv")



#Error bars, +/- 1 Standard Deviation
limits <- aes(ymin = Avg_NO3_Uptake - Sd_NO3_Uptake,
              ymax = Avg_NO3_Uptake + Sd_NO3_Uptake)

Rosemount_PRS_Plot <- ggplot(Rosemount_PRS_Plot_Data_Cleaned,
                         aes(group = Treatment,
                             x = Retrieval_Date,
                             y = Avg_NO3_Uptake,
                             color = Treatment)) +
                             geom_line(linewidth = 1) +
                             geom_point(aes(shape = Group, size = 2))+
                             guides(size = "none") +
                             scale_color_brewer(palette = "Paired") +
                             labs(x = "Retrieval Date",
                                  y = expression(Nitrate~Uptake~(ug~'/10cm'^2~'/14days'))) +
                             ggtitle("Continuous Corn Nitrate Uptake") +
                             theme_bw() +
                             theme(panel.grid.major.x = element_blank(),
                                   panel.grid.minor.x = element_blank()) +
                             theme(axis.text = element_text(size = 14)) +
                             theme(legend.text = element_text(size = 14)) +
                             theme(axis.title = element_text(size = 16)) +
                             theme(plot.title = element_text(size = 18))


plot(Rosemount_PRS_Plot)

#------------------------------------------------------------------------------------#

SROC_PRS_Arrange <- Trt_Index[c(1,2,3,4,5,6,7,8,9,11), ]


#get avg/sd of uptake rate for each treatment at for burial(starting w/ no3)
for (x in SROC_PRS_Arrange$Trt_Num) {

  #select plot dataframes by treatment column
  PRS_Treatment <-map(Growing_Season_2025$Waseca_PRS_Data, ~.x[.x$Treatment == Trt_Index$Names_SROC[x],]) %>%
    keep( ~nrow(.) > 0)

  PRS_Treatment <- bind_rows(PRS_Treatment)

  Avg_PRS_Treatment <- PRS_Treatment %>%
    group_by(Burial) %>%
    summarize(Avg_NO3_Uptake = mean(as.numeric(`NO3-N`), na.rm = TRUE),
              Avg_NH4_Uptake = mean(as.numeric(`NH4-N`), na.rm = TRUE),
              Avg_P_Uptake = mean(as.numeric(`P`), na.rm = TRUE),
              Avg_K_Uptake = mean(as.numeric(`K`), na.rm = TRUE),
              Avg_S_Uptake = mean(as.numeric(`S`), na.rm = TRUE),
              Avg_Ca_Uptake = mean(as.numeric(`Ca`), na.rm = TRUE),
              Avg_Na_Uptake = mean(as.numeric(`Na`), na.rm = TRUE),
              Sd_NO3_Uptake = sd(as.numeric(`NO3-N`), na.rm = TRUE),
              Sd_NH4_Uptake = sd(as.numeric(`NH4-N`), na.rm = TRUE),
              Sd_P_Uptake = sd(as.numeric(`P`), na.rm = TRUE),
              Sd_K_Uptake = sd(as.numeric(`K`), na.rm = TRUE),
              Sd_S_Uptake = sd(as.numeric(`S`), na.rm = TRUE),
              Sd_Ca_Uptake = sd(as.numeric(`Ca`), na.rm = TRUE),
              Sd_Na_Uptake = sd(as.numeric(`Na`), na.rm = TRUE)) %>%
              mutate(Treatment = Trt_Index$Names_SROC[x],
              Group = Trt_Index$Groups[x],
              Timing = Trt_Index$Timing[x])

  if (x == 1) { #initialize dataframe

    Waseca_PRS_Plot_Data <- Avg_PRS_Treatment

  } else {  #append to end of dataframe

    Waseca_PRS_Plot_Data <- rbind(Waseca_PRS_Plot_Data, Avg_PRS_Treatment)

  }
}

write_csv(Waseca_PRS_Plot_Data, "Waseca_PRS.csv")

Waseca_PRS_Plot_Data_Cleaned <- read.csv("G:/Shared drives/ManureLabTeam/ManureResearch_Shared/Experiments/Timing/Data/Raw_Data/PRS/Waseca_PRS_Cleaned.csv")

Waseca_PRS_Pot <- ggplot(Waseca_PRS_Plot_Data_Cleaned,
                         aes(group = Treatment,
                             x = Retrieval.Date,
                             y = Avg_NO3_Uptake,
                             color = Treatment)) +
                             geom_line(linewidth = 1) +
                             geom_point(aes(shape = Group, size = 2)) +
                             guides(size = "none") +
                             scale_color_brewer(palette = "Paired")

plot(Waseca_PRS_Pot)
