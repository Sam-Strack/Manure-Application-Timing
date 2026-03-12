#Script for doing statistical analysis on manure timing experimental data. This
#will focus on performing regression to create an N response curve featuring
#5 N-Rates(0,50%,75%,100%,150%) from two sites- each under a different cropping system(C-C, SB-C)
#Written By: Sam Strack :)
#Last Updated: 02/26/2026

#N-response curve:
#https://blogs.cornell.edu/agsci-interns/2015/08/20/the-models-used-to-fit-the-data-of-yield-nitrogen-fertilizer-response/
#https://cropsandsoils.extension.wisc.edu/articles/what-you-can-and-cant-learn-from-a-nitrogen-response-curve/
  #2 sites -> 5 treatments
  #fit line to yield x rate data
    #need to rewrite treatments as numeric rates(not str)
    #note R^2
    #calculate AONR/EONR
      #know price per lb N
      #know price per unit manure
      #know price per bu corn
      #price ratio lbN/buCorn
  #extra info to include
    #preplant N result(ppm) and associated credit
    #missing/messed up treatments


#load in necessary packages
library(ggplot2)
library(ggpubr)

#load in yield/silage data, remove manure treatments
waseca_yield_raw <- tibble(Plot = Growing_Season_2025$Waseca_R6$Plot,
                           Trt = Growing_Season_2025$Waseca_R6$Fert_N_Rate_lb.acre,
                           Manure_Source = Growing_Season_2025$Waseca_R6$Manure_Source,
                           Yield = Growing_Season_2025$Waseca_R6$Grain_Yield_Bu.acre)
waseca_yield <- waseca_yield_raw[grepl('None', waseca_yield_raw$Manure_Source), ]


rosemount_yield_raw <- tibble(Plot = Growing_Season_2025$Rosemount_R6$Plot,
                           Trt = Growing_Season_2025$Rosemount_R6$Trt,
                           Manure_Source = Growing_Season_2025$Rosemount_R6$Manure_Source,
                           Yield = Growing_Season_2025$Rosemount_R6$Grain_Yield_Bu.acre)
rosemount_yield <- rosemount_yield_raw[grepl('None', rosemount_yield_raw$Manure_Source), ]

rosemount_silage_raw <- tibble(Plot = Growing_Season_2025$Rosemount_Silage$Plot,
                              Trt = Growing_Season_2025$Rosemount_Silage$Trt,
                              Manure_Source = Growing_Season_2025$Rosemount_Silage$Manure_Source,
                              Silage = Growing_Season_2025$Rosemount_Silage$Silage_Yield_65Percent_Moisture_Ton.acre)
rosemount_silage <- rosemount_silage_raw[grepl('None', rosemount_silage_raw$Manure_Source), ]
rosemount_silage <- rosemount_silage[!grepl(c('107'), rosemount_silage$Plot), ]
rosemount_silage <- rosemount_silage[!grepl(c('211'), rosemount_silage$Plot), ]
rosemount_silage <- rosemount_silage[!grepl(c('301'), rosemount_silage$Plot), ]
rosemount_silage <- rosemount_silage[!grepl(c('406'), rosemount_silage$Plot), ]


#make array for ordering x-axis
figure_arrange <- c("Control (0 N)", "Fertilizer 50%", "Fertilizer 75%", "Fertilizer 100%", "Fertilizer 150%" )

#set factor levels in dataframes
waseca_yield$Trt <- factor(waseca_yield$Trt)# , levels = figure_arrange)
rosemount_yield$Trt <- factor(rosemount_yield$Trt, levels = figure_arrange)
rosemount_silage$Trt <- factor(rosemount_silage$Trt, levels = figure_arrange)

#run regression to get lines for plots -> yield is a function of N applied
waseca_line <- lm(Yield ~ Trt, data = waseca_yield)


#plots:
#waseca grain
ggplot(data = waseca_yield,
       aes(x = Trt,
           y = Yield,
           group = 1)) +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) + #linear regression, standard error
       stat_regline_equation(label.y = 250) +   #equation of line
       stat_cor(aes(label = after_stat(rr.label)), label.y = 240) + #R^2
       ggtitle("SROC Yield-N Response Curve Soybean-Corn") +
       xlab("N Rate(Pre-Plant Urea)") +
       ylab("Yield Bu/Acre") +
       scale_x_discrete(labels = c("0lbsN","75lbsN","112lbsN","150lbsN","225lbsN"))

#rosemount grain
ggplot(data = rosemount_yield,
       aes(x = Trt,
           y = Yield,
           group = 1)) +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       stat_regl
       ggtitle("RROC Yield-N Response Curve Corn-Corn") +
       xlab("N Rate(Pre-Plant Urea)") +
       ylab("Yield Bu/Acre") +
       scale_x_discrete(labels = c("0lbsN","98lbsN","146lbsN","195lbsN","293lbsN"))

#rosemount silage
ggplot(data = rosemount_silage,
       aes(x = Trt,
           y = Silage)) +
       geom_point() +
       ggtitle("RROC Silage Yield-N Response Curve Corn-Corn") +
       xlab("N Rate(Pre-Plant Urea)") +
       ylab("Silage Yield(65% Moist) Ton/Acre") +
       scale_x_discrete(labels = c("0lbsN","98lbsN","146lbsN","195lbsN","293lbsN"))

