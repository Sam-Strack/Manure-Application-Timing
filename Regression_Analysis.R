#Script for doing statistical analysis on manure timing experimental data.
#This will focus on performing regression to determine the relationship
#between NDVI/NDRE reflectance from the crop and yield.
#Written By: Sam Strack :)
#Last Updated: 04/03/2026

library(ggpubr)

#Reflectance & yield data
ref_yield_list <- list(RROC_Grain = tibble(Type = Growing_Season_2025$Rosemount_R6$Manure_Source,
                                           NDRE = Growing_Season_2025$Rosemount_NDVI_NDRE$Avg_NDRE_HRow,
                                           NDVI = Growing_Season_2025$Rosemount_NDVI_NDRE$Avg_NDVI_HRow,
                                           Yield = Growing_Season_2025$Rosemount_R6$Grain_Yield_Bu.acre),
                       RROC_Silage = tibble(Type = Growing_Season_2025$Rosemount_R6$Manure_Source,
                                           NDRE = Growing_Season_2025$Rosemount_NDVI_NDRE$Avg_NDRE_HRow,
                                           NDVI = Growing_Season_2025$Rosemount_NDVI_NDRE$Avg_NDVI_HRow,
                                           Yield = Growing_Season_2025$Rosemount_Silage$Silage_Yield_65Percent_Moisture_Ton.acre),
                       SROC_Grain = tibble(Type = Growing_Season_2025$Waseca_R6$Manure_Source,
                                           NDRE = Growing_Season_2025$Waseca_NVDI_NDRE$Avg_NDRE_HRow,
                                           NDVI = Growing_Season_2025$Waseca_NVDI_NDRE$Avg_NDVI_HRow,
                                           Yield = Growing_Season_2025$Waseca_R6$Grain_Yield_Bu.acre))


#loop for site, harvest types
for (yield in 1 : length(ref_yield_list)) {

  ref_yield <- ref_yield_list[[yield]]

  if (yield == 1 | yield == 2) {

    site = 1

  } else {

    site = 2

  }

  #loop for ref index
  for (VI in c("NDRE","NDVI")) {

    #run regression on each combo of yield and VI
    lm_VI_yield <- lm(Yield ~ ref_yield[[VI]], ref_yield )

    print(paste0(Trt_index$Sites[[site]]," ", Trt_index$Y_labels[[yield]],"~",VI," Regresssion"))
    print(summary(lm_VI_yield))

    #make plot
    reg_plot <- ggplot(ref_yield,
                   aes(x = .data[[VI]],
                       y = Yield)) +
                       geom_point() +
                       geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
                       stat_regline_equation(label.y = max(ref_yield[["Yield"]])) +                      #equation of line
                       stat_cor(aes(label = after_stat(rr.label)), label.y = max(ref_yield[["Yield"]]) * 0.97) + #R^2
                       ggtitle(paste0(Trt_index$Sites[[site]]," ",Trt_index$Titles[[yield]],"~",VI," 08/26/2025")) +
                       xlab(paste0(VI)) +
                       ylab(paste0(Trt_index$Y_labels[[yield]])) +
                       xlim(c(0,1)) +
                       theme_bw() +
                       theme(panel.grid.major.x = element_blank(),
                             panel.grid.minor.x = element_blank()) +
                       theme(axis.text = element_text(size = 10)) +
                       theme(legend.text = element_text(size = 14)) +
                       theme(axis.title = element_text(size = 16)) +
                       theme(plot.title = element_text(size = 18))

    #save plots
    setwd("G:/Shared drives/ManureLabTeam/ManureResearch_Shared/Experiments/Timing/Data/r_Analysis/Yield_VI_Regression")
    ggsave(paste0(Trt_index$Sites[[site]]," ",Trt_index$Titles[[yield]]," ",VI, " Regression_08_26_2025.png"),reg_plot, scale = 1)


  }

}
