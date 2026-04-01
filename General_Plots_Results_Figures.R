#Script for making plots for preliminary analysis of Manure Application timing experimental data.
#Each section will include a section to:
  #Re-arrange data for use in specific plots
  #generate plots for various data sources
  #save plots to shared drive

#Plots to make:
  #Yield by treatment
  #cumulative and daily precip over growing season(double y axis bar graph)
  #PRS trends over time(line graph) for each analyte
  #Soil N throughout the season splot by treatment type(solid,liquid, urea)

#written by: Sam Strack :)
#last edited: 03/17/2025

install.packages("reshape")
install.packages("gridExtra")
library(reshape)
library(gridExtra)
library(ggplot2)
library(tidyverse)

#Daily and Cumulative Precip
#------------------------------------------------------------------------------------#
#make list of yearly precip files
precip <- list(rosemount_precip_2024 = Growing_Season_2025$Rosemount_Weather_Daily_2024,
               waseca_precip_2024 = Growing_Season_2025$Waseca_Weather_Daily_2024,
               rosemount_precip_2025 = Growing_Season_2025$Rosemount_Weather_Daily_2025,
               waseca_precip_2025 = Growing_Season_2025$Waseca_Weather_Daily_2025)

#sites list and year list
sites_list <- list("RROC",
                "SROC")
year_list <- list("2024",
                  "2025")
retrieval_dates <- as.Date(c("2025-05-29", "2025-06-12", "2025-06-27", "2025-07-10"))


#set desired range of dates for precip graphs, set to "yr" to make plot for whole year
#Format: (yyyy-mm-dd)
#date_range <- c("2025-05-29",
#                "2025-07-10")
date_range <- "yr"

#loop for year
for (yr in 1 : as.numeric(length(year_list))) {

  #whole year
  precip_year <-precip[str_subset(names(precip),year_list[[yr]])]

  #loop for site
  for (sites in 1 : as.numeric(length(precip_year))) {

    #if date range is contained within year, make plot for that chunk. Make plot for whole year otherwise
    if (date_range[[1]] == "yr") {

      dates <- c(precip_year[[sites]][["Date"]][[1]],
                      precip_year[[sites]][["Date"]][[length(precip_year[[sites]][["Date"]])]])

    }  else if (date_range[[1]] != "yr" & substr(date_range[[1]], 1, 4) != year_list[[yr]] ) {

      dates <- c(precip_year[[sites]][["Date"]][[1]],
                 precip_year[[sites]][["Date"]][[length(precip_year[[sites]][["Date"]])]])

    } else if (date_range[[1]] != "yr" & substr(date_range[[1]], 1, 4) == year_list[[yr]] ) {

      dates <- date_range

    }


    #select data by date range
    precip_plot = precip_year[[sites]] %>%
      filter(Date >= as.Date(dates[[1]]) &
             Date <= as.Date(dates[[2]]))

    print(paste0("Precip plotted from ",dates[[1]]," to ", dates[[2]]))


    #make plots
    Daily <- ggplot(precip_plot,
                    aes(x = Date,
                        y = as.numeric(Precip_Daily))) +
                        geom_col(width =  1.5, color = "blue") +
                        ylab("Daily Precipitation (in)") +
                        scale_x_date(
                          date_breaks = "1 month", # Sets a major tick mark every month
                          date_labels = "%b") +    # Formats labels to show abbreviated month and full year (e.g., "Jan 2020"))
                        ylim(0,6.5) +
                        theme_bw()

    Cumulative <- ggplot(precip_plot,
                         aes(x = Date,
                              y = Precip_Cum)) +
                         geom_line(linewidth = 1.25, color = "blue") +
                         ylab("Cumulative Precipitation (in)") +
                         scale_x_date(
                           date_breaks = "1 month", # Sets a major tick mark every month
                           date_labels = "%b") +    # Formats labels to show abbreviated month and full year (e.g., "Jan 2020"))
                        ggtitle(paste0(sites_list[[sites]]," ",year_list[[yr]]," Precipitation")) +
                         theme_bw()

    #make stacked plot
    stacked_precip <- grid.arrange(Cumulative,Daily)

    #save plots
    setwd("G:/Shared drives/ManureLabTeam/ManureResearch_Shared/Experiments/Timing/Data/r_Plots/Precipitation")
    ggsave(paste0(sites_list[[sites]],"_",year_list[[yr]],"_Precipitation.png"),stacked_precip, scale = 2.15)

  }

}

#------------------------------------------------------------------------------------#


#PRS Graphs
#------------------------------------------------------------------------------------#
#make long datable for plotting
PRS_list <-list(Growing_Season_2025$Rosemount_PRS_Data, Growing_Season_2025$Waseca_PRS_Data)
sites_list <- c("RROC","SROC")
column_names <- c("Plot", "Treatment","Date","B1", "B2", "B3","B4")
burials <- c("2025-05-15","2025-05-29","2025-06-12","2025-06-27")
nutr <- c("NO3-N", "NH4-N", "Ca", "Mg", "K", "P", "Fe", "Mn", "Cu", "Zn", "B", "S", "Pb", "Al", "Cd","Na")

#loop for each site
for (site in 1 : as.numeric(length(sites_list))) {

  avg_PRS_long <- data.frame()

  PRS <- PRS_list[[site]]

  #loop for each analyte
  for (analyte in c("NO3-N", "NH4-N", "Ca", "Mg", "K", "P", "Fe", "Mn", "Cu", "Zn", "B", "S", "Pb", "Al", "Cd","Na")) {

    #loop for each plot. Initialize dataframe for first plot, then append the rest of the plots. Try wrapper eliminates the plots with no data
    for (plot_num in 1 : as.numeric(length(PRS))) {

      if (plot_num == 1)  {
        analyte_frame <- tibble(Plot = substr(PRS[[plot_num]][["Sample ID"]][[1]], 5,8 ),
                                Treatment = PRS[[plot_num]][["Treatment"]][[1]],
                                Date = PRS[[plot_num]][["Burial Date"]][[1]]) %>%
                                bind_cols(as_tibble(t(as.numeric(PRS[[plot_num]][[analyte]]))))

      } else {

          try(analyte_frame <- add_row(analyte_frame, (tibble(Plot = substr( PRS[[plot_num]][["Sample ID"]][[1]], 5,8 ),
                                                               Treatment = PRS[[plot_num]][["Treatment"]][[1]],
                                                               Date = PRS[[plot_num]][["Burial Date"]][[1]]) %>%
                                                               bind_cols(as_tibble(t(as.numeric(PRS[[plot_num]][[analyte]])))))),
              silent = TRUE)

      }

    }

    #assign names to columns
    names(analyte_frame) <- column_names

    #calculate averages from each treatment, convert data to long.
    analyte_frame  <- analyte_frame %>%
      group_by(Treatment) %>%
      #summarise_at(vars("B1", "B2", "B3","B4"), list(mean)) %>%
      pivot_longer(cols = c("B1", "B2", "B3","B4"),
                   names_to = "Burial",
                   values_to = paste0(analyte))

    if (analyte == "NO3-N") {

      avg_PRS_long <- analyte_frame

    } else {

    avg_PRS_long <- avg_PRS_long %>%
        mutate("{analyte}" :=  analyte_frame[[analyte]])

    }

  }

  #assign grouping variable
  avg_PRS_long <- avg_PRS_long %>%
    mutate(Group = case_when(str_detect(Treatment, "Liquid") ~ "Liquid",
                             str_detect(Treatment, "Solid") ~ "Solid",
                             str_detect(Treatment, "Urea") ~ "Urea",
                             str_detect(Treatment, "Control") ~ "Control"),
  Retrieval_Date = case_when(str_detect(Burial, "B1") ~ "05/29/2025",
                             str_detect(Burial, "B2") ~ "06/12/2025",
                             str_detect(Burial, "B3") ~ "06/27/2025",
                             str_detect(Burial, "B4") ~ "07/10/2025"))

  #save each site as dataframe
  assign(paste0(sites_list[[site]],"_PRS_long"),avg_PRS_long)

}

PRS_list_long <- list(RROC_PRS_long,SROC_PRS_long)

#loop for site
for (site in 1 : as.numeric(length(sites_list))) {

 #loop for analyte
  for (analyte in c("NO3-N", "NH4-N", "Ca", "Mg", "K", "P", "Fe", "Mn", "Cu", "Zn", "B", "S", "Pb", "Al", "Cd","Na")) {

      PRS_split <- list(Liquid = filter(PRS_list_long[[site]],Group != "Solid" ),
                        Solid = filter(PRS_list_long[[site]],Group != "Liquid" ),
                        Control_Urea = filter(PRS_list_long[[site]], Group == "Control"| Group == "Urea" ))

      #liquid manure
      liq <- ggplot(data = PRS_split[[1]],
                    aes(group = Treatment,
                        x = Retrieval_Date,
                        y = PRS_split[[1]][[analyte]],
                        color = Group)) +
                    ylim(min(PRS_list_long[[site]][[analyte]],na.rm = TRUE) * 0.75,
                         max(PRS_list_long[[site]][[analyte]],na.rm = TRUE) * 1.25) +
                    guides(color = "none") +
                    geom_line() +
                    geom_point(aes(shape = Treatment, size = 2))+
                    guides(size = "none") +
                    scale_color_manual(values = c("Liquid" = "#ffcc33", "Control" = "#141414", "Urea" = "#A9A9A9" )) +
                    labs(x = "Retrieval Date",
                         y = paste0(analyte," Uptake (ug/10cm^2/Burial Period(days))")) +
                    ggtitle(paste0(sites_list[[site]]," ",analyte," Uptake - Liquid Manure")) +
                    theme_bw() +
                    theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank()) +
                    theme(axis.text = element_text(size = 14)) +
                    theme(legend.text = element_text(size = 12)) +
                    theme(axis.title = element_text(size = 16)) +
                    theme(plot.title = element_text(size = 18)) +
                    theme(legend.position = "bottom") +
                    theme(legend.title = element_blank())
                    #facet_wrap(~Group)

      #solid manure
      sol <- ggplot(data = PRS_split[[2]],
                    aes(group = Treatment,
                        x = Retrieval_Date,
                        y = PRS_split[[2]][[analyte]],
                        color = Group)) +
                    ylim(min(PRS_list_long[[site]][[analyte]],na.rm = TRUE) * 0.75,
                         max(PRS_list_long[[site]][[analyte]],na.rm = TRUE) * 1.25) +
                    guides(color = "none") +
                    geom_line() +
                    geom_point(aes(shape = Treatment, size = 2)) +
                    scale_color_manual(values = c("Solid" = "#7a0019", "Control" = "#141414", "Urea" = "#A9A9A9" )) +
                    guides(size = "none") +
                    labs(x = "Retrieval Date",
                         y = element_blank()) +
                    ggtitle(paste0(sites_list[[site]]," ",analyte," Uptake - Solid Manure")) +
                    theme_bw() +
                    theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank()) +
                    theme(axis.text = element_text(size = 14)) +
                    theme(legend.text = element_text(size = 12)) +
                    theme(axis.title = element_text(size = 16)) +
                    theme(plot.title = element_text(size = 18)) +
                    theme(legend.position = "bottom") +
                    theme(legend.title = element_blank())

                    #facet_wrap(~Group)

    #stack plots
    stacked_plot <- grid.arrange(liq,sol,ncol = 2)

    #save plots
    setwd("G:/Shared drives/ManureLabTeam/ManureResearch_Shared/Experiments/Timing/Data/r_Plots/PRS")
    ggsave(paste0(sites_list[[site]],"_",analyte,"_PRS.png"),stacked_plot, scale = 2.5)

  }

}

#------------------------------------------------------------------------------------#


#Yield by treatment
#------------------------------------------------------------------------------------#
#make index with various arrays for labeling and sorting
#due to missed treatments, arrays always in order --> list(Rosemount,
#                                                          Waseca)
Trt_Index <- list(Groups = list(c("Liquid","Liquid","Liquid","Solid","Solid","Solid","Control","Urea","Urea","Urea","Urea"),
                                c("Liquid","Liquid","Liquid","Liquid","Solid","Solid","Solid","Control","Urea","Urea","Urea","Urea")),
                 Arrange = list(c(8,11,12,9,10,3,1,4,7,5,6),
                                c(8,11,12,9,10,3,1,2,4,7,5,6)),
                 Trt_Num = list(c(1,3,4,5,6,7,8,9,10,11,12),
                                c(1,2,3,4,5,6,7,8,9,10,11,12)),
                 Names =  list(c("Liquid Dairy Early Fall","Liquid Dairy Spring","Liquid Dairy Sidedress","Solid Beef Early Fall","Solid Beef Late Fall",
                                "Solid Beef Spring","Control","Urea 98lbsN","Urea 146lbsN","Urea 195lbsN","Urea 293lbsN"),
                               c("Liquid Swine Early Fall","Liquid Swine Late Fall","Liquid Swine Spring","Liquid Swine Sidedress","Solid Turkey Early Fall","Solid Turkey Late Fall",
                                "Solid Turkey Spring","Control","Urea 75lbsN","Urea 112lbsN","Urea 150lbsN","Urea 225lbsN"),
                               "Control"),
                 Yield_Types = list("Grain_Yield_Bu.acre",
                                    "Silage_Yield_65Percent_Moisture_Ton.acre",
                                    "Grain_Yield_Bu.acre"),
                 Y_labels = list("Grain Yield(Bu/Acre)",
                                 "Silage Yield(65%Moisture)(Ton/Acre)",
                                 "Grain Yield(Bu/Acre)"),
                 Titles = list("Grain Yield",
                               "Silage Yield",
                               "Grain Yield"),
                 Sample_Timings = list("In Season(V4)",
                                       "Post Harvest",
                                       "Pre-Plant"),
                 Sample_Depths = list('0-12in',
                                      '12-24in',
                                      '24-36in'),
                 Sites = list("Rosemount",
                              "Waseca")
                 )

#make list of all raw harvest data
yield_list <- list(Growing_Season_2025$Rosemount_R6,
                   Growing_Season_2025$Rosemount_Silage,
                   Growing_Season_2025$Waseca_R6)

#loop for sites(there are 2 harvests from RROC)
for (yields in as.numeric(1 : length(yield_list))) {

  if (yields == 1 | yields ==  2) {

    site <-  1

  } else if (yields == 3) {

    site <- 2

  }

  #calculate avg yields for each treatment, add treatment number and arrange for plotting
  Avg_Yield <- Yield_list[[yields]] %>%
    group_by(Trt) %>%
    summarise_at(vars(all_of(Trt_Index$Yield_Types[[yields]])),list(Avg_Yield = mean,StdDev_Yield = sd)) %>%
    mutate(Treat_Num = Trt_Index$Arrange[[site]]) %>%
    arrange(Treat_Num) %>%
    mutate(Trt = fct_inorder(Trt),
           Group = Trt_Index$Groups[[site]])


  #Plot Avg Grain Yield by Treatment
  Avg_Grain_Yield_Treatment_Plot <- ggplot(Avg_Yield,
                                           aes(fill = Group,
                                               x = Trt,
                                               y = Avg_Yield)) +
                                              geom_col(color = "black") +
                                              scale_fill_manual(values = c("Liquid" = "#ffcc33", "Solid" = "#7a0019", "Control" = "#141414", "Urea" = "#A9A9A9" )) +
                                              scale_x_discrete(labels = Trt_Index$Names[[site]], guide = guide_axis(n.dodge = 2)) +
                                              ggtitle(paste0(sites_list[[site]]," ",Trt_Index$Titles[[yields]]," by Treatment")) +
                                              ylab(paste0(Trt_Index$Y_labels[[yields]])) +
                                              xlab("Treatment") +
                                              theme_bw() +
                                              theme(
                                                panel.grid.major.x = element_blank(),
                                                panel.grid.minor.x = element_blank()) +
                                              theme(axis.text = element_text(size = 10)) +
                                              theme(legend.text = element_text(size = 14)) +
                                              theme(axis.title = element_text(size = 16)) +
                                              theme(plot.title = element_text(size = 18))

  #save plots
  setwd("G:/Shared drives/ManureLabTeam/ManureResearch_Shared/Experiments/Timing/Data/r_Plots/Yield")
  ggsave(paste0(sites_list[[site]]," ",Trt_Index$Titles[[yields]],".png"),Avg_Grain_Yield_Treatment_Plot, scale = 2.0)

}


#------------------------------------------------------------------------------------#


#Soil N by treatment
#------------------------------------------------------------------------------------#
#pull data from masterlist
soilN_list <- list(Growing_Season_2025$`2025 Rosemount Soil N`,
                   Growing_Season_2025$`2025 Waseca Soil N`)

#loop for site
for (site in 1 : length(soilN_list)) {

  soil_split <- soilN_list[[site]] %>%
    group_split(Timing)

  #loop for timings(-1 to exclude pre-plant timing)
  for (samp in 1 : (length(soil_split) - 1) ) {

    depth_split <- soil_split[[samp]] %>%
      group_split(Depth.in.)

    #loop for depth
    for (depth in 1 :length(depth_split)) {

      #make tibble for each plot
      AvgN <- depth_split[[depth]] %>%
        group_by(Treatment) %>%
        summarise_at(vars("Ch1_NO3.Primary_540nm","Ch3_NH4.Primary_670nm"), list(Avg = mean)) %>%
        mutate(across(Treatment, as.numeric),) %>%
        arrange(Treatment) %>%
        mutate(Trt = Trt_Index$Names[[site]],
               Timing = Trt_Index$Sample_Timings[[samp]]) %>%
        rename("NO3" = Ch1_NO3.Primary_540nm_Avg,
                "NH4" = Ch3_NH4.Primary_670nm_Avg) %>%
        pivot_longer(cols = c("NO3","NH4"),
                     names_to = "N Type",
                     values_to = "N Concentration") %>%
        mutate(Trt = fct_inorder(Trt))

      #set y limits for each sampling
      y_limits <- c(16,5)

      #make plots
      AvgN_plot <- ggplot(AvgN,
                         aes(fill = `N Type`,
                             x = Trt,
                             y = `N Concentration`)) +
                          geom_bar(color = "black", position = position_stack(reverse = TRUE), stat = "identity") +
                          scale_fill_manual(values = c("NO3" = "#ffcc33", "NH4" = "#7a0019")) +
                          scale_x_discrete(labels = Trt_Index$Names[[site]], guide = guide_axis(n.dodge = 2)) +
                          ggtitle(paste0(Trt_Index$Sites[[site]]," ",Trt_Index$Sample_Timings[[samp]], " Soil N by Treatment ", Trt_Index$Sample_Depths[[depth]])) +
                          ylab("Soil N(PPM)") +
                          xlab("Treatment") +
                          ylim(c(-1,y_limits[[samp]])) +
                          theme_bw() +
                          theme(panel.grid.major.x = element_blank(),
                                panel.grid.minor.x = element_blank()) +
                          theme(axis.text = element_text(size = 12)) +
                          theme(legend.text = element_text(size = 14)) +
                          theme(axis.title = element_text(size = 16)) +
                          theme(plot.title = element_text(size = 18)) +
                          theme(legend.position = c(0.1, 0.85)) + # Places legend in the bottom-right
                          theme(legend.background = element_rect(color = "black", fill = "white", linetype = "solid"))

      #save plots
      setwd("G:/Shared drives/ManureLabTeam/ManureResearch_Shared/Experiments/Timing/Data/r_Plots/SoilN")
      ggsave(paste0(Trt_Index$Sites[[site]]," ",Trt_Index$Sample_Timings[[samp]]," Soil ", Trt_Index$Sample_Depths[[depth]],".png"),AvgN_plot, scale = 2)

    }

  }

}
