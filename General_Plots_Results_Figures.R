#Script for making plots for preliminary analysis of Manure Application timing experimental data.
#Each section will include a section to:
  #Re-arrange data for use in specific plots
  #generate plots for various data sources
  #save plots to shared drive

#Plots to make:
  #Yield by treatment
  #cumulative and daily precip over growing season(double y axis bar graph)
  #PRS trends over time(line graph) for each analyte
  #
  #


#Daily and Cumulative Precip
#------------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------#




#PRS Graphs
#------------------------------------------------------------------------------------#
#make datatable for all burials of each plot for each nutrient

PRS_list <-list(Growing_Season_2025$Rosemount_PRS_Data, Growing_Season_2025$Waseca_PRS_Data)
sites_list <- c("RROC","SROC")
column_names <- c("Plot", "Treatment","B1", "B2", "B3"," B4")

#loop for each site
for (site in 1 : as.numeric(length(sites_list))) {

  PRS <- PRS_list[[site]]

  #loop for each analyte
  for (analyte in c("NO3-N", "NH4-N", "Ca", "Mg", "K", "P", "Fe", "Mn", "Cu", "Zn", "B", "S", "Pb", "Al", "Cd","Na")) {

    #loop for each plot. Initialize dataframe for first plot, then append the rest of the plots. Try wrapper eliminates the plots with no data
    for (plot_num in 1 : as.numeric(length(PRS))) {

      if (plot_num == 1)  {
        analyte_frame <- tibble(t(c(substr(PRS[[plot_num]][["Sample ID"]][[1]], 5,8 ),
                                    PRS[[plot_num]][["Treatment"]][[1]],
                                    as.numeric(PRS[[plot_num]][[analyte]]))))

      } else {

        try(analyte_frame <- add_row(analyte_frame, t(c(substr( PRS[[plot_num]][["Sample ID"]][[1]], 5,8 ),
                                                        PRS[[plot_num]][["Treatment"]][[1]],
                                                        PRS[[plot_num]][[analyte]]))),
            silent = TRUE)

      }

    }

    assign(paste0(sites_list[[site]],"_PRS_",analyte),analyte_frame)

  }

}

#calculate averages from each treatment
analyte_frame %>%
  group_by(Treat)



ggplot(data = `RROC_PRS_NO3-N`,
       x = )



#------------------------------------------------------------------------------------#

#Yield by treatment
#------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------#




