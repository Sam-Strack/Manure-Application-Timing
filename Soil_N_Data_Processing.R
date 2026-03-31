#Script for sorting, labeling, and cleaning the raw inorganic soil N data from the FIAlab into long format
#for plotting and analysis
#Written By: Sam Strack :)
#Last Updated: 03/30/2025

#load necessary packages
library(tidyverse)
library(scales)
library(dplyr)

soilN <- list(Rosemount_Data = lapply((list.files(paste0(Raw_Data_Loc,"/SoilN"), pattern=".*Rosemount.*\\.csv$", full.names=TRUE)), read.csv),
              Waseca_Data = lapply((list.files(paste0(Raw_Data_Loc,"/SoilN"), pattern=".*Waseca.*\\.csv$", full.names=TRUE)), read.csv))

#make arrays for loops
year <- c("2025", "2026")
sites <- c("Rosemount", "Waseca")
timings <- c("In Season", "Post Harvest", "Pre-Plant")

#loop for sites
for (site in 1 : length(sites)) {

  #list for each site, then assign sample timing names
  site_soilN <- soilN[[site]]
  site_soilN <- set_names(site_soilN, timings)

  #initialize dataframe for all soilN data
  soil_samp_combined <- tibble()

  #loop for sample timings
  for (samp in 1 : length(site_soilN)) {

    #remove rows with FIA standards, checks, blanks. Add column for sample timings and treatments
    soil_samp <- site_soilN[[samp]] %>%
      filter(!grepl("Std",Sample.Name),
             !grepl("Stabil inj",Sample.Name),
             !grepl("Check",Sample.Name),
             !grepl("Blank",Sample.Name)) %>%
      mutate(across(Sample.Name, as.numeric),
             Timing = timings[[samp]],
             Treatment = case_when(Sample.Name %in% c(105,202,303,409) ~ "1",
                                   Sample.Name %in% c(107,211,301,406) ~ "2",
                                   Sample.Name %in% c(101,208,305,402) ~ "3",
                                   Sample.Name %in% c(103,201,306,408) ~ "4",
                                   Sample.Name %in% c(106,205,309,407) ~ "5",
                                   Sample.Name %in% c(102,207,312,401) ~ "6",
                                   Sample.Name %in% c(108,204,310,403) ~ "7",
                                   Sample.Name %in% c(112,210,307,410) ~ "8",
                                   Sample.Name %in% c(110,206,311,412) ~ "9",
                                   Sample.Name %in% c(104,209,302,411) ~ "10",
                                   Sample.Name %in% c(111,212,308,404) ~ "11",
                                   Sample.Name %in% c(109,203,304,405) ~ "12"))

    #append tibbles to each other in long format
    soil_samp_combined <- bind_rows(soil_samp_combined,soil_samp)

  }

  #make new tibble named based on year/site
  assign(paste(year[[1]],sites[[site]], "Soil N"), soil_samp_combined)

}

#add final tibbles to master list for growing season
Growing_Season_2025 <- append(Growing_Season_2025, list(`2025 Rosemount Soil N` = `2025 Rosemount Soil N`,
                                                        `2025 Waseca Soil N` = `2025 Waseca Soil N`))

#remove in process variables, keep specified variables
rm(list = ls()[!ls() %in% c("Growing_Season_2025","ManureTiming_Loc","Analysis_Loc","Raw_Data_Loc","Plots_Loc","Scripts_Loc")])
