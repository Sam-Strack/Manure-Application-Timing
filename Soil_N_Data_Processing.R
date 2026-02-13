#Script for sorting, labeling, and cleaning the raw inorganic soil N data from the FIAlab
#Written By: Sam Strack :)
#Last Updated: 10/21/25

#load necessary packages
library(tidyverse)
library(scales)
library(dplyr)

#data location for yearly project soil data
setwd(paste0(Raw_Data_Loc,"/SoilN"))

PRS_Raw <- lapply((list.files(paste0(Raw_Data_Loc,"/PRS"), pattern=".*PRS results.*\\.csv$", full.names=TRUE)), read.csv)


#load soil N data into tables based on location in filename
Rosemount_Data <- lapply((list.files(paste0(Raw_Data_Loc,"/SoilN"), pattern=".*Rosemount.*\\.csv$", full.names=TRUE)), read.csv)
Waseca_Data <- lapply((list.files(paste0(Raw_Data_Loc,"/SoilN"), pattern=".*Waseca.*\\.csv$", full.names=TRUE)), read.csv)

#load naming variables
SampleTimingNames <- c("Fall","In-Season","Pre-Plant") ##"Post-Harvest"
sites <- c("Rosemount","Waseca")

#appending treatment and depth labels to soil concentration data from FIALab output
#repeat loop for each site
for (x in 1:as.numeric(length(sites))) {

  if (x == 1) { #Rosemount
  SiteName <- sites[x]

    for (y in 1:as.numeric(length(SampleTimingNames))) {

      #separate by sampling time
      if (y == 1) {
      Soil_Data <- Rosemount_Data[[y]]
      TimingName <- SampleTimingNames[[y]]
      }
      else if (y == 2) {
      Soil_Data <- Rosemount_Data[[y]]
      TimingName <- SampleTimingNames[[y]]
      }
      else if (y == 3) {
      Soil_Data <- Rosemount_Data[[y]]
      TimingName <- SampleTimingNames[[y]]
      }
      ##else if (y == 4) {
      ##Soil_Data <- Rosemount_Data[[y]]
      ##TimingName <- SampleTimingNames[[y]]
      ##}

      #Average DUP measurements to have 1 measurement per plot/depth
      Soil_Data <- Soil_Data %>%
        group_by(Sample.Name) %>%
        summarise(Soil_NO3 = mean(Ch1_NO3.Primary_540nm),
                  Soil_NH4 = mean(Ch3_NH4.Primary_670nm))

      #make plot/depth assignments by sample name -> "a" = 0-12in "b" = 12-24in "c" = 24-36in
      Soil_Data <- Soil_Data %>%
        mutate(Depth = case_when(
          grepl("a",Sample.Name) ~ "0-12",
          grepl("b",Sample.Name) ~ "12-24",
          grepl("c",Sample.Name) ~ "24-36",
          #whole rep samples
          grepl("Rep1",Sample.Name) ~ "0-6",
          grepl("Rep2",Sample.Name) ~ "0-6",
          grepl("Rep3",Sample.Name) ~ "0-6",
          grepl("Rep4",Sample.Name) ~ "0-6",

          TRUE ~ "N/A"
        )) %>%

        #make plot/treatment assignments by sample name
        mutate(Treatment = case_when(
          #Rep 1
          grepl("101",Sample.Name) ~ "Liquid Dairy Spring",
          grepl("102",Sample.Name) ~ "Solid Beef Late Fall",
          grepl("103",Sample.Name) ~ "Liquid Dairy Sidedress",
          grepl("104",Sample.Name) ~ "Urea 146lbsN",
          grepl("105",Sample.Name) ~ "Liquid Dairy Early Fall",
          grepl("106",Sample.Name) ~ "Solid Beef Early Fall",
          grepl("107",Sample.Name) ~ "Liquid Dairy Late Fall",
          grepl("108",Sample.Name) ~ "Solid Beef Spring",
          grepl("109",Sample.Name) ~ "Urea 293lbsN",
          grepl("110",Sample.Name) ~ "Urea 98lbsN",
          grepl("111",Sample.Name) ~ "Urea 195lbsN",
          grepl("112",Sample.Name) ~ "Control",
          #Rep 2
          grepl("201",Sample.Name) ~ "Liquid Dairy Sidedress",
          grepl("202",Sample.Name) ~ "Liquid Dairy Early Fall",
          grepl("203",Sample.Name) ~ "Urea 293lbsN",
          grepl("204",Sample.Name) ~ "Solid Beef Spring",
          grepl("205",Sample.Name) ~ "Solid Beef Early Fall",
          grepl("206",Sample.Name) ~ "Urea 98lbsN",
          grepl("207",Sample.Name) ~ "Solid Beef Late Fall",
          grepl("208",Sample.Name) ~ "Liquid Dairy Spring",
          grepl("209",Sample.Name) ~ "Urea 146lbsN",
          grepl("210",Sample.Name) ~ "Control",
          grepl("211",Sample.Name) ~ "Liquid Dairy Late Fall",
          grepl("212",Sample.Name) ~ "Urea 195lbsN",
          #Rep 3
          grepl("301",Sample.Name) ~ "Liquid Dairy Late Fall",
          grepl("302",Sample.Name) ~ "Urea 146lbsN",
          grepl("303",Sample.Name) ~ "Liquid Dairy Early Fall",
          grepl("304",Sample.Name) ~ "Urea 293lbsN",
          grepl("305",Sample.Name) ~ "Liquid Dairy Spring",
          grepl("306",Sample.Name) ~ "Liquid Dairy Sidedress",
          grepl("307",Sample.Name) ~ "Control",
          grepl("308",Sample.Name) ~ "Urea 195lbsN",
          grepl("309",Sample.Name) ~ "Solid Beef Early Fall",
          grepl("310",Sample.Name) ~ "Solid Beef Spring",
          grepl("311",Sample.Name) ~ "Urea 98lbsN",
          grepl("312",Sample.Name) ~ "Solid Beef Late Fall",
          #Rep 4
          grepl("401",Sample.Name) ~ "Solid Beef Late Fall",
          grepl("402",Sample.Name) ~ "Liquid Dairy Spring",
          grepl("403",Sample.Name) ~ "Urea 195lbsN",
          grepl("404",Sample.Name) ~ "Solid Beef Spring",
          grepl("405",Sample.Name) ~ "Urea 293lbsN",
          grepl("406",Sample.Name) ~ "Liquid Dairy Late Fall",
          grepl("407",Sample.Name) ~ "Solid Beef Early Fall",
          grepl("408",Sample.Name) ~ "Liquid Dairy Sidedress",
          grepl("409",Sample.Name) ~ "Liquid Dairy Early Fall",
          grepl("410",Sample.Name) ~ "Control",
          grepl("411",Sample.Name) ~ "Urea 146lbsN",
          grepl("412",Sample.Name) ~ "Urea 98lbsN",
          #whole rep samples
          grepl("Rep1",Sample.Name) ~ "Rep1",
          grepl("Rep2",Sample.Name) ~ "Rep2",
          grepl("Rep3",Sample.Name) ~ "Rep3",
          grepl("Rep4",Sample.Name) ~ "Rep4",

          TRUE ~ "N/A"
        ))

      #check flags remove column if none, alert if exist
      if ("Flags" %in% names(Soil_Data)) {
        flagcheck <- sum(Soil_Data$Flags)

        if (is.na(flagcheck)) {
          Soil_Data$Flags <- NULL
        }
        else if (is.integer(flagcheck)) {
        print(paste0("Flag present in ",SiteName,"_",TimingName,"_Soil"))
        }

      }

     # else {
     # }

      #remove rows with treatment = N/A
      Soil_Data <- Soil_Data[!grepl("N/A",Soil_Data$Treatment, ignore.case = TRUE),]

      #If plots are samples at multiple depths,Separate samples by depth, then remove depth tag from plot label,
      #add depth to N value label, then recombine dataframes
      if (grepl("Rep",Soil_Data$Treatment[1]) == FALSE) {

        Soil_0_12 <- Soil_Data[grepl("0-12", Soil_Data$Depth, ignore.case = TRUE), ]
        Soil_12_24 <- Soil_Data[grepl("12-24", Soil_Data$Depth, ignore.case = TRUE), ]

        Soil_0_12 <- Soil_0_12 %>%
          rename(Soil_NO3_0_12 = Soil_NO3) %>%
          rename(Soil_NH4_0_12 = Soil_NH4) %>%
          select(-Depth) %>%
          mutate(Sample.Name = str_remove(Soil_0_12$Sample.Name, "[ab]"))


        Soil_12_24 <- Soil_12_24 %>%
          rename(Soil_NO3_12_24 = Soil_NO3) %>%
          rename(Soil_NH4_12_24 = Soil_NH4) %>%
          select(-Depth) %>%
          select(-Treatment) %>%
          mutate(Sample.Name = str_remove(Soil_12_24$Sample.Name, "[ab]"))


        Soil_Data_join <-inner_join(Soil_0_12,Soil_12_24, by = "Sample.Name")
        Soil_Data <- Soil_Data_join %>%
          rename(Plot = Sample.Name) %>%
          relocate(Treatment, .after = Plot)
      }

      #make dataframe for each timing
      assign(paste0(SiteName,"_",TimingName,"_Soil"),Soil_Data)

    }

  }

  else if (x == 2) { #Waseca
  SiteName <- sites[x]

    for (y in 1:as.numeric(length(SampleTimingNames))) {

      #separate by sampling time
      if (y == 1) {
        Soil_Data <- Waseca_Data[[y]]
        TimingName <- SampleTimingNames[[y]]
      }
      else if (y == 2) {
        Soil_Data <- Waseca_Data[[y]]
        TimingName <- SampleTimingNames[[y]]
      }
      else if (y == 3) {
        Soil_Data <- Waseca_Data[[y]]
        TimingName <- SampleTimingNames[[y]]
      }
      #else if (y == 4) {
      #Soil_Data <- Waseca_Data[[y]]
      #TimingName <- SampleTimingNames[[y]]
      #}

      #Average DUP measurements to have 1 measurement per plot/depth
      Soil_Data <- Soil_Data %>%
        group_by(Sample.Name) %>%
        summarise(Soil_NO3 = mean(Ch1_NO3.Primary_540nm),
                  Soil_NH4 = mean(Ch3_NH4.Primary_670nm))

      #make plot/depth assignments by sample name -> "a" = 0-12in "b" = 12-24in "c" = 24-36in
      Soil_Data <- Soil_Data %>%
        mutate(Depth = case_when(
          grepl("a",Sample.Name) ~ "0-12",
          grepl("b",Sample.Name) ~ "12-24",
          grepl("c",Sample.Name) ~ "24-36",
          #whole rep samples
          grepl("Rep1",Sample.Name) ~ "0-6",
          grepl("Rep2",Sample.Name) ~ "0-6",
          grepl("Rep3",Sample.Name) ~ "0-6",
          grepl("Rep4",Sample.Name) ~ "0-6",

          TRUE ~ "N/A"
        )) %>%

        #make plot/treatment assignments by sample name
        mutate(Treatment = case_when(
          #Rep 1
          grepl("101",Sample.Name) ~ "Liquid Swine Spring",
          grepl("102",Sample.Name) ~ "Solid Turkey Late Fall",
          grepl("103",Sample.Name) ~ "Liquid Swine Sidedress",
          grepl("104",Sample.Name) ~ "Urea 112lbsN",
          grepl("105",Sample.Name) ~ "Liquid Swine Early Fall",
          grepl("106",Sample.Name) ~ "Solid Turkey Early Fall",
          grepl("107",Sample.Name) ~ "Liquid Swine Late Fall",
          grepl("108",Sample.Name) ~ "Solid Turkey Spring",
          grepl("109",Sample.Name) ~ "Urea 225lbsN",
          grepl("110",Sample.Name) ~ "Urea 75lbsN",
          grepl("111",Sample.Name) ~ "Urea 150lbsN",
          grepl("112",Sample.Name) ~ "Control",
          #Rep 2
          grepl("201",Sample.Name) ~ "Liquid Swine Sidedress",
          grepl("202",Sample.Name) ~ "Liquid Swine Early Fall",
          grepl("203",Sample.Name) ~ "Urea 225lbsN",
          grepl("204",Sample.Name) ~ "Solid Turkey Spring",
          grepl("205",Sample.Name) ~ "Solid Turkey Early Fall",
          grepl("206",Sample.Name) ~ "Urea 75lbsN",
          grepl("207",Sample.Name) ~ "Solid Turkey Late Fall",
          grepl("208",Sample.Name) ~ "Liquid Swine Spring",
          grepl("209",Sample.Name) ~ "Urea 112lbsN",
          grepl("210",Sample.Name) ~ "Control",
          grepl("211",Sample.Name) ~ "Liquid Swine Late Fall",
          grepl("212",Sample.Name) ~ "Urea 150lbsN",
          #Rep 3
          grepl("301",Sample.Name) ~ "Liquid Swine Late Fall",
          grepl("302",Sample.Name) ~ "Urea 112lbsN",
          grepl("303",Sample.Name) ~ "Liquid Swine Early Fall",
          grepl("304",Sample.Name) ~ "Urea 225lbsN",
          grepl("305",Sample.Name) ~ "Liquid Swine Spring",
          grepl("306",Sample.Name) ~ "Liquid Swine Sidedress",
          grepl("307",Sample.Name) ~ "Control",
          grepl("308",Sample.Name) ~ "Urea 150lbsN",
          grepl("309",Sample.Name) ~ "Solid Turkey Early Fall",
          grepl("310",Sample.Name) ~ "Solid Turkey Spring",
          grepl("311",Sample.Name) ~ "Urea 75lbsN",
          grepl("312",Sample.Name) ~ "Solid Turkey Late Fall",
          #Rep 4
          grepl("401",Sample.Name) ~ "Solid Turkey Late Fall",
          grepl("402",Sample.Name) ~ "Liquid Swine Spring",
          grepl("403",Sample.Name) ~ "Solid Turkey Spring",
          grepl("404",Sample.Name) ~ "Urea 150lbsN",
          grepl("405",Sample.Name) ~ "Urea 225lbsN",
          grepl("406",Sample.Name) ~ "Liquid Swine Late Fall",
          grepl("407",Sample.Name) ~ "Solid Turkey Early Fall",
          grepl("408",Sample.Name) ~ "Liquid Swine Sidedress",
          grepl("409",Sample.Name) ~ "Liquid Swine Early Fall",
          grepl("410",Sample.Name) ~ "Control",
          grepl("411",Sample.Name) ~ "Urea 112lbsN",
          grepl("412",Sample.Name) ~ "Urea 75lbsN",
          #whole rep samples
          grepl("Rep1",Sample.Name) ~ "Rep1",
          grepl("Rep2",Sample.Name) ~ "Rep2",
          grepl("Rep3",Sample.Name) ~ "Rep3",
          grepl("Rep4",Sample.Name) ~ "Rep4",

          TRUE ~ "N/A"
        ))

      #check flags remove column if none, alert if exist
      if ("Flags" %in% names(Soil_Data)) {
        flagcheck <- sum(Soil_Data$Flags)

        if (is.na(flagcheck)) {
          Soil_Data$Flags <- NULL
        }
        else if (is.integer(flagcheck)) {
          print(paste0("Flag present in ",SiteName,"_",TimingName,"_Soil"))
        }

      }

      else {
      }

      #remove rows with treatment = N/A
      Soil_Data <- Soil_Data[!grepl("N/A",Soil_Data$Treatment, ignore.case = TRUE),]

      #If plots are samples at multiple depths,Separate samples by depth, then remove depth tag from plot label,
      #add depth to N value label, then recombine dataframes
      if (grepl("Rep",Soil_Data$Treatment[1]) == FALSE) {

        Soil_0_12 <- Soil_Data[grepl("0-12", Soil_Data$Depth, ignore.case = TRUE), ]
        Soil_12_24 <- Soil_Data[grepl("12-24", Soil_Data$Depth, ignore.case = TRUE), ]

        Soil_0_12 <- Soil_0_12 %>%
          rename(Soil_NO3_0_12 = Soil_NO3) %>%
          rename(Soil_NH4_0_12 = Soil_NH4) %>%
          select(-Depth) %>%
          mutate(Sample.Name = str_remove(Soil_0_12$Sample.Name, "[ab]"))


        Soil_12_24 <- Soil_12_24 %>%
          rename(Soil_NO3_12_24 = Soil_NO3) %>%
          rename(Soil_NH4_12_24 = Soil_NH4) %>%
          select(-Depth) %>%
          select(-Treatment) %>%
          mutate(Sample.Name = str_remove(Soil_12_24$Sample.Name, "[ab]"))


        Soil_Data_join <-inner_join(Soil_0_12,Soil_12_24, by = "Sample.Name")
        Soil_Data <- Soil_Data_join %>%
          rename(Plot = Sample.Name) %>%
          relocate(Treatment, .after = Plot)
      }

      #make dataframe for each timing
      assign(paste0(SiteName,"_",TimingName,"_Soil"),Soil_Data)

    }

  }

}


#add final dataframes to master list for growing season
Growing_Season_2025 <- list(Rosemount_Fall_Soil = Rosemount_Fall_Soil,`Rosemount_Pre-Plant_Soil` = `Rosemount_Pre-Plant_Soil`,`Rosemount_In-Season_Soil` = `Rosemount_In-Season_Soil`,
                            Waseca_Fall_Soil = Waseca_Fall_Soil,`Waseca_Pre-Plant_Soil` = `Waseca_Pre-Plant_Soil`,`Waseca_In-Season_Soil` = `Waseca_In-Season_Soil`)

#remove in process variables, keep specified variables
rm(list = ls()[!ls() %in% c("Growing_Season_2025","ManureTiming_Loc","Analysis_Loc","Raw_Data_Loc","Plots_Loc","Scripts_Loc")])
