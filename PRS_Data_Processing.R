#Script for sorting, labeling, and cleaning the PRS supply rate data from Western Ag Innovations
#Written By: Sam Strack :)
#Last Updated: 10/21/25

#load necessary packages
library(tidyverse)
library(scales)
library(dplyr)

#data location for yearly project PRS data
setwd(paste0(Raw_Data_Loc,"/PRS"))

#import data
PRS_Raw <- lapply((list.files(paste0(Raw_Data_Loc,"/PRS"), pattern=".*PRS results.*\\.csv$", full.names=TRUE)), read.csv)
PRS_Raw_Data <- PRS_Raw[[1]]

#set column labels, remove unnecessary rows/columns
PRS_Labels <- as.character(PRS_Raw_Data[4,])
PRS_Labeled <- PRS_Raw_Data %>%
  set_names(PRS_Labels)

PRS_Raw_Cleaned <- PRS_Labeled[-c(1:5),-c(1,5,6,7,24,25)]

#extract PRS detection limits, NA values that meet or fall below threshold
PRS_DetectionLimits <- PRS_Labeled[5,8:23]

PRS_Raw_Cleaned$`NO3-N`[as.numeric(PRS_Raw_Cleaned$`NO3-N`) <= as.numeric(PRS_DetectionLimits$`NO3-N`)] <- NA
PRS_Raw_Cleaned$`NH4-N`[as.numeric(PRS_Raw_Cleaned$`NH4-N`) <= as.numeric(PRS_DetectionLimits$`NH4-N`)] <- NA
PRS_Raw_Cleaned$`Ca`[as.numeric(PRS_Raw_Cleaned$`Ca`) <= as.numeric(PRS_DetectionLimits$`Ca`)] <- NA
PRS_Raw_Cleaned$`Mg`[as.numeric(PRS_Raw_Cleaned$`Mg`) <= as.numeric(PRS_DetectionLimits$`Mg`)] <- NA
PRS_Raw_Cleaned$`K`[as.numeric(PRS_Raw_Cleaned$`K`) < as.numeric(PRS_DetectionLimits$`K`)] <- NA
PRS_Raw_Cleaned$`P`[as.numeric(PRS_Raw_Cleaned$`P`) <= as.numeric(PRS_DetectionLimits$`P`) ] <- NA
PRS_Raw_Cleaned$`Fe`[as.numeric(PRS_Raw_Cleaned$`Fe`) <= as.numeric(PRS_DetectionLimits$`Fe`)] <- NA
PRS_Raw_Cleaned$`Mn`[as.numeric(PRS_Raw_Cleaned$`Mn`) <= as.numeric(PRS_DetectionLimits$`Mn`)] <- NA
PRS_Raw_Cleaned$`Cu`[as.numeric(PRS_Raw_Cleaned$`Cu`) < as.numeric(PRS_DetectionLimits$`Cu`)] <- NA
PRS_Raw_Cleaned$`Zn`[as.numeric(PRS_Raw_Cleaned$`Zn`) <= as.numeric(PRS_DetectionLimits$`Zn`) ] <- NA
PRS_Raw_Cleaned$`B`[as.numeric(PRS_Raw_Cleaned$`B`) <= as.numeric(PRS_DetectionLimits$`B`)] <- NA
PRS_Raw_Cleaned$`S`[as.numeric(PRS_Raw_Cleaned$`S`) <= as.numeric(PRS_DetectionLimits$`S`)] <- NA
PRS_Raw_Cleaned$`Pb`[as.numeric(PRS_Raw_Cleaned$`Pb`) <= as.numeric(PRS_DetectionLimits$`Pb`)] <- NA
PRS_Raw_Cleaned$`Al`[as.numeric(PRS_Raw_Cleaned$`Al`) <= as.numeric(PRS_DetectionLimits$`Al`)] <- NA
PRS_Raw_Cleaned$`Cd`[as.numeric(PRS_Raw_Cleaned$`Cd`) < as.numeric(PRS_DetectionLimits$`Cd`)] <- NA

#add Sitelabel, burial time, burial label, and treatment columns, split by site
PRS_Split_Data <- PRS_Raw_Cleaned %>%
  mutate(Site = case_when(
    grepl("RROC",`Sample ID`) ~ "Rosemount",
    grepl("SROC",`Sample ID`) ~ "Waseca"
  )) %>%
  mutate(Burial_Time =
    as.numeric(
      date(PRS_Raw_Cleaned$`Retrieval Date`) - date(PRS_Raw_Cleaned$`Burial Date`)

    )
  ) %>%
  mutate(Burial = case_when(
    grepl("B1",`Sample ID`) ~ 1,
    grepl("B2",`Sample ID`) ~ 2,
    grepl("B3",`Sample ID`) ~ 3,
    grepl("B4",`Sample ID`) ~ 4
    ##grepl("B5",`Sample ID`) ~ "Burial_5",
    ##grepl("B6",`Sample ID`) ~ "Burial_6",
    ##grepl("B7",`Sample ID`) ~ "Burial_7",
    ##grepl("B8",`Sample ID`) ~ "Burial_8"

  )) %>%

  group_split(Site)

#make dataframes for each site, split by burial
Rosemount_Data <- PRS_Split_Data[[1]]
Waseca_Data <- PRS_Split_Data[[2]]

#load naming variables
sites <- c("Rosemount","Waseca")
timings <- c("Burial 1","Burial 2","Burial 3","Burial 4") ##,"Burial 5","Burial 6","Burial 7","Burial 8")

#start loop for assigning treatments, and splitting data by burial period by site
for (x in 1: as.numeric(length(sites))) {

  if (x == 1) { #Rosemount

    #set sitename
    SiteName <- sites[x]

    #Set data by site
    PRS_Data <- Rosemount_Data

    #make plot/treatment assignments by sample name
    PRS_Data <- PRS_Data %>%
      mutate(Treatment = case_when(
        #Rep 1
        grepl("101",`Sample ID`) ~ "Liquid Dairy Spring",
        grepl("102",`Sample ID`) ~ "Solid Beef Late Fall",
        grepl("103",`Sample ID`) ~ "Liquid Dairy Sidedress",
        grepl("104",`Sample ID`) ~ "Urea 146lbsN",
        grepl("105",`Sample ID`) ~ "Liquid Dairy Early Fall",
        grepl("106",`Sample ID`) ~ "Solid Beef Early Fall",
        grepl("107",`Sample ID`) ~ "Liquid Dairy Late Fall",
        grepl("108",`Sample ID`) ~ "Solid Beef Spring",
        grepl("109",`Sample ID`) ~ "Urea 293lbsN",
        grepl("110",`Sample ID`) ~ "Urea 98lbsN",
        grepl("111",`Sample ID`) ~ "Urea 195lbsN",
        grepl("112",`Sample ID`) ~ "Control",
        #Rep 2
        grepl("201",`Sample ID`) ~ "Liquid Dairy Sidedress",
        grepl("202",`Sample ID`) ~ "Liquid Dairy Early Fall",
        grepl("203",`Sample ID`) ~ "Urea 293lbsN",
        grepl("204",`Sample ID`) ~ "Solid Beef Spring",
        grepl("205",`Sample ID`) ~ "Solid Beef Early Fall",
        grepl("206",`Sample ID`) ~ "Urea 98lbsN",
        grepl("207",`Sample ID`) ~ "Solid Beef Late Fall",
        grepl("208",`Sample ID`) ~ "Liquid Dairy Spring",
        grepl("209",`Sample ID`) ~ "Urea 146lbsN",
        grepl("210",`Sample ID`) ~ "Control",
        grepl("211",`Sample ID`) ~ "Liquid Dairy Late Fall",
        grepl("212",`Sample ID`) ~ "Urea 195lbsN",
        #Rep 3
        grepl("301",`Sample ID`) ~ "Liquid Dairy Late Fall",
        grepl("302",`Sample ID`) ~ "Urea 146lbsN",
        grepl("303",`Sample ID`) ~ "Liquid Dairy Early Fall",
        grepl("304",`Sample ID`) ~ "Urea 293lbsN",
        grepl("305",`Sample ID`) ~ "Liquid Dairy Spring",
        grepl("306",`Sample ID`) ~ "Liquid Dairy Sidedress",
        grepl("307",`Sample ID`) ~ "Control",
        grepl("308",`Sample ID`) ~ "Urea 195lbsN",
        grepl("309",`Sample ID`) ~ "Solid Beef Early Fall",
        grepl("310",`Sample ID`) ~ "Solid Beef Spring",
        grepl("311",`Sample ID`) ~ "Urea 98lbsN",
        grepl("312",`Sample ID`) ~ "Solid Beef Late Fall",
        #Rep 4
        grepl("401",`Sample ID`) ~ "Solid Beef Late Fall",
        grepl("402",`Sample ID`) ~ "Liquid Dairy Spring",
        grepl("403",`Sample ID`) ~ "Urea 195lbsN",
        grepl("404",`Sample ID`) ~ "Solid Beef Spring",
        grepl("405",`Sample ID`) ~ "Urea 293lbsN",
        grepl("406",`Sample ID`) ~ "Liquid Dairy Late Fall",
        grepl("407",`Sample ID`) ~ "Solid Beef Early Fall",
        grepl("408",`Sample ID`) ~ "Liquid Dairy Sidedress",
        grepl("409",`Sample ID`) ~ "Liquid Dairy Early Fall",
        grepl("410",`Sample ID`) ~ "Control",
        grepl("411",`Sample ID`) ~ "Urea 146lbsN",
        grepl("412",`Sample ID`) ~ "Urea 98lbsN",


        TRUE ~ "N/A"
      ))


    #Make new list with plots as fields
                     #Rep1
    PRS_Data <- list(RROC_101 = filter(PRS_Data,grepl("101",PRS_Data$`Sample ID`)),
                     RROC_102 = filter(PRS_Data,grepl("102",PRS_Data$`Sample ID`)),
                     RROC_103 = filter(PRS_Data,grepl("103",PRS_Data$`Sample ID`)),
                     RROC_104 = filter(PRS_Data,grepl("104",PRS_Data$`Sample ID`)),
                     RROC_105 = filter(PRS_Data,grepl("105",PRS_Data$`Sample ID`)),
                     RROC_106 = filter(PRS_Data,grepl("106",PRS_Data$`Sample ID`)),
                     RROC_107 = filter(PRS_Data,grepl("107",PRS_Data$`Sample ID`)),
                     RROC_108 = filter(PRS_Data,grepl("108",PRS_Data$`Sample ID`)),
                     RROC_109 = filter(PRS_Data,grepl("109",PRS_Data$`Sample ID`)),
                     RROC_110 = filter(PRS_Data,grepl("110",PRS_Data$`Sample ID`)),
                     RROC_111 = filter(PRS_Data,grepl("111",PRS_Data$`Sample ID`)),
                     RROC_112 = filter(PRS_Data,grepl("112",PRS_Data$`Sample ID`)),
                     #Rep2
                     RROC_201 = filter(PRS_Data,grepl("201",PRS_Data$`Sample ID`)),
                     RROC_202 = filter(PRS_Data,grepl("202",PRS_Data$`Sample ID`)),
                     RROC_203 = filter(PRS_Data,grepl("203",PRS_Data$`Sample ID`)),
                     RROC_204 = filter(PRS_Data,grepl("204",PRS_Data$`Sample ID`)),
                     RROC_205 = filter(PRS_Data,grepl("205",PRS_Data$`Sample ID`)),
                     RROC_206 = filter(PRS_Data,grepl("206",PRS_Data$`Sample ID`)),
                     RROC_207 = filter(PRS_Data,grepl("207",PRS_Data$`Sample ID`)),
                     RROC_208 = filter(PRS_Data,grepl("208",PRS_Data$`Sample ID`)),
                     RROC_209 = filter(PRS_Data,grepl("209",PRS_Data$`Sample ID`)),
                     RROC_210 = filter(PRS_Data,grepl("210",PRS_Data$`Sample ID`)),
                     RROC_211 = filter(PRS_Data,grepl("211",PRS_Data$`Sample ID`)),
                     RROC_212 = filter(PRS_Data,grepl("212",PRS_Data$`Sample ID`)),
                     #Rep3
                     RROC_301 = filter(PRS_Data,grepl("301",PRS_Data$`Sample ID`)),
                     RROC_302 = filter(PRS_Data,grepl("302",PRS_Data$`Sample ID`)),
                     RROC_303 = filter(PRS_Data,grepl("303",PRS_Data$`Sample ID`)),
                     RROC_304 = filter(PRS_Data,grepl("304",PRS_Data$`Sample ID`)),
                     RROC_305 = filter(PRS_Data,grepl("305",PRS_Data$`Sample ID`)),
                     RROC_306 = filter(PRS_Data,grepl("306",PRS_Data$`Sample ID`)),
                     RROC_307 = filter(PRS_Data,grepl("307",PRS_Data$`Sample ID`)),
                     RROC_308 = filter(PRS_Data,grepl("308",PRS_Data$`Sample ID`)),
                     RROC_309 = filter(PRS_Data,grepl("309",PRS_Data$`Sample ID`)),
                     RROC_310 = filter(PRS_Data,grepl("310",PRS_Data$`Sample ID`)),
                     RROC_311 = filter(PRS_Data,grepl("311",PRS_Data$`Sample ID`)),
                     RROC_312 = filter(PRS_Data,grepl("312",PRS_Data$`Sample ID`)),
                     #Rep4
                     RROC_401 = filter(PRS_Data,grepl("401",PRS_Data$`Sample ID`)),
                     RROC_402 = filter(PRS_Data,grepl("402",PRS_Data$`Sample ID`)),
                     RROC_403 = filter(PRS_Data,grepl("403",PRS_Data$`Sample ID`)),
                     RROC_404 = filter(PRS_Data,grepl("404",PRS_Data$`Sample ID`)),
                     RROC_405 = filter(PRS_Data,grepl("405",PRS_Data$`Sample ID`)),
                     RROC_406 = filter(PRS_Data,grepl("406",PRS_Data$`Sample ID`)),
                     RROC_407 = filter(PRS_Data,grepl("407",PRS_Data$`Sample ID`)),
                     RROC_408 = filter(PRS_Data,grepl("408",PRS_Data$`Sample ID`)),
                     RROC_409 = filter(PRS_Data,grepl("409",PRS_Data$`Sample ID`)),
                     RROC_410 = filter(PRS_Data,grepl("410",PRS_Data$`Sample ID`)),
                     RROC_411 = filter(PRS_Data,grepl("411",PRS_Data$`Sample ID`)),
                     RROC_412 = filter(PRS_Data,grepl("412",PRS_Data$`Sample ID`))
    )

  }

  if (x == 2) { #Waseca

    #set sitename
    SiteName <- sites[x]

    #Set data by site
    PRS_Data <- Waseca_Data

    #make plot/treatment assignments by sample name
    PRS_Data <- PRS_Data %>%
      mutate(Treatment = case_when(
        #Rep 1
        grepl("101",`Sample ID`) ~ "Liquid Swine Spring",
        grepl("102",`Sample ID`) ~ "Solid Turkey Late Fall",
        grepl("103",`Sample ID`) ~ "Liquid Swine Sidedress",
        grepl("104",`Sample ID`) ~ "Urea 112lbsN",
        grepl("105",`Sample ID`) ~ "Liquid Swine Early Fall",
        grepl("106",`Sample ID`) ~ "Solid Turkey Early Fall",
        grepl("107",`Sample ID`) ~ "Liquid Swine Late Fall",
        grepl("108",`Sample ID`) ~ "Solid Turkey Spring",
        grepl("109",`Sample ID`) ~ "Urea 225lbsN",
        grepl("110",`Sample ID`) ~ "Urea 75lbsN",
        grepl("111",`Sample ID`) ~ "Urea 150lbsN",
        grepl("112",`Sample ID`) ~ "Control",
        #Rep 2
        grepl("201",`Sample ID`) ~ "Liquid Swine Sidedress",
        grepl("202",`Sample ID`) ~ "Liquid Swine Early Fall",
        grepl("203",`Sample ID`) ~ "Urea 225lbsN",
        grepl("204",`Sample ID`) ~ "Solid Turkey Spring",
        grepl("205",`Sample ID`) ~ "Solid Turkey Early Fall",
        grepl("206",`Sample ID`) ~ "Urea 75lbsN",
        grepl("207",`Sample ID`) ~ "Solid Turkey Late Fall",
        grepl("208",`Sample ID`) ~ "Liquid Swine Spring",
        grepl("209",`Sample ID`) ~ "Urea 112lbsN",
        grepl("210",`Sample ID`) ~ "Control",
        grepl("211",`Sample ID`) ~ "Liquid Swine Late Fall",
        grepl("212",`Sample ID`) ~ "Urea 150lbsN",
        #Rep 3
        grepl("301",`Sample ID`) ~ "Liquid Swine Late Fall",
        grepl("302",`Sample ID`) ~ "Urea 112lbsN",
        grepl("303",`Sample ID`) ~ "Liquid Swine Early Fall",
        grepl("304",`Sample ID`) ~ "Urea 225lbsN",
        grepl("305",`Sample ID`) ~ "Liquid Swine Spring",
        grepl("306",`Sample ID`) ~ "Liquid Swine Sidedress",
        grepl("307",`Sample ID`) ~ "Control",
        grepl("308",`Sample ID`) ~ "Urea 150lbsN",
        grepl("309",`Sample ID`) ~ "Solid Turkey Early Fall",
        grepl("310",`Sample ID`) ~ "Solid Turkey Spring",
        grepl("311",`Sample ID`) ~ "Urea 75lbsN",
        grepl("312",`Sample ID`) ~ "Solid Turkey Late Fall",
        #Rep 4
        grepl("401",`Sample ID`) ~ "Solid Turkey Late Fall",
        grepl("402",`Sample ID`) ~ "Liquid Swine Spring",
        grepl("403",`Sample ID`) ~ "Solid Turkey Spring",
        grepl("404",`Sample ID`) ~ "Urea 150lbsN",
        grepl("405",`Sample ID`) ~ "Urea 225lbsN",
        grepl("406",`Sample ID`) ~ "Liquid Swine Late Fall",
        grepl("407",`Sample ID`) ~ "Solid Turkey Early Fall",
        grepl("408",`Sample ID`) ~ "Liquid Swine Sidedress",
        grepl("409",`Sample ID`) ~ "Liquid Swine Early Fall",
        grepl("410",`Sample ID`) ~ "Control",
        grepl("411",`Sample ID`) ~ "Urea 112lbsN",
        grepl("412",`Sample ID`) ~ "Urea 75lbsN",

        TRUE ~ "N/A"
      ))

    #Make new list with plots as fields
    #Rep1
    PRS_Data <- list(SROC_101 = filter(PRS_Data,grepl("101",PRS_Data$`Sample ID`)),
                     SROC_102 = filter(PRS_Data,grepl("102",PRS_Data$`Sample ID`)),
                     SROC_103 = filter(PRS_Data,grepl("103",PRS_Data$`Sample ID`)),
                     SROC_104 = filter(PRS_Data,grepl("104",PRS_Data$`Sample ID`)),
                     SROC_105 = filter(PRS_Data,grepl("105",PRS_Data$`Sample ID`)),
                     SROC_106 = filter(PRS_Data,grepl("106",PRS_Data$`Sample ID`)),
                     SROC_107 = filter(PRS_Data,grepl("107",PRS_Data$`Sample ID`)),
                     SROC_108 = filter(PRS_Data,grepl("108",PRS_Data$`Sample ID`)),
                     SROC_109 = filter(PRS_Data,grepl("109",PRS_Data$`Sample ID`)),
                     SROC_110 = filter(PRS_Data,grepl("110",PRS_Data$`Sample ID`)),
                     SROC_111 = filter(PRS_Data,grepl("111",PRS_Data$`Sample ID`)),
                     SROC_112 = filter(PRS_Data,grepl("112",PRS_Data$`Sample ID`)),
                     #Rep2
                     SROC_201 = filter(PRS_Data,grepl("201",PRS_Data$`Sample ID`)),
                     SROC_202 = filter(PRS_Data,grepl("202",PRS_Data$`Sample ID`)),
                     SROC_203 = filter(PRS_Data,grepl("203",PRS_Data$`Sample ID`)),
                     SROC_204 = filter(PRS_Data,grepl("204",PRS_Data$`Sample ID`)),
                     SROC_205 = filter(PRS_Data,grepl("205",PRS_Data$`Sample ID`)),
                     SROC_206 = filter(PRS_Data,grepl("206",PRS_Data$`Sample ID`)),
                     SROC_207 = filter(PRS_Data,grepl("207",PRS_Data$`Sample ID`)),
                     SROC_208 = filter(PRS_Data,grepl("208",PRS_Data$`Sample ID`)),
                     SROC_209 = filter(PRS_Data,grepl("209",PRS_Data$`Sample ID`)),
                     SROC_210 = filter(PRS_Data,grepl("210",PRS_Data$`Sample ID`)),
                     SROC_211 = filter(PRS_Data,grepl("211",PRS_Data$`Sample ID`)),
                     SROC_212 = filter(PRS_Data,grepl("212",PRS_Data$`Sample ID`)),
                     #Rep3
                     SROC_301 = filter(PRS_Data,grepl("301",PRS_Data$`Sample ID`)),
                     SROC_302 = filter(PRS_Data,grepl("302",PRS_Data$`Sample ID`)),
                     SROC_303 = filter(PRS_Data,grepl("303",PRS_Data$`Sample ID`)),
                     SROC_304 = filter(PRS_Data,grepl("304",PRS_Data$`Sample ID`)),
                     SROC_305 = filter(PRS_Data,grepl("305",PRS_Data$`Sample ID`)),
                     SROC_306 = filter(PRS_Data,grepl("306",PRS_Data$`Sample ID`)),
                     SROC_307 = filter(PRS_Data,grepl("307",PRS_Data$`Sample ID`)),
                     SROC_308 = filter(PRS_Data,grepl("308",PRS_Data$`Sample ID`)),
                     SROC_309 = filter(PRS_Data,grepl("309",PRS_Data$`Sample ID`)),
                     SROC_310 = filter(PRS_Data,grepl("310",PRS_Data$`Sample ID`)),
                     SROC_311 = filter(PRS_Data,grepl("311",PRS_Data$`Sample ID`)),
                     SROC_312 = filter(PRS_Data,grepl("312",PRS_Data$`Sample ID`)),
                     #Rep4
                     SROC_401 = filter(PRS_Data,grepl("401",PRS_Data$`Sample ID`)),
                     SROC_402 = filter(PRS_Data,grepl("402",PRS_Data$`Sample ID`)),
                     SROC_403 = filter(PRS_Data,grepl("403",PRS_Data$`Sample ID`)),
                     SROC_404 = filter(PRS_Data,grepl("404",PRS_Data$`Sample ID`)),
                     SROC_405 = filter(PRS_Data,grepl("405",PRS_Data$`Sample ID`)),
                     SROC_406 = filter(PRS_Data,grepl("406",PRS_Data$`Sample ID`)),
                     SROC_407 = filter(PRS_Data,grepl("407",PRS_Data$`Sample ID`)),
                     SROC_408 = filter(PRS_Data,grepl("408",PRS_Data$`Sample ID`)),
                     SROC_409 = filter(PRS_Data,grepl("409",PRS_Data$`Sample ID`)),
                     SROC_410 = filter(PRS_Data,grepl("410",PRS_Data$`Sample ID`)),
                     SROC_411 = filter(PRS_Data,grepl("411",PRS_Data$`Sample ID`)),
                     SROC_412 = filter(PRS_Data,grepl("412",PRS_Data$`Sample ID`))
    )

  }

  #make dataframe for each site
  assign(paste0(SiteName,"_PRS_Data"),PRS_Data)

}

#append PRS data to master datalist ##ADD ADDITIONAL BURIALS HERE
Growing_Season_2025 <- append(Growing_Season_2025,list(Rosemount_PRS_Data = Rosemount_PRS_Data,Waseca_PRS_Data = Waseca_PRS_Data))

#remove  in-process variables
rm(list = ls()[!ls() %in% c("Growing_Season_2025","ManureTiming_Loc","Analysis_Loc","Raw_Data_Loc","Plots_Loc","Scripts_Loc")])
