#Script for sorting, labeling, and cleaning the plant/grain data from R6 sampling,
#and silage/grain harvesting
#Written By: Sam Strack :)
#Last Updated: 1/30/26

#load necessary packages
library(tidyverse)
library(scales)
library(dplyr)

#data location for yearly project PRS data
setwd(paste0(Raw_Data_Loc,"/Plant_Grain"))


#load plant data into tables based on location in filename
Rosemount_Data <- lapply((list.files(paste0(Raw_Data_Loc,"/Plant_Grain"), pattern=".*Rosemount.*\\.csv$", full.names=TRUE)), read.csv)
Waseca_Data <- lapply((list.files(paste0(Raw_Data_Loc,"/Plant_Grain"), pattern=".*Waseca.*\\.csv$", full.names=TRUE)), read.csv)

Rosemount_R6 <- Rosemount_Data[[1]]
Rosemount_Silage <- Rosemount_Data[[2]]
Waseca_R6 <- Waseca_Data[[1]]

#append PRS data to master datalist
Growing_Season_2025 <- append(Growing_Season_2025, list(`Rosemount_R6` = `Rosemount_R6`,`Rosemount_Silage` = `Rosemount_Silage`,`Waseca_R6` = `Waseca_R6`))

#remove  in-process variables
rm(list = ls()[!ls() %in% c("Growing_Season_2025","ManureTiming_Loc","Analysis_Loc","Raw_Data_Loc","Plots_Loc","Scripts_Loc")])
