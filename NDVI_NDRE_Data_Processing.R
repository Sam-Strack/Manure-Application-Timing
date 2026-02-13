#Script for loading in and organizing remote sensing data from ManureTiming Experiment,
#Written By: Sam Strack :)
#Last Updated: 12/26/2025

#load necessary packages
library(tidyverse)
library(scales)
library(dplyr)

#data location for yearly project PRS data
setwd(paste0(Raw_Data_Loc,"/NDVI_NDRE"))

#load plant data into tables based on location in filename
Rosemount_Data <- lapply((list.files(paste0(Raw_Data_Loc,"/NDVI_NDRE"), pattern=".*RROC.*\\.csv$", full.names=TRUE)), read.csv)
Waseca_Data <- lapply((list.files(paste0(Raw_Data_Loc,"/NDVI_NDRE"), pattern=".*SROC.*\\.csv$", full.names=TRUE)), read.csv)

#extract date of images from filename
Rosemount_Dates <- substr((basename(list.files(paste0(Raw_Data_Loc,"/NDVI_NDRE"), pattern=".*RROC.*\\.csv$", full.names=TRUE))),26,35 )
Waseca_Dates <- substr((basename(list.files(paste0(Raw_Data_Loc,"/NDVI_NDRE"), pattern=".*SROC.*\\.csv$", full.names=TRUE))),26,35 )

#start loop to extract RS info from every day of imaging based on dates in filenames in folder
#for
Rosemount_NDVI_NDRE <- Rosemount_Data[[1]]
Waseca_NVDI_NDRE <- Waseca_Data[[1]]

#append NDVI_NDRE data to master datalist
Growing_Season_2025 <- append(Growing_Season_2025, list(Rosemount_NDVI_NDRE = Rosemount_NDVI_NDRE,
                                                        Waseca_NVDI_NDRE = Waseca_NVDI_NDRE))
#remove  in-process variables
rm(list = ls()[!ls() %in% c("Growing_Season_2025","ManureTiming_Loc","Analysis_Loc","Raw_Data_Loc","Plots_Loc","Scripts_Loc")])
