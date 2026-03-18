#Script for sorting, labeling, and cleaning the raw weather data from the ROC weather stations. For now,
#daily/cumulative precipitaion, and daily high/low air temps.
#Written By: Sam Strack :)
#Last Updated:03/17/26

#THERE IS SOMETHING FUNNY WITH THE RROC/SYNOPTIC DATA


#load necessary packages
library(tidyverse)
library(scales)
library(dplyr)
library(lubridate)

#data location for yearly project weather data
setwd(paste0(Raw_Data_Loc,"/Weather"))

#load site weather data into tables based on location in filename
precip <- list(Precip_2024_Data = lapply((list.files(paste0(Raw_Data_Loc,"/Weather"), pattern=".*2024.*\\.csv$", full.names=TRUE)), read.csv),
               Precip_2025_Data = lapply((list.files(paste0(Raw_Data_Loc,"/Weather"), pattern=".*2025.*\\.csv$", full.names=TRUE)), read.csv))

#load naming variables
SampleTimingNames <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")
sites <- c("Rosemount","Waseca")
years <- list("2024","2025")

#loop for year
for (year in 1 : as.numeric(length(years))) {

  weather_year_raw <- precip[[year]]

  #loop for splitting and labeling the weather station data from the ROCs the data sources for the two ROCs
  #are formatted very differently
  for (site in 1 : as.numeric(length(sites))) {

      #set sitename naming variable
      sitename <- sites[site]

      #load in raw data from Rosemount station
      weather_year <- weather_year_raw[[site]]

      #make variable names
      varname <- c("Station_ID", "Date_Time", "Air_Temp","Air_Temp_Max", "Air_Temp_Min", "Precip_Daily")

      #set column names, remove metadata rows, append new date column, remove unnecessary variables, calculate and add cumulative precip column
      yearly_weather <- weather_year %>%
        set_names(varname) %>%
        slice(-1:-11) %>%
        mutate(Date = as.Date(Date_Time), .before = 1) %>%
        mutate_if(is.character, ~replace(., . == "", "0")) %>%
        mutate(Precip_Cum = cumsum(as.numeric(Precip_Daily))) %>%
        select(-Date_Time,-Station_ID,-Air_Temp)

      #make final yearly weather dataframe
      assign(paste0(sites[site],"_Weather_Daily_",years[[year]]),yearly_weather)

  }
}

#append weather data to master datalist
Growing_Season_2025 <- append(Growing_Season_2025, list(Rosemount_Weather_Daily_2024 = Rosemount_Weather_Daily_2024,
                                                        Waseca_Weather_Daily_2024 = Waseca_Weather_Daily_2024,
                                                        Rosemount_Weather_Daily_2025 = Rosemount_Weather_Daily_2025,
                                                        Waseca_Weather_Daily_2025 = Waseca_Weather_Daily_2025))

#remove in process variables, keep specified variables
rm(list = ls()[!ls() %in% c("Growing_Season_2025","ManureTiming_Loc","Analysis_Loc","Raw_Data_Loc","Plots_Loc","Scripts_Loc")])
