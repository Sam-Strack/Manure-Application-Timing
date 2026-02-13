#Script for sorting, labeling, and cleaning the raw weather data from the ROC weather stations. For now,
#daily/cumulative precipitaion, and daily high/low air temps.
#Written By: Sam Strack :)
#Last Updated: 09/09/25

#THERE IS SOMETHING FUNNY WITH THE RROC/SYNOPTIC DATA


#load necessary packages
library(tidyverse)
library(scales)
library(dplyr)
library(lubridate)

#data location for yearly project weather data
setwd(paste0(Raw_Data_Loc,"/Weather"))

#load site weather data into tables based on location in filename
Rosemount_Data <- lapply((list.files(paste0(Raw_Data_Loc,"/Weather"), pattern=".*Rosemount.*\\.csv$", full.names=TRUE)), read.csv)
Waseca_Data <- lapply((list.files(paste0(Raw_Data_Loc,"/Weather"), pattern=".*Waseca.*\\.csv$", full.names=TRUE)), read.csv)

#load naming variables
SampleTimingNames <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")
sites <- c("Rosemount","Waseca")
year <- 2024

#loop for splitting and labeling the weather station data from the ROCs the data sources for the two ROCs
#are formatted very differently
for (site in 1 : as.numeric(length(sites))) {

  if (site == 1) { #Rosemount

    #set sitename naming variable
    sitename <- sites[site]

    #load in raw data from Rosemount station
    Weather_Year_Raw <- Rosemount_Data[[1]]

    #make variable names
    varname <- c("Station_ID", "Date_Time", "Air_Temp","Air_Temp_Max", "Air_Temp_Min", "Precip_Daily")

    #set column names, remove metadata rows, append new date column, remove unnecessary variables, calculate and add cumulative precip column
    yearly_weather <- Weather_Year_Raw %>%
      set_names(varname) %>%
      slice(-1:-11) %>%
      mutate(Date = as.Date(Date_Time), .before = 1) %>%
      mutate_if(is.character, ~replace(., . == "", "0")) %>%
      mutate(Precip_Cum = cumsum(as.numeric(Precip_Daily))) %>%
      select(-Date_Time,-Station_ID,-Air_Temp)

    #make final yearly weather dataframe
    assign(paste0(sites[site],"_Weather_Daily_",year),yearly_weather)

  }

  if (site == 2) { #Waseca

    sitename <- sites[site]

    Weather_Year_Raw <- Waseca_Data[[1]]

    #split yearly data into monthly by row number, extract daily and cumulative precipitation values
    waseca_monthly <- list(
      Jan = Weather_Year_Raw[10:40,],
      Feb = Weather_Year_Raw[58:86,],
      Mar = Weather_Year_Raw[103:133,],
      Apr = Weather_Year_Raw[150:179,],
      May = Weather_Year_Raw[195:225,],
      June = Weather_Year_Raw[242:271,],
      July = Weather_Year_Raw[287:317,],
      Aug = Weather_Year_Raw[332:362,],
      Sept = Weather_Year_Raw[377:406,],
      Oct = Weather_Year_Raw[424:454,],
      Nov = Weather_Year_Raw[471:500,],
      Dec = Weather_Year_Raw[516:546,]
    )

    #start loop for organizing Waseca data. split into dataframes by month, select desired variables, reformat the dates,
    #then make a new dataframe for the whole year
    for (y in 1: as.numeric(length(waseca_monthly))) {

      month <- waseca_monthly[[y]]

      #extract day of month from monthly dataframes, make new vector for dates
      dayofmonth <- na.omit(as.numeric(parse_number(month$X)))
      Date <- make_date(month = y, day = dayofmonth,year = year)

      #add desired weather variables to dataframe
      monthly_weather <- data.frame(Date,Air_Temp_Max = month$X.1, Air_Temp_Min = month$X.2, Precip_Daily = month$X.5)

      #convert non-numeric values to NA, then Zero
      monthly_weather$Precip_Daily <- as.numeric(monthly_weather$Precip_Daily)
      monthly_weather$Precip_Daily[is.na(monthly_weather$Precip_Daily)] <- 0

      #calculate cumulative precip totals, add column to dataframe
      monthly_weather$Precip_Cum <- cumsum(monthly_weather$Precip_Daily)

      #make new monthly dataframe
      assign(paste0("month_",y),monthly_weather)

    }

  #combine monthly dataframes, calculate new cumulative precip values for the whole year
  yearly_weather <- bind_rows(month_1,month_2,month_3,month_4,month_5,month_6,month_7,month_8,month_9,month_10,month_11,month_12)
  yearly_weather$Precip_Cum <- cumsum(yearly_weather$Precip_Daily)

  #make final yearly weather dataframe
  assign(paste0(sites[site],"_Weather_Daily_",year),yearly_weather)

  }

}

#append weather data to master datalist
Growing_Season_2025 <- append(Growing_Season_2025, list(Rosemount_Weather_Daily_2024 = Rosemount_Weather_Daily_2024,
                                                        Waseca_Weather_Daily_2024 = Waseca_Weather_Daily_2024))

#remove in process variables, keep specified variables
rm(list = ls()[!ls() %in% c("Growing_Season_2025","ManureTiming_Loc","Analysis_Loc","Raw_Data_Loc","Plots_Loc","Scripts_Loc")])
