#Script for sorting labeling and cleaning data from Spectrum Watchdog dataloggers.
#Air Temp, Relative Humidity, Dew Point, Soil Temp, Soil moisture
#Written By: Sam Strack :)
#Last Updated: 12/26/25

#load necessary packages
library(tidyverse)
library(scales)
library(dplyr)
library(lubridate)

#data location for yearly project weather data
setwd(paste0(Raw_Data_Loc,"/Watchdog"))

#load naming variables
sites <- c("Rosemount","Waseca")
year <- 2025

#load site watchdog data into tables based on site location in filename
Rosemount_Data <- lapply((list.files(paste0(Raw_Data_Loc,"/Watchdog"), pattern=".*Rosemount.*\\.TXT$", full.names=TRUE)), read.delim)
Waseca_Data <- lapply((list.files(paste0(Raw_Data_Loc,"/Watchdog"), pattern=".*Waseca.*\\.TXT$", full.names=TRUE)), read.delim)
Data_list <- list(Rosemount_Data = Rosemount_Data,Waseca_Data = Waseca_Data)

#logger install/removal dates to bracket data(there is a 5hr offset for some reason)
Rosemount_Dates <- c("2025-05-15 19:00:00","2025-08-17 19:00:00")
Waseca_Dates <- c("2025-05-15 19:00:00","2025-08-03 19:00:00")
Dates_list <- list(Rosemount_Dates = Rosemount_Dates, Waseca_Dates = Waseca_Dates)


#site loop begins here
for (x in 1 : as.numeric(length(sites))) {
  Watchdog_Raw <- Data_list[[x]][[1]]

  #set date column to datetime
  Watchdog_Raw$Date.and.Time <- ymd_hm(Watchdog_Raw$Date.and.Time)

  #only use data from when the station was installed
  Watchdog_Filtered <- Watchdog_Raw %>%
    filter(Date.and.Time >= Dates_list[[x]][[1]] & Date.and.Time <= Dates_list[[x]][[2]])

  #extract the first day in the dataframe
  day1 <- (Watchdog_Filtered$Date.and.Time[1])

  #day loop begins here. make dataframe for each day, taking hourly averages for each measurement
  for (y in 1: as.numeric(Watchdog_Filtered$Date.and.Time[length(Watchdog_Filtered$Date.and.Time)] - Watchdog_Filtered$Date.and.Time[1])) {

    #bracket each day based on distance from first day
    day_start <- day1 + days(y-1)
    day_end <- day1 + days(y)

    #extract day
    Watchdog_Daily <- Watchdog_Filtered %>%
      filter(Date.and.Time >= day_start & Date.and.Time < day_end )

    #Change 15min measurements into hourly averages for whole day
    Watchdog_Hourly_Avg <- Watchdog_Daily %>%
      mutate(Time = floor_date(Date.and.Time, "hour")) %>%
      group_by(Time) %>%
      summarize(Soil_Temp_6 = mean(TMPA, na.rm = TRUE),
                Temp_12 = mean(TMPB, na.rm = TRUE),
                Soil_Matric_Pot_6 = mean(SMSC, na.rm = TRUE),
                Soil_Matric_Pot_12 = mean(SMSD, na.rm = TRUE),
                Air_Temp = mean(TMP, na.rm = TRUE),
                RH = mean(HMD, na.rm = TRUE),
                Dew_Point = mean(DEW, na.rm = TRUE))

    #make tibble for each day with names based on date
    assign(paste0(sites[x],"_Watchdog_Data_",day_start),Watchdog_Hourly_Avg)

  }

  #make list of daily tibbles based on site name
  daily_list <- Filter(is_tibble, mget(ls(pattern = sites[x])))
  assign(paste0(sites[x],"_Watchdog_Daily_",year),daily_list)

  #combine tibbles into one large tibble
  watchdog_combined <- bind_rows(daily_list)
  assign(paste0(sites[x],"_Watchdog_Combined_",year),watchdog_combined)

  #remove in process variables, keep specified variables
  rm(list = ls()[!ls() %in% c("Growing_Season_2025","ManureTiming_Loc","Analysis_Loc","Raw_Data_Loc","Plots_Loc","Scripts_Loc",
                              "sites","year","Data_list","Dates_list","x",
                              "Rosemount_Watchdog_Daily_2025","Rosemount_Watchdog_Combined_2025",
                              "Waseca_Watchdog_Daily_2025","Waseca_Watchdog_Combined_2025")])

}

#append watchdog data to master datalist ###Edit here to include combined tibble as well as daily list
Growing_Season_2025 <- append(Growing_Season_2025, list(Rosemount_Watchdog_Daily_2025 = Rosemount_Watchdog_Daily_2025,
                                                        Waseca_Watchdog_Daily_2025 = Waseca_Watchdog_Daily_2025))

#remove in process variables, keep specified variables
rm(list = ls()[!ls() %in% c("Growing_Season_2025","ManureTiming_Loc","Analysis_Loc","Raw_Data_Loc","Plots_Loc","Scripts_Loc")])
