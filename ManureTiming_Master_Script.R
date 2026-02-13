#This script will process and analyze all of the data from the 2024-2027 Wilson lab Manure Timing experiment
#Written By: Sam Strack :)
#Last Updated: 12/26/2025

#Data processed by this script will be:
  #Inorganic Soil N(ppm)
    #Soil Bulk Density(g/cm3)
  #PRS Probes (ug/10cm2/burial length)
  #Total Plant/Grain N & P (%)
  #Stand Count(plants/10ft), Yield(bu/acre)
  #Watchdog Soil Matric Potential/Temp(kPa/degC)
  #Watchdog Air Temp/Humidity(degC/%)
  #On-Site Weather Station Data (Several Variables)

#------------------------------------------------------------------------------------#
#Relevant file Locations
ManureTiming_Loc <- "G:/Shared drives/ManureLabTeam/ManureResearch_Shared/Experiments/Timing/Data"

Analysis_Loc <- paste0(ManureTiming_Loc,"/r_Analysis")
Raw_Data_Loc <- paste0(ManureTiming_Loc,"/Raw_Data")
Plots_Loc <- paste0(ManureTiming_Loc,"/r_Plots")
Scripts_Loc <- paste0(ManureTiming_Loc,"/r_Scripts")

#------------------------------------------------------------------------------------#
#Data Processing

#Raw data from FIAlab analyzer is assigned treatments, organized by site/sampling time
#cleaned of NAs, and FIAlab cal/check/blank samples
  source(paste0(Scripts_Loc,"/Soil_N_Data_Processing.R"))

#Raw data from PRS Probes is filtered, labelled and organized by site/burial period, cleaned
#of values that do not meet measurement thresholds
  source(paste0(Scripts_Loc,"/PRS_Data_Processing.R"))

#Raw data from RAL Plant/Grain Analysis
  source(paste0(Scripts_Loc,"/Plant_Grain_Data_Processing.R"))

#Raw Data from Watchdog Datalogger
  source(paste0(Scripts_Loc,"/Watchdog_Data_Processing.R"))

#Raw Data from ArcGIS NDVI/NDRE attribute table
  source(paste0(Scripts_Loc,"/NDVI_NDRE_Data_Processing.R"))

#Raw Data from On-Site Weather Stations at ROCs: daily observations are organized by site and year
  source(paste0(Scripts_Loc,"/Weather_Data_Processing.R"))

#------------------------------------------------------------------------------------#
#Analysis

#Two way ANOVA to test effect of manure timing, manure type, and manure timingXtype on all soil/plant variables and yield
  #https://statsandr.com/blog/two-way-anova-in-r/

#Regression of Vegetation Indices vs Treatment/Plot

#Look into stat tests Eduardo used for PRS x Treatment


#------------------------------------------------------------------------------------#
#Plots

#Treatment X Yield/Biomass

#PRS Nutrients across time including Precip





