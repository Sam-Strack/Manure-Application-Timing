#Script for sorting, labeling, and cleaning the PRS supply rate data from Western Ag Innovations
#Written By: Sam Strack :)
#Last Updated: 03/30/2025

#load necessary packages
library(tidyverse)
library(scales)

#make arrays for loops
years <- c("2025","2026")
sites <- c("Rosemount", "Waseca")
analytes <- c("NO3-N", "NH4-N", "Ca", "Mg", "K", "P", "Fe", "Mn", "Cu", "Zn", "B", "S", "Pb", "Al", "Cd","Na")

#import data
PRS_Raw_Data <- read.csv("G:/Shared drives/ManureLabTeam/ManureResearch_Shared/Experiments/Timing/Data/Raw_Data/PRS/PRS results for S Strack and M Wilson (project 2591).csv")

#set column labels
PRS_Labels <- as.character(PRS_Raw_Data[4,])
PRS_Labeled <- PRS_Raw_Data %>%
  set_names(PRS_Labels)

#remove unnecessary rows/columns
PRS_Raw_Cleaned <- PRS_Labeled[-c(1:5),-c(1,5,6,7,24,25)]

#extract PRS detection limits, NA values that meet or fall below threshold
PRS_DetectionLimits <- PRS_Labeled[5,8:23]

for (det_lim in analytes) {

  PRS_Raw_Cleaned[[det_lim]][as.numeric(PRS_Raw_Cleaned[[det_lim]]) <= as.numeric(PRS_DetectionLimits[[det_lim]])] <- NA

}

#make new columns for plot, burial and treatment
##Edit here when new burials are analyzed
PRS_split_long <- PRS_Raw_Cleaned %>%
  mutate(Plot = substr(`Sample ID`,6,8),
         Site = case_when(grepl("RROC",`Sample ID`) ~ "Rosemount",
                          grepl("SROC",`Sample ID`) ~ "Waseca"),
         Burial = case_when(grepl("B1",`Sample ID`) ~ "B1",
                            grepl("B2",`Sample ID`) ~ "B2",
                            grepl("B3",`Sample ID`) ~ "B3",
                            grepl("B4",`Sample ID`) ~ "B4"),
                            ##grepl("B5",`Sample ID`) ~ "B5",
                            ##grepl("B6",`Sample ID`) ~ "B6",
                            ##grepl("B7",`Sample ID`) ~ "B7",
                            ##grepl("B8",`Sample ID`) ~ "B8"),
         Treatment = case_when(Plot %in% c(105,202,303,409) ~ "1",
                               Plot %in% c(107,211,301,406) ~ "2",
                               Plot %in% c(101,208,305,402) ~ "3",
                               Plot %in% c(103,201,306,408) ~ "4",
                               Plot %in% c(106,205,309,407) ~ "5",
                               Plot %in% c(102,207,312,401) ~ "6",
                               Plot %in% c(108,204,310,403) ~ "7",
                               Plot %in% c(112,210,307,410) ~ "8",
                               Plot %in% c(110,206,311,412) ~ "9",
                               Plot %in% c(104,209,302,411) ~ "10",
                               Plot %in% c(111,212,308,404) ~ "11",
                               Plot %in% c(109,203,304,405) ~ "12")) %>%
  group_split(Site)

#site loop to make tibbles for each site
for (site in 1 : length(sites)) {

  assign(paste0(sites[[site]]," PRS Data"), PRS_split_long[[site]])

}

#append tibbles to master list
Growing_Season_2025 <- append(Growing_Season_2025, list(`Rosemount PRS Data` = `Rosemount PRS Data`,
                                                        `Waseca PRS Data` = `Waseca PRS Data`))

#print results
print("Appended PRS Data to List")

#remove in process variables, keep specified variables
rm(list = ls()[!ls() %in% c("Growing_Season_2025","ManureTiming_Loc","Analysis_Loc","Raw_Data_Loc","Plots_Loc","Scripts_Loc")])
