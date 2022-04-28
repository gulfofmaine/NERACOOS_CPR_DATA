# Preparing the NOAA CPR Data from Mid-Atlantic Transect


####  Packages  ####
library(tidyverse)
library(targets)
library(readxl)


####  ERDDAP Preparation Workflow:  ####



####  Support Functions  ####
source(here("R/support/targets_support.R"))




####  Data  ####

raw_path <- "data_raw/NOAA_Mid_Atlantic/"



mid_atlantic_dat <- function(raw_path, sample_type){
    
  # Header Notes:
  # Note 1- Values of -9999 for abundace indicate that a taxa was observed in the sample but not 
  # counted during quantitative analysis
  
  # Note 2- Date for both the Gulf of Maine and Mid Atlantic cpr routes are included. 
  # All data north of 41 degrees are from the GOM route.
  # Note 3- Values of NaN for phytoplankton color index indicate no data.
  
  
  # Switch to the right sheet number
  sheet_number <- switch(sample_type,
                         "zooplankton" = 1,
                         "zoo" = 1,
                         "phytoplankton" = 2,
                         "phyto" = 2)
  
  # Load the data, skkipping the notes:
  mab_raw <- read_xlsx(path = str_c(raw_path,  "MID ATLANTIC CPR DATA.XLSX", sep = "/"), 
                       skip = 5, 
                       sheet = sheet_number)
  
  
}


# Testing abundance data loading
phyto_abund <- mid_atlantic_dat(raw_path, "phyto")
traverse_abund <- mid_atlantic_dat(raw_path, "trav")