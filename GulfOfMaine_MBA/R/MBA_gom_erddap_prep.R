# Preparing the MBA CPR Data from Gulf of Maine Transect


####  Packages  ####
library(tidyverse)
library(targets)
library(readxl)


####  ERDDAP Preparation Workflow:  ####

####  Original Functions:  ####
# Original Source: 13_allspecies_cpr_cleanup.R
# these were the steps taken to import and clean the data to match NOAA data organization

#' 
#' 
#' 
#' 
#' #' @title Create Taxa Keys for CPR Data from the MBA
#' #'
#' #' @param raw_data_folder Folder path to where the MBA/SAHFOS Data is located
#' #' @param mc_option designation of "mc1" or "mc2" to process either batch of data
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' mba_taxa_key <- function(raw_data_folder, mc_option = "mc1"){
#'   
#'   if(mc_option == "mc1"){
#'     
#'     #MC part 1
#'     mc1_phyto   <- readxl::read_xlsx(str_c(raw_data_folder, "MC part1.xlsx"), sheet = 1)
#'     mc1_eye     <- readxl::read_xlsx(str_c(raw_data_folder, "MC part1.xlsx"), sheet = 2)
#'     mc1_trav    <- readxl::read_xlsx(str_c(raw_data_folder, "MC part1.xlsx"), sheet = 3)
#'     
#'     # Combining Taxon Keys for phytoplankton, eye, and trav groups
#'     mc1_t_phyto <- readxl::read_xlsx(str_c(raw_data_folder, "MC part1.xlsx"), skip = 1, sheet = 5) %>%
#'       mutate(taxa_class = "phyto") %>%
#'       mutate(note = NA)
#'     mc1_t_trav  <- readxl::read_xlsx(str_c(raw_data_folder, "MC part1.xlsx"), skip = 1, sheet = 6) %>%
#'       rename(note = 3) %>%
#'       mutate(taxa_class = "trav")
#'     mc1_t_eye   <- readxl::read_xlsx(str_c(raw_data_folder, "MC part1.xlsx"), skip = 1, sheet = 7) %>%
#'       rename(note = 3) %>%
#'       mutate(taxa_class = "eye")
#'     
#'     #Combine Taxa Key
#'     mc1_taxa <- bind_rows(mc1_t_phyto, mc1_t_trav, mc1_t_eye) %>%
#'       arrange(taxa_class, `Taxon Name`) %>%
#'       select(taxa_class, `Taxon Name`, `Accepted ID`, note)
#'     
#'     # tidy up
#'     rm(mc1_t_phyto, mc1_t_trav, mc1_t_eye)
#'     
#'     return(mc1_taxa)
#'   }
#'   
#'   
#'   # MC2 Table Taxa Key
#'   if(mc_option == "mc2"){
#'     
#'     
#'     mc2_phyto <- readxl::read_xlsx(str_c(raw_data_folder, "MC part 2.xlsx"), sheet = 1)
#'     mc2_eye   <- readxl::read_xlsx(str_c(raw_data_folder, "MC part 2.xlsx"), sheet = 2)
#'     mc2_trav  <- readxl::read_xlsx(str_c(raw_data_folder, "MC part 2.xlsx"), sheet = 3)
#'     
#'     # Combining Taxon Keys for phytoplankton, eye, and trav groups
#'     mc2_t_phyto   <- readxl::read_xlsx(str_c(raw_data_folder, "MC part 2.xlsx"),
#'                                        sheet = 4, skip = 1) %>%
#'       select(`Taxon Name` = 2, `Accepted ID` = 1, note = 5) %>%
#'       mutate(taxa_class = "phyto") %>%
#'       filter(is.na(`Taxon Name`) == FALSE)
#'     
#'     mc2_t_trav   <- readxl::read_xlsx(str_c(raw_data_folder, "MC part 2.xlsx"),
#'                                       sheet = 4, skip = 1) %>%
#'       select(`Taxon Name` = 8, `Accepted ID` = 7, note = 10) %>%
#'       mutate(taxa_class = "trav") %>%
#'       filter(is.na(`Taxon Name`) == FALSE)
#'     
#'     mc2_t_eye   <- readxl::read_xlsx(str_c(raw_data_folder, "MC part 2.xlsx"),
#'                                      sheet = 4, skip = 1) %>%
#'       select(`Taxon Name` = 14, `Accepted ID` = 13, note = 16) %>%
#'       mutate(taxa_class = "eye") %>%
#'       filter(is.na(`Taxon Name`) == FALSE)
#'     
#'     mc2_taxa <- bind_rows(mc2_t_phyto, mc2_t_trav, mc2_t_eye) %>%
#'       arrange(taxa_class, `Taxon Name`) %>%
#'       select(taxa_class, `Taxon Name`, `Accepted ID`, note)
#'     
#'     return(mc2_taxa)
#'   }
#'   
#'   
#'   
#'   
#' } #close sahfos_taxa_key
#' 
#' 
#' 
#' 
#' import_mba_mc1 <- function(raw_data_path,
#'                             mc_taxa_key = mc1_taxa, 
#'                            sample_type = c("phyto", "eye", "trav")){
#'   
#'   # excel path
#'   xl_path <- str_c(raw_data_path, "MC part1.xlsx")
#'   
#'   # excel sheet number
#'   sheet_num <- switch(sample_type,
#'                       "phtyo" = 1,
#'                       "eye"   = 2,
#'                       "trav"  = 3)
#'   
#'   # Read the correct sheet of excel file
#'   mc1_data <- readxl::read_xlsx(xl_path, sheet = sheet_num)
#'   
#'   # do cleanup on it
#'   mc1_clean <- mba_reshaping(messy_df = mc1_data,
#'                           taxon_key = mc_taxa_key,
#'                           taxa = sample_type) %>%
#'     rename_all(tolower)
#'   
#'   #prep for export
#'   return(mc1_clean)
#'   
#'   
#' }
#' 
#' 
#' # MC2 Tables
#' import_mba_mc2 <- function(raw_data_path,
#'                            mc_taxa_key = mc2_taxa, 
#'                            sample_type = c("phyto", "eye", "trav")){
#'   
#'   
#'   # excel path
#'   xl_path <- str_c(raw_data_path, "MC part 2.xlsx")
#'   
#'   # excel sheet number
#'   sheet_num <- switch(sample_type,
#'                       "phtyo" = 1,
#'                       "eye"   = 2,
#'                       "trav"  = 3)
#'   
#'   # Read the correct sheet of excel file
#'   mc2_data <- readxl::read_xlsx(xl_path, sheet = sheet_num)
#'   
#'   
#'   # do cleanup on it
#'   mc2_clean <- mba_reshaping(messy_df = mc2_data,
#'                           taxon_key = mc_taxa_key,
#'                           taxa = sample_type) %>%
#'     rename_all(tolower)
#'   
#'   #prep for export
#'   return(mc2_clean)
#'   
#' }
#' 
#' 
#' 
#' 
#' #' @title MBA Data Cleanup
#' #'
#' #' @param messy_df Raw mc1 or mc2 datasheet from SAHFOS/MBA
#' #' @param taxon_key A taxonomic key from the header information, built using mba_taxa_key
#' #' @param taxa string indicating which measurement scale to process, 
#' #' corresponds to the data sheet of the excel file which represent CPR mesurement scales. 
#' #' options are phyto, trav, eye
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' mba_reshaping <- function(messy_df = mc1_phyto, 
#'                        taxon_key = mc1_taxa, 
#'                        taxa = "phyto") {
#'   
#'   #Create new columns to match NOAA data
#'   clean_df <- messy_df %>%
#'     mutate(Cruise = str_extract(Sample_Id, "[^-]*"),
#'            Station = str_extract(Sample_Id, "-.*"),
#'            Station = str_replace(Station, "-", ""),
#'            Day = lubridate::day(Midpoint_Date_Local),
#'            Hour = lubridate::hour(Midpoint_Date_Local),
#'            Minute = lubridate::minute(Midpoint_Date_Local))
#'   
#'   #Phytoplankton sheet has the phytoplankton color index
#'   if(taxa == "phyto"){
#'     
#'     clean_df <- clean_df %>%
#'       select(Sample_Id, Cruise, Station, Year, Month, Day, Hour, Minute,
#'              `Latitude (degrees)` = Latitude, `Longitude (degrees)` = Longitude,
#'              `Phytoplankton Color Index` = Chlorophyll_Index,  everything()) %>%
#'       select(-Midpoint_Date_Local)
#'   } else {
#'     
#'     clean_df <- clean_df %>%
#'       select(Sample_Id, Cruise, Station, Year, Month, Day, Hour, Minute,
#'              `Latitude (degrees)` = Latitude, `Longitude (degrees)` = Longitude, everything()) %>%
#'       select(-Midpoint_Date_Local)
#'   }
#'   
#'   #Pull taxon key for just phytoplankton
#'   tkey <- taxon_key %>% filter(taxa_class == taxa)
#'   
#'   #Swap column names using look up table
#'   data.table::setnames(clean_df, old = as.character(tkey$`Accepted ID`), new = tkey$`Taxon Name`, skip_absent = TRUE)
#'   
#'   return(clean_df)
#' } # Close mba_reshaping



####__________________####


####  Loading Raw Data  ####


# Base path to the raw data folder
raw_path <- "data_raw/SAHFOS-MBA_2013-2017/"

# Testing abundance data loading
phyto_abund <- mba_abundance_dat(raw_path, "phyto")
traverse_abund <- mba_abundance_dat(raw_path, "trav")
eyecount_abund <- mba_abundance_dat(raw_path, "eye")



# Testing Taxa key loading
phyto_key <- extract_mba_key(raw_path, "phyto")
traverse_key <- extract_mba_key(raw_path, "trav")
eyecount_key <- extract_mba_key(raw_path, "eye")


# Testing pivot
phyto_erddap    <- pivot_mba_data(phyto_abund, phyto_key, "phyto")
traverse_erddap <- pivot_mba_data(traverse_abund, traverse_key, "trav")
eyecount_erddap <- pivot_mba_data(eyecount_abund, eyecount_key, "eye")







