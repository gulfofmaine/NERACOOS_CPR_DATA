# About:
# This script lists the necessary processing steps
# to work from raw data to the data that was uploaded onto
# ERDDAP. 

# To recreate the workflow simply run:
# targets::tar_make()


# 1. Load packages and set specific options for the workflow
options(tidyverse.quiet = T)
suppressWarnings(library(targets))
suppressPackageStartupMessages(suppressWarnings(library(here)))
suppressWarnings(library(tidyverse))

# 2. Load any additional packages as-needed
tar_option_set(packages = c(
  "targets",
  "here",
  "tidyverse",
  "readxl"
))

# 3. Load Support Functions for Pipeline
source(here("targets_R", "targets_support.R"))


# 4. Define target pipeline: Outlines high-level steps of the analysis
# Format is just a list of all the targets
# Order is not important, package sorts out connections for everything
list(
  
  #### Raw Data from NOAA (1961-2013)  ####
  
  ####__ NOAA Raw Data - All  ####
  tar_target(
    GOM_NOAA_RAW,
    "data_raw/NOAA_1961-2013/Gulf of Maine CPR (Feb 14, 2014 update).xlsx"
  ),
  
  
  #####__ Phytoplankton Data: ####
  # Units: #/cubic meter
  
  # 1. Raw Data
  tar_target(
    phytoplankton_raw,
    separate_measure_scales(raw_file = GOM_NOAA_RAW, "phyto")
  ),
  
  # 2. Marmap Key
  tar_target(
    phyto_key,
    pull_phyto_pieces(phytoplankton_raw, "key")
  ),
  
  # 3. Abundances with MARMAP codes repaired
  tar_target(
    phyto_abundances,
    pull_phyto_pieces(phytoplankton_raw, "abundances")
  ),
  
  # # 4. Instances of duplicate MARMAP codes
  # tar_target(
  #   phyto_duplicates,
  #   pull_phyto_pieces(phytoplankton_raw, "duplicates")
  # ),
  
  # 5. Pivot longer and rejoin header info
  tar_target(
    noaa_phytoplankton_erddap,
    pivot_phyto(phyto_abundances, phyto_key)
  ),
  
  
  
  #####__  Zooplankton Data:   ####
  # Units: #/100 cubic meters
  
  
  # 1. Raw Data
  tar_target(
    zooplankton_raw,
    separate_measure_scales(raw_file = GOM_NOAA_RAW, "zoo")
  ),
  
  # 2. Marmap Key
  tar_target(
    zoo_key,
    pull_zoo_pieces(zooplankton_raw, "key")
  ),
  
  # 3. Abundances with MARMAP codes repaired
  tar_target(
    zoo_abundances,
    pull_zoo_pieces(zooplankton_raw, "abundances")
  ),
  
  # # 4. Instances of duplicate MARMAP codes
  # tar_target(
  #   zoo_duplicates,
  #   pull_zoo_pieces(zooplankton_raw, "duplicates")
  # ),
  
  # 5. Pivot longer and rejoin header info
  tar_target(
    noaa_zooplankton_erddap,
    pivot_zooplankton(zoo_abundances, zoo_key)
  ),
  
  
  ####  Raw Data from SAHFOS/MBA (2013-2017)  ####
  
  ####__MBA Raw Data  ####
  
  ####__ MBA Abundance Data  ####
  tar_target(GOM_MBA_RAW, "data_raw/SAHFOS-MBA_2013-2017/"),
  tar_target(mba_phyto_abund,  mba_abundance_dat(GOM_MBA_RAW, "phyto")),
  tar_target(mba_traverse_abund,  mba_abundance_dat(GOM_MBA_RAW, "trav")),
  tar_target(mba_eyecount_abund,  mba_abundance_dat(GOM_MBA_RAW, "eye")),
  
  ####__ MBA Taxa Keys  ####
  tar_target(mba_phyto_key,  extract_mba_key(GOM_MBA_RAW, "phyto")),
  tar_target(mba_traverse_key,  extract_mba_key(GOM_MBA_RAW, "trav")),
  tar_target(mba_eyecount_key,  extract_mba_key(GOM_MBA_RAW, "eye")),
  
  ####__  Pivot and Rename  ####
  tar_target(mba_phyto_erddap   ,  pivot_mba_data(mba_phyto_abund, mba_phyto_key, "phyto")),
  tar_target(mba_traverse_erddap,  pivot_mba_data(mba_traverse_abund, mba_traverse_key, "trav")),
  tar_target(mba_eyecount_erddap,  pivot_mba_data(mba_eyecount_abund, mba_eyecount_key, "eye")),
  
 
  
  
  ####  Save Data for ERDDAP  ####
  
  # NOAA data
  tar_target(save_noaa_phytoplankton, write_csv(noaa_phytoplankton_erddap, here::here("erddap_data/noaa_gom_cpr_phytoplankton.csv"))),
  tar_target(save_noaa_zooplankton, write_csv(noaa_zooplankton_erddap, here::here("erddap_data/noaa_gom_cpr_zooplankton.csv"))),
  
  # MBA data
  tar_target(save_mba_phytoplankton, write_csv(mba_phyto_erddap, here::here("erddap_data/mba_gom_cpr_phytoplankton.csv"))),
  tar_target(save_mba_traverse, write_csv(mba_traverse_erddap, here::here("erddap_data/mba_gom_cpr_traverse.csv"))),
  tar_target(save_mba_eyecount, write_csv(mba_eyecount_erddap, here::here("erddap_data/mba_gom_cpr_eyecount.csv")))
 
  

  
)
# End of _targets.R