
####____________________####
####  Setup:  ####
# About:
# This script lists the necessary processing steps
# to work from raw data to the data that was uploaded onto
# ERDDAP. 

# To recreate the workflow simply run:
# targets::tar_make()

##### Packages:  ####

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



##### Target Functions:  ####

# 3. Load Support Functions for Pipeline
source(here("targets_R", "targets_support.R"))


##### Target Pipeline:  ####

# 4. Define target pipeline: Outlines high-level steps of the analysis
# Format is just a list of all the targets
# Order is not important, package sorts out connections for everything
list(
  
  
  
  
  ####____________________####
  #### Data from NOAA (1961-2013)  ####
  
  ######__ noaa_start_dat  ####
  # NOAA Raw Data - All
  tar_target(
    noaa_start_dat,
    "data_raw/NOAA_1961-2013/Gulf of Maine CPR (Feb 14, 2014 update).xlsx"
  ),
  
  
  #### Phytoplankton: ####
  # Units: #/cubic meter
  
  # 1. Raw Data
  ######__ phyto_raw  ####
  tar_target(
    phyto_raw,
    separate_measure_scales(raw_file = noaa_start_dat, 
                            sample_type = "phyto")),
  
  # 2. Marmap Key
  ######__ phyto_key  ####
  tar_target(
    phyto_key,
    pull_phyto_pieces(phyto_raw = phyto_raw, 
                      return_option = "key")),
  
  # 3. Abundances with MARMAP codes repaired
  ######__ phyto_abund  ####
  tar_target(
    phyto_abund,
    pull_phyto_pieces(phyto_raw = phyto_raw,
                      return_option =  "abundances")),
  
  # # 4. Instances of duplicate MARMAP codes
  # tar_target(
  #   phyto_duplicates,
  #   pull_phyto_pieces(phyto_raw, "duplicates")
  # ),
  
  # 5. Pivot longer and rejoin header info
  ######__ noaa_phyto_erddap  ####
  tar_target(
    noaa_phyto_erddap,
    pivot_phyto(phyto_abund = phyto_abund, 
                phyto_key = phyto_key)
  ),
  
  
  
  #####  Zooplankton:   ####
  # Units: #/100 cubic meters


  # 1. Raw Data
  ######__ zp_raw  ####
  tar_target(
    zp_raw,
    separate_measure_scales(raw_file = noaa_start_dat,
                            sample_type =  "zoo")
  ),

  # 2. Marmap Key
  ######__ zp_key  ####
  tar_target(
    zp_key,
    pull_zoo_pieces(noaa_zoo = zp_raw,
                    return_option =  "key")
  ),

  # 3. Abundances with MARMAP codes repaired
  ######__ zp_abund  ####
  tar_target(
    zp_abund,
    pull_zoo_pieces(zp_raw, "abundances")
  ),

  # # 4. Instances of duplicate MARMAP codes
  # tar_target(
  #   zp_duplicates,
  #   pull_zoo_pieces(zp_raw, "duplicates")
  # ),

  # 5. Pivot longer and rejoin header info
  ######__ noaa_zp_erddap  ####
  tar_target(
    noaa_zp_erddap,
    pivot_zooplankton(zp_abund, zp_key)
  ),
  
  
  
  
  
  ####____________________####
  ####  Data from MBA (2013-2017)  ####
  
  
  #####__ mba_start_dat  ####
  tar_target(mba_start_dat, "data_raw/SAHFOS-MBA_2013-2017/"),
  
  
  
  ####  Phytoplankton:  ####
  
  # 1. Pull Abundance Data
  #####__ mba_phyto_abund  ####
  tar_target(mba_phyto_abund,    mba_abundance_dat(mba_start_dat, "phyto")),

  # 2. Get Column/Taxa Key
  #####__ mba_phyto_key  ####
  tar_target(mba_phyto_key,  extract_mba_key(mba_start_dat, "phyto")),

  # 3. Pivot to long-format
  #####__ mba_phyto_long  ####
  tar_target(mba_phyto_long   ,  pivot_mba_data(mba_phyto_abund, mba_phyto_key, "phyto")),

  # 4. Perform Unit Conversion
  #####__ mba_phyto_erddap  ####
  tar_target(mba_phyto_erddap,  mutate(mba_phyto_long, abundance = transect_to_m3(abundance_per_transect, "meters cubed"))),


  ####  Traverse Zooplankton:  ####

  # 1. Pull Abundance Data
  #####__ mba_zpt_abund  ####
  tar_target(mba_zpt_abund, mba_abundance_dat(mba_start_dat, "trav")),

  # 2. Get Column/Taxa Key
  #####__ mba_zpt_key  ####
  tar_target(mba_zpt_key,  extract_mba_key(mba_start_dat, "trav")),

  # 3. Pivot to long-format
  #####__ mba_zpt_long  ####
  tar_target(mba_zpt_long,  pivot_mba_data(mba_zpt_abund, mba_zpt_key, "trav")),

  # 4. Perform Unit Conversion
  #####__ mba_zpt_100m  ####
  tar_target(mba_zpt_100m, mutate(mba_zpt_long, abundance = transect_to_m3(abundance_per_transect, "100 meters cubed"))),

  # 5. Add PCI to Zooplankton
  #####__ mba_zpt_erddap  ####
  tar_target(mba_zpt_erddap,
             supplement_PCI(phyto_dat = mba_phyto_erddap,
                            zooplankton_dat = mba_zpt_100m)),


  ####  Eyecount Zooplankton:  ####

  # 1. Pull Abundance Data
  #####__ mba_zpe_abund  ####
  tar_target(mba_zpe_abund, mba_abundance_dat(mba_start_dat, "eye")),

  # 2. Get Column/Taxa Key
  #####__ mba_zpe_key  ####
  tar_target(mba_zpe_key,  extract_mba_key(mba_start_dat, "eye")),

  # 3. Pivot to long-format
  #####__ mba_zpe_long  ####
  tar_target(mba_zpe_long,  pivot_mba_data(mba_zpe_abund, mba_zpe_key, "eye")),

  # 4. Perform Unit Conversion
  #####__ mba_zpe_100m  ####
  tar_target(mba_zpe_100m, mutate(mba_zpe_long, abundance = transect_to_m3(abundance_per_transect, "100 meters cubed"))),

  # 5. Add PCI to Zooplankton
  #####__ mba_zpt_erddap  ####
  tar_target(mba_zpe_erddap,
             supplement_PCI(phyto_dat = mba_phyto_erddap,
                            zooplankton_dat = mba_zpe_100m)),
  
  
  ####____________________####
  ####  Save Data for ERDDAP  ####
  
  # Both xml and CSV's sent to Box for upload to s3:
  tar_target(
    export_all,
    targets_tobox(
      noaa_phyto = noaa_phyto_erddap,
      noaa_zoo   = noaa_zp_erddap,
      mba_phyto  = mba_phyto_erddap,
      mba_trav   = mba_zpt_erddap,
      mba_eye    = mba_zpe_erddap)
  )
  
  
  # # Saving to erddap_ready folder:
  # # NOAA data
  # tar_target(save_noaa_phyto,
  #            write_csv(noaa_phyto_erddap, here::here("erddap_ready/noaa_gom_cpr_phytoplankton.csv"))),
  # tar_target(save_noaa_zp,
  #            write_csv(noaa_zp_erddap, here::here("erddap_ready/noaa_gom_cpr_zooplankton.csv"))),
  # 
  # # MBA data
  # tar_target(save_mba_phyto,
  #            write_csv(mba_phyto_erddap, here::here("erddap_ready/mba_gom_cpr_phytoplankton.csv"))),
  # tar_target(save_mba_zpt,
  #            write_csv(mba_zpt_erddap, here::here("erddap_ready/mba_gom_cpr_traverse.csv"))),
  # tar_target(save_mba_zpe,
  #            write_csv(mba_zpe_erddap, here::here("erddap_ready/mba_gom_cpr_eyecount.csv")))
  
  
  
  
  
  
  ####____________________####
  #### Raw Data from NOAA Mid Atlantic  ####
  
  
  
  
  
  
  
  
  
  
  
  
  
)
# End of _targets.R