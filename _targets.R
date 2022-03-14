


# Load packages and set specific options for the workflow
options(tidyverse.quiet = T)
suppressWarnings(library(targets))
suppressWarnings(library(tarchetypes))
suppressPackageStartupMessages(suppressWarnings(library(here)))
suppressWarnings(library(tidyverse))
suppressWarnings(library(readxl))


# Support Functions for Pipeline
source(here("R", "support/targets_support.R"))


# Define target pipeline: Outlines high-level steps of the analysis
# Format is just a list of all the targets
# Order is not important, package sorts out connections for everything
list(
  
  #### Raw Data from NOAA (1961-2013)  ####
  
  #####__ Phytoplankton Data: ####
  # Units: #/cubic meter
  
  # 1. Raw Data
  tar_target(
    phytoplankton_raw,
    noaa_cpr_raw("phyto")
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
    erddap_phytoplankton,
    pivot_phyto(phyto_abundances, phyto_key)
  ),
  
  
  
  #####__  Zooplankton Data:   ####
  # Units: #/100 cubic meters
  
  
  # 1. Raw Data
  tar_target(
    zooplankton_raw,
    noaa_cpr_raw("zoo")
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
    erddap_zooplankton,
    pivot_zooplankton(zoo_abundances, zoo_key)
  )
  
  
  ####  Handling Raw Data from SAHFOS (2013-2017)
  
  
)
# End of _targets.R