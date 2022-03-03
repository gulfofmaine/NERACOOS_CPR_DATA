####  NOAA GOM CPR
#### ERDDAP Prep



####  Packages  ####
library(here)
library(readxl)
suppressPackageStartupMessages(suppressWarnings(library(tidyverse)))



####  Support Functions  ####
source(here("R/support/targets_support.R"))




####  Data  ####



##### Phytoplankton


# load data and the key (header from original format)
# Phytoplankton received from NOAA in units: number per cubic meter for phyto
phyto_abund <- import_noaa_cpr(sample_type = "phyto",  return_option = "abundances")
phyto_marmap_key <- import_noaa_cpr(sample_type = "phyto",  return_option = "key")





# Pivot the table into longer format
phyto_long <- pivot_longer(phyto_abund, 
                           names_to = "taxon name", 
                           values_to = "abundance", 
                           cols = 11:ncol(phyto_abund))


# Join the marmap codes in & prep/format the data
phyto_erd <- phyto_long %>% 
  mutate(date = as.POSIXct(str_c(year, "-", month, "-", day, " ", hour, ":", minute, ":", "00")),
         `taxon name` = str_to_sentence(`taxon name`)) %>% 
  left_join(phyto_marmap_key, by = "taxon name") 

# clean up columns
phyto_erd_clean <- phyto_erd %>% 
  select(cruise, 
         station, 
         date, 
         lat = `latitude (degrees)`, 
         lon = `longitude (degrees)`, 
         pci = `phytoplankton color index`,
         taxon = `taxon name`,
         marmap_code = `marmap code:`,
         abundance = abundance)

# check
glimpse(phyto_erd_clean)

# not bad!
phyto_erd_clean %>% filter(is.na(marmap_code)) %>% distinct(taxon)


# ready!?


##### Traverse (Zooplankton)

# CPR Zooplankton data received from NOAA in units: number per 100 cubic meters for zooplankton

# load data and the key (header from original format)
zoo_abund <- import_noaa_cpr(sample_type = "zoo",  return_option = "abundances")
zoo_key <- import_noaa_cpr(sample_type = "zoo",  return_option = "key")



# Pivot longer
zoo_long <- pivot_longer(zoo_abund, names_to = "taxon name", 
                         values_to = "abundance", 
                         cols = 11:ncol(zoo_abund))

# Split taxon name and stage
zoo_split <- zoo_long %>% 
  separate(`taxon name`, into = c("taxa", "stage"), sep = "[_]", fill = "right", remove = FALSE) %>% 
  mutate(taxa = str_to_title(taxa))


# Prep zoo key names
key_clean <- zoo_key %>% 
  separate(`taxon name`,  into = c("taxa", "stage"), sep = "[_]", fill = "right", remove = TRUE)



# Join marmap codes in, prep the data... key is all jank
zoo_erd <- zoo_split %>% 
  inner_join(key_clean, by = c("taxa", "stage")) 


# Check that the stage matched up
zoo_erd %>% distinct(`taxon name`, taxa, stage)




# reformat columns
zoo_erd_clean <- zoo_erd %>% 
  mutate(date = as.POSIXct(str_c(year, "-", month, "-", day, " ", hour, ":", minute, ":", "00"))) %>% 
  select(cruise, 
         station, 
         date, 
         lat = `latitude (degrees)`, 
         lon = `longitude (degrees)`, 
         pci = `phytoplankton color index`,
         taxa,
         stage,
         marmap_taxa = `marmap taxonomic code:`,
         marmap_stage = `marmap stage code:`,
         abundance = abundance)




####  Exports:
phyto_erd_clean
zoo_erd_clean
# 
# write_csv(phyto_erd_clean, here::here("erddap_data/noaa_gom_phyto.csv"))
# write_csv(zoo_erd_clean, here::here("erddap_data/noaa_gom_traverse.csv"))