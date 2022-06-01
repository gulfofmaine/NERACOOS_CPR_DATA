#####  Making a Single Phytoplankton Timeseries



####  Packages  ####
library(targets)   # For loading data from targets workflow
library(tidyverse) # Data manipulation
library(rerddap)   # Accessing Data using ERDDAP API
library(worrms)    # Interface with WORMS and Aphia ID's

# helper for selecting thins "not in" a vector
`%nin%` <- function(x, table){ is.na(match(x, table, nomatch = NA_integer_))}


####  Data  ####


# Targets:
tar_load("noaa_phyto_erddap")
tar_load("mba_phyto_erddap")


# Shorten names
noaa <- noaa_phyto_erddap
mba <- mba_phyto_erddap
rm(noaa_phyto_erddap, mba_phyto_erddap)




# ERDDAP:





####  Processing  ####

# Units: just go with abundance per m3 that NOAA uses
mba <- mba %>% select(-abundance_per_transect)



# Notes:
# Because they have different ID systems we will replace them with Aphia ID's if possible
# But before that I'll drop the old ones
mba <- mba %>% select(-mba_id)
noaa <- noaa %>% select(-marmap_code)


# Make transect number consistent
mba <- mba %>% mutate(transect_number = as.numeric(transect_number))

# Make PCI consistent
noaa <- noaa %>% mutate(pci = as.numeric(pci))

# Bind them
phyto_all <- bind_rows(list(noaa, mba))



# Check for problem taxa for Aphia ID's
phyto_all %>% distinct(taxon) %>% arrange(taxon) %>%  pull()

# Notes:
# Lot of spp, spp. and things in parentheses
# the spp is redundant, the things in parentheses may need to stay

# First, visually check which would be effected by a string removal of spp
phyto_all %>%
  distinct(taxon) %>% 
  filter(str_detect(taxon, "spp")) %>% 
  pull()


# Perform the changes
# Drop spp and spp.
phyto_all <- phyto_all %>% 
  mutate(
    taxon = str_remove_all(taxon, "spp"),
    taxon = str_remove_all(taxon, "[.]")
  )

# Those would all prevent a successful match to aphia ID's

# Now we are left with things that should match, with the exception of things
# that contain notes like "Total" or taxon in parentheses
phyto_all %>% distinct(taxon) %>% arrange(taxon) %>%  pull()


# Get aphia ID's
phyto_aphia <- phyto_all %>% 
  split(.$taxon) %>% 
  imap_dfr(function(x, y){
    # Attempt to gain Aphia ID using lookup:
    aphia_id = try(
      expr = wm_name2id(name = y), 
      silent = TRUE)
    
    # If unsuccessful fill with NA
    aphia_id <- ifelse(class(aphia_id) == "try-error", NA, aphia_id)
    x_out <- mutate(x, aphia_id = aphia_id, .after = "taxon")
    return(x_out)}) %>% 
  arrange(time, cruise, transect_number, taxon)


# These all failed at matching an Aphia ID:
phyto_aphia %>% filter(is.na(aphia_id)) %>% distinct(taxon) %>% pull()


# This is about as much as I could do without making subjective decisions.
# If you have any questions or want to contribute, feel empowered to do so


# Export
write_csv(phyto_aphia, here::here("erddap_ready/gom_cpr_phytoplankton_full.csv"))
