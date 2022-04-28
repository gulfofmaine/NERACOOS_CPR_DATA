# Taxa Keys


# Goal: 
# Over the span of the survey taxa abundances were placed in a number of conflicting/overlapping 
# groupings such that getting a total for specific taxa can sometimes be difficult and unclear

# Examples: Calanus I-III and Calanus II-IV

# To get abundances across these groups, they have been simplified and parsed into groupings 
# that follow early I-IV and late V-VI copepodite stages

# These are different for the data that comes to us from NOAA than the data that comes from SAHFOS
# because the groups present in both sets are not the same.


#### MBA Taxa Key  ####
# Used to resolve inconsistent groups in MBA Gulf of Maine CPR taxonomic groups



# These taxa all need to be renamed slightly to match the naming conventions of the NOAA data:
####  WIP: white names have questionable transfer rationale  ####
mba_key_table <- tribble(
   ~"in_taxa",                             ~"out_taxa",     
   #--------------------------------------|------------------------------
  "acartia spp. (unidentified)",           "acartia spp.",              
  "amphipoda (unidentified)",              "amphipoda spp.",            
  "appendicularia",                        "appendicularia spp.",       
  "bivalvia larvae",                       "bivalvia spp.",             
  "calanus finmarchicus",                  "calanus finmarchicus v-vi", 
  "calanus i-iv",                          "calanus i-iv",              
  "calanus v-vi unidentified",             "calanus spp.",     
  "candacia i-iv",                         "candacia spp.",            #
  "candacia spp. (unidentified)",          "candacia spp.",            #
  "centropages spp. (unidentified)",       "centropages spp.",  
  "copepod eggs",                          "copepoda spp.",            #
  "copepod nauplii",                       "copepoda spp.",            #
  "cumacea",                               "cumacea spp.",        
  "decapod megalopa",                      "decapoda spp.",            #
  "decapod zoea",                          "decapoda spp.",            #
  "decapoda larvae (total)",               "decapoda spp.",            #
  "doliolidae",                            "doliolidae spp.",           
  "euchaetidae (unidentified)",            "euchaeta spp.",             
  "euphausiacea total",                    "euphausiacea spp.",      
  "fish eggs (total)",                     "fish eggs",            #
  "fish eggs with oil globules",           "fish eggs",            #
  "fish eggs without oil globules",        "fish eggs",            #
  "foraminifera (total)",                  "foraminifera spp.",         
  "gammaridea",                            "gammaridea spp.",           
  "gastropoda (unidentified)",             "gastropoda spp.",           
  "gymnosomata (unidentified)",            "gymnosomoata spp.",         
  "harpacticoida total traverse",          "harpacticoida spp.",        
  "hyperiidea (total)",                    "hyperiidea spp.",           
  "ischnocalanus",                         "ischnocalanus spp.",        
  "lepas nauplii",                         "lepas spp.",                
  "metridia spp. (v-vi) (unidentified)",   "metridia spp.",             
  "monstrilloida",                         "monstrilloida spp.",        
  "ostracoda",                             "ostracoda spp.",            
  "pleuromamma spp. (unidentified)",       "pleuromamma spp.",          
  "polychaete larvae (unidentified)",      "polychaeta larva",          
  "pseudocalanus spp. adult atlantic",     "pseudocalanus spp.",            #
  "pseudocalanus spp. adult total",        "pseudocalanus spp.",            #
  "radiolaria non-acantharian",            "radiolaria spp.",            #
  "radiolaria total",                      "radiolaria spp.",            #
  "salpidae (total)",                      "salpidae spp.",             
  "siphonostomatoida",                     "siphonostomatoida spp.",    
  "thecosomata (north atlantic)",          "thecosomata spp.",          
  "tintinnida total",                      "tintinnidae spp."          
)


# These taxa occurred in instances where many columns became one,
# these were then set to NULL in the original workup
null_taxa <- c(
  "candacia i-iv",
  "candacia spp. (unidentified)",
  "copepod eggs",
  "copepod nauplii",
  "decapod megalopa",
  "decapod zoea",
  "decapoda larvae (total)",
  "fish eggs (total)",
  "fish eggs with oil globules",
  "fish eggs without oil globules",
  "pseudocalanus spp. adult atlantic",
  "pseudocalanus spp. adult total",
  "radiolaria non-acantharian",
  "radiolaria total"
)




####  Exporting Taxa Key  ####

# save the list to load in the R function
saveRDS(mba_key_table, file = here::here("GulfOfMaine_MBA/taxa_resolving/mba_gulfofmaine_taxa_key.rds"))

# Save the csv file for checking what happened
write_csv(mba_key_table, here::here("GulfOfMaine_MBA/taxa_resolving/mba_gulfofmaine_taxa_key.csv"))
