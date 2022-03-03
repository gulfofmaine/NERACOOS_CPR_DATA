# Taxa Keys


# Goal: 
# Over the span of the survey taxa abundances were placed in a number of conflicting/overlapping 
# groupings such that getting a total for specific taxa can sometimes be difficult and unclear

# Examples: Calanus I-III and Calanus II-IV

# To get abundances across these groups, they have been simplified and parsed into groupings 
# that follow early I-IV and late V-VI copepodite stages

# These are different for the data that comes to us from NOAA than the data that comes from SAHFOS
# because the groups present in both sets are not the same.


#### NOAA Taxa Key  ####
# Used to resolve inconsistent groups in noaa GOM taxonomic groups
noaa_gom_taxa_key <- list( 
  `acartia danae`                       = c("acartia danae_copepodite ii-vi"),
  
  `acartia longiremis`                  = c("acartia longiremis_copepodite ii-vi", 
                                            "acartia longiremis_unstaged"),
  
  `acartia spp.`                        = c("acartia_copepodite i-v (pseudocalanus / copepodite i-vi (paracalanus)", 
                                            "acartia_copepodite ii-vi"),
  
  `amphipoda spp.`                      =  c("amphipoda_unstaged"),
  
  `appendicularia spp.`                 =  c("appendicularia_unstaged"),
  
  `bivalvia spp.`                       =  c("bivalvia_larva"),
  
  `brachyura spp.`                      = c("bivalvia_larva",                                                                            
                                            "brachyura_nauplius"),
  
  `calanus i-iv`                        = c("calanus_copepodite i-iv"),
  
  `calanus finmarchicus v-vi`           = c("calanus finmarchicus_copepodite v-vi"),
  
  `calanus helgolandicus`               = c("calanus helgolandicus_copepodite v-vi"),
  
  `calanus hyperboreus`                 = c("calanus hyperboreus_copepodite i-vi",
                                            "calanus hyperboreus_copepodite iii-vi"),
  
  `calocalanus spp.`                    = c("calocalanus_copepodite i-iv",                                                               
                                            "calocalanus_copepodite v-vi"),
  
  `candacia spp.`                       = c("candacia_copepodite iii-iv"),
  
  `cavoliniidae spp.`                   = c("cavoliniidae_unstaged"),
  
  `centropages hamatus`                 = c("centropages hamatus_copepodite iv-vi",                                                      
                                            "centropages hamatus_copepodite vi",                                                         
                                            "centropages hamatus_unstaged"),
  
  `centropages typicus`                 = c("centropages typicus_copepodite iv",                                                         
                                            "centropages typicus_copepodite iv-v",                                                       
                                            "centropages typicus_copepodite iv-vi",                                                      
                                            "centropages typicus_copepodite vi" ),
  
  `centropages spp.`                    = c("centropages_copepodite i-iv",                                                               
                                            "centropages_copepodite v-vi"),
  
  `chaetognatha eyecount`               = c("chaetognatha hpr eyecount_unstaged"),
  
  `chaetognatha traverse`               = c("chaetognatha hpr traverse_unstaged"),
  
  `cladocera spp.`                      = c("cladocera_unstaged"),
  
  `clausocalanus spp.`                  = c("clausocalanus_copepodite vi"),
  
  `clione spp.`                         = c("clione_immature (sexually, or juvenile)",                                                   
                                            "clione_unstaged" ),
  
  `copepoda spp.`                       = c("copepoda_copepodite i-v",                                                                   
                                            "copepoda_nauplius"),
  
  `cumacea spp.`                        = c("cumacea_unstaged"),
  
  `cyclopoida spp.`                     = c("cyclopoida_copepodite vi"),
  
  `decapoda spp.`                       = c("decapoda_larva"),
  
  `echinoderm larvae`                   = c("echinodermata_larva"),
  
  `ectoprocta cyphonautes`              = c("ectoprocta_cyphonautes"),
  
  `eucalanus spp.`                      = c("eucalanus_copepodite i-vi"),
  
  `euchaeta acuta`                      = c("euchaeta acuta_copepodite v-vi",                                                            
                                            "euchaeta acuta_unstaged"),
  `euchaeta marina`                     = c("euchaeta marina_copepodite i-iv",                                                           
                                            "euchaeta marina_copepodite v-vi",                                                           
                                            "euchaeta marina_unstaged" ),
  
  `euchaeta spp.`                       = c("euchaeta_copepodite vi"),
  
  `euchirella rostrata`                 = c("euchirella rostrata_copepodite v-vi"),
  
  `euphausiacea calyptosis`             = c("euphausiacea_calyptopis (protozoea)"),
  
  `euphausiacea nauplii`                = c("euphausiacea_nauplius"),
  
  `euphausiacea spp.`                  = c("euphausiacea_post calyptopis"),
  
  `eurytemora americana`                = c("eurytemora americana_copepodite vi",                                                        
                                            "eurytemora americana_unstaged"),
  
  `eurytemora herdmani`                 = c("eurytemora herdmani_unstaged"),
  
  `eurytemora spp.`                     = c("eurytemora_copepodite i-iv",                                                                
                                            "eurytemora_copepodite v-vi",                                                                
                                            "eurytemora_unstaged"),
  
  `evadne spp.`                         = c("evadne_unstaged"),
  
  `foraminiferida spp.`                 = c("foraminiferida_unstaged"),
  
  `gammaridea spp.`                     = c("gammaridae_unstaged"),
  
  `gastropoda spp.`                     = c("gastropoda_larva",                                                                          
                                            "gastropoda_unstaged" ),
  
  `gymnosomata spp.`                    = c("gymnosomata_unstaged"),
  
  `halithalestris spp.`                 = c("halithalestris croni_unstaged"),
  
  `harpacticoida spp.`                  = c("harpacticoida_copepodite i-vi"),
  
  `heteropoda spp.`                     = c("heteropoda"),
  
  `heterorhabdus papilliger`            = c("heterorhabdus papilliger_unstaged"),
  
  `homarus americanus`                  = c("homarus americanus_larva",                                                                  
                                            "homarus americanus_unstaged"),
  
  `hyperiidea spp.`                          = c("hyperiidea_unstaged"),
  
  `isopoda spp.`                        = c("isopoda_larva",                                                                             
                                            "isopoda_unstaged"),
  
  `labidocera aestiva`                  = c("labidocera aestiva_copepodite vi"),
  
  `limacina spp.`                       = c("limacina_unstaged"),
  
  `lucifer typus`                       = c("lucifer typus_immature (sexually), juvenile, or adult"),
  
  `macrosetella gracilis`               = c("macrosetella gracilis_copepodite i-vi"),
  
  `mecynocera clausi`                   = c("mecynocera clausi_copepodite v-vi"),
  
  `metridia longa`                      = c("metridia longa_copepodite v-vi"),
  
  `metridia lucens`                     = c("metridia lucens_copepodite v-vi",                                                           
                                            "metridia lucens_unstaged"),
  
  `metridia i-iv`                       = c("metridia_copepodite i-iv"),
  
  `microsetella rosea`                  = c("microsetella rosea_copepodite i-iv",                                                        
                                            "microsetella rosea_unstaged"),
  
  `musca spp.`                          = c("musca_unstaged"),
  
  `mysida spp.`                         = c("mysida_unstaged"),
  
  `nannocalanus minor`                  = c("nannocalanus minor_copepodite v",                                                           
                                            "nannocalanus minor_copepodite v-vi",                                                        
                                            "nannocalanus minor_copepodite vi"),
  
  `nemata spp.`                         = c("nemata_unstaged"),
  
  `oithona spp.`                        = c("oithona_copepodite iv-vi"),
  
  `oncaea spp.`                         = c("oncaea_copepodite vi"),
  
  `ostracoda spp.`                      = c("ostracoda_immature (sexually), juvenile, or adult",
                                            "ostracoda_unstaged"),
  
  `paedoclione doliiformis`             = c("paedoclione doliiformis_unstaged"),
  
  `para-pseudocalanus spp.`             = c("paracalanus or pseudocalanus_copepodite i-v",
                                            "paracalanus or pseudocalanus_copepodite i-v (pseudocalanus / copepodite i-vi (paracalanus)"),
  
  `paracalanus spp.`                    = c("paracalanus_copepodite vi",                                                                 
                                            "paracalanus_unstaged" ),
  
  `paraeuchaeta norvegica`              = c("paraeuchaeta norvegica_copepodite iii-vi",                                                  
                                            "paraeuchaeta norvegica_nauplius" ),
  
  `penilia avirostris`                  = c("penilia avirostris_unstaged"),
  
  `pleuromamma piseki`                  = c("pleuromamma piseki_copepodite i-vi",                                                        
                                            "pleuromamma piseki_copepodite v-vi"),
  
  `pleuromamma robusta`                 = c("pleuromamma robusta_copepodite v-vi"),
  
  `pleuromamma spp.`                    = c("pleuromamma_copepodite ii-vi",                                                              
                                            "pleuromamma_copepodite iv-v",                                                               
                                            "pleuromamma_copepodite vi",                                                                 
                                            "pleuromamma_unstaged"),
  
  `pneumodermopsis paucidens`           = c("pneumodermopsis paucidens_unstaged"),
  
  `podon spp.`                          = c("podon_unstaged"),
  
  `polychaeta larva`                    = c("polychaeta_larva"),
  
  `pseudocalanus spp.`                  = c("pseudocalanus_copepodite vi"),
  
  `pycnogonida spp.`                    = c("pycnogonida_unstaged"),
  
  `rhincalanus nasutus`                 = c("rhincalanus nasutus_copepodite v-vi",                                                       
                                            "rhincalanus nasutus_unstaged"),
  
  `rhincalanus spp.`                    = c("rhincalanus_copepodite v-vi",                                                               
                                            "rhincalanus_copepodite vi",                                                                 
                                            "rhincalanus_unstaged"),
  
  `sapphirina spp.`                     = c("sapphirina_copepodite i-vi"),
  
  `sarcodina spp.`                      = c("sarcodina (not foraminiferida)_unstaged"),
  
  `sessilia spp.`                       = c("sessilia_cypris", 
                                            "sessilia_nauplius"),
  
  `siphonostomoida spp.`                = c("siphonostomatoida_unstaged"),
  
  `sipuncula spp.`                      = c("sipuncula_unstaged"),
  
  `stellate bodies`                     = c("stellate bodies_unstaged"),
  
  `stomatopoda spp.`                    = c("stomatopoda_larva"),
  
  `temora longicornis`                  = c("temora longicornis_copepodite iv-vi",                                                       
                                            "temora longicornis_unstaged"),
  
  `temora turbinata`                    = c("temora turbinata_copepodite iv-vi"), 
  
  `thalia democratica`                  = c("thalia democratica_unstaged"),
  
  `thaliacea spp.`                      = c("thaliacea_unstaged"),
  
  `thecosomata spp.`                    = c("thecosomata_unstaged"), 
  
  `tintinnidae spp.`                    = c("tintinnidae_unstaged"),
  
  `tortanus discaudatus`                = c("tortanus discaudatus_copepodite v-vi",                                                      
                                            "tortanus discaudatus_copepodite vi"),
  
  `undeuchaeta plumosa`                 = c("undeuchaeta plumosa_copepodite v-vi"),
  
  `unidentified plankton and fragments` = c("unidentified plankton and fragments_unstaged")
)
####  End NOAA Key  ####



# Reformat as big table which is easier to check for non R-users:
key_table <- noaa_gom_taxa_key %>% 
  map(~ as_tibble(.x) %>% setNames("in_taxa")) %>% 
  bind_rows(.id = "out_taxa") %>% 
  select(in_taxa, out_taxa)


####  Exporting Taxa Key  ####

# save the list to load in the R function
saveRDS(noaa_gom_taxa_key, file = here::here("data_processed/noaa_gulfofmaine_taxa_key.rds"))

# Save the csv file for checking what happened
write_csv(key_table, here::here("data_processed/noaa_gulfofmaine_taxa_key.csv"))
