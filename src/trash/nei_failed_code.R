##################################################
## Project: data_landscape
## Script purpose: Get NEI's for species
## Date: 5/12/2020
## Author: Gordon Blasco
##################################################


library(tidyverse)
library(broom)
library(patchwork)
library(ggpubr)
library(rworldmap)
source("~/github/aquaculture/src/directories.R") # Sets file directories 

# load raw data
fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/TS_FI_PRODUCTION.csv")) # Raw fao data
fao_country <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_COUNTRY_GROUPS.csv")) # Species ref
## make vector of all species code listed in the FAO production table
fao_species_vec <- fao_production %>% 
  distinct(SPECIES) %>% 
  pull(SPECIES)
## filter the FAO species metadata for those that actually appear in the landings data
fao_species <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_SPECIES_GROUPS.csv")) %>% # Species ref
  rename(SPECIES = `3Alpha_Code`) %>% 
  filter(SPECIES %in% fao_species_vec) 

## delete later
spp_master <- read_csv(file.path(dir_data_aquaculture, "keys/master_species_key.csv")) %>% # master species key
  select(alpha_code, id_level, isscaap_division)



# Create NEI list
# nei <- fao_production %>% 
#   distinct(SPECIES) %>% 
#   left_join(spp_master, by = c("SPECIES" = "alpha_code")) %>% 
#   left_join(., fao_species, by = c("SPECIES" = "3Alpha_Code")) %>% 
#   filter(id_level == "Nei") 


fao_idlevel <- fao_species %>% 
  #distinct(SPECIES) %>% 
  #left_join(., fao_species, by = c("SPECIES" = "3Alpha_Code")) %>% # there are no NA species names
  select(SPECIES, Scientific_Name, Name_En) %>% 
  filter(!(str_detect(Scientific_Name,"^\\w*\\b$"))) %>% # Is only one word long
  filter(!(str_detect(Scientific_Name, " spp$"))) %>% # Ends with spp
  filter(!(str_detect(Scientific_Name,"^\\w*dae\\b"))) # one word Ends with dae (removes family entries)


SPECIES_idlevels <- fao_idlevel %>% 
  select(SPECIES) %>% 
  mutate(id_level = "Species")


fao_upsides_idlevel <- fao_species %>% 
  distinct(SPECIES) %>% 
  left_join(., SPECIES_idlevels) 

fao_upsides_idlevel$id_level[is.na(fao_upsides_idlevel$id_level)] <- "Neis"





nei_genus <- fao_species %>% 
  filter(id_level == "Nei") %>% 
  mutate(nei_levels = case_when(
    str_detect(Scientific_Name, " spp$") ~ "genus",
    str_detect(SPECIES, "DGZ") ~ "genus",
    str_detect(Scientific_Name, pattern = ",..\\.") ~ "mixed species"
  )) #%>% 
# filter(!is.na(nei_levels))


nei_family <- nei_genus %>% 
  filter(is.na(nei_levels)) %>% 
  mutate(nei_levels = case_when(
    (str_detect(Scientific_Name, "dae$") & !is.na(Family)) ~ "family",
    Scientific_Name == "Gigartinaceae" ~ "family",
    SPECIES == "GRX" ~ "family"
  )) #%>% 
#filter(!is.na(nei_levels))


nei_major <- nei_family %>% 
  filter(is.na(nei_levels)) %>% 
  mutate(nei_levels = case_when(
    str_detect(Order, "MISCELL") ~ "major group"
  )) #%>% 
#filter(!is.na(nei_levels))


nei_order <- nei_major %>% 
  filter(is.na(nei_levels)) %>% 
  mutate(nei_levels = "order")



nei_levels <- nei_genus %>% 
  filter(!is.na(nei_levels)) %>% 
  bind_rows(nei_family) %>% 
  filter(!is.na(nei_levels)) %>% 
  bind_rows(nei_major) %>% 
  filter(!is.na(nei_levels)) %>% 
  bind_rows(nei_order) %>% 
  filter(!is.na(nei_levels)) 


nei_prep <- nei_levels %>% 
  select(SPECIES, id_level, nei_levels)



# missing nei CDE  sp. nov.
# Ex Unionidae	FSH
# TXY SHould be a species hybrid
# TXZ Should be a species hybrid

freshwater <- fao_production %>% 
  distinct(SPECIES) %>% 
  left_join(fao_species, by = c("SPECIES" = "3Alpha_Code")) %>% 
  filter(ISSCAAP_Group == "Miscellaneous freshwater fishes"|
           ISSCAAP_Group == "Freshwater crustaceans"|
           CPC_Class     == "Freshwater fish, live, fresh or chilled"|
           ISSCAAP_Group == "Freshwater molluscs"|
           SPECIES       == "FSH"|
           SPECIES       == "QEX"|
           SPECIES       == "QPD") %>% 
  pull(SPECIES)


excluded_species <- fao_production %>% 
  distinct(SPECIES) %>% 
  left_join(fao_species, by = c("SPECIES" = "3Alpha_Code")) %>% 
  filter(CPC_Class == "Coral and similar products, shells of molluscs, crustaceans or echinoderms and cuttle-bone; live aquatic plants and animals for ornamental purpose") %>% 
  pull(SPECIES)

spp_info <- read_csv(file.path(dir_data_aquaculture, "keys/master_species_key.csv")) %>% 
  select(-id_level) %>% 
  left_join(nei_prep, by = c("alpha_code" = "SPECIES")) %>% 
  mutate(id_level = if_else(is.na(id_level), "Species", "Nei")) %>% 
  mutate(habitat  = if_else((alpha_code %in% freshwater), "freshwater", "marine"))

write_rds(spp_info, "explore/blasco_explore/landscape_paper/species_info.rds")

#rm(list = objects(pattern = "^nei"), new_nei)
