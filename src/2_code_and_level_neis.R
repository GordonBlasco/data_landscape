##################################################
## Project: 
## Script purpose: 
## Date:
## Author: Gordon Blasco
##################################################

## Load libraries
##################################################
library(tidyverse)
library(stringr)

## Load raw data from fao
##################################################

source("src/file_names.R") # Sets file directories 

#source("~/github/aquaculture/src/directories.R") # Sets file directories 
fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/TS_FI_PRODUCTION.csv")) # Raw fao data
fao_country <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/CL_FI_COUNTRY_GROUPS.csv")) # Species ref

## make vector of all species code listed in the FAO production table
fao_species_vec <- fao_production %>% 
  distinct(SPECIES) %>% 
  pull(SPECIES)

## filter the FAO species metadata for those that actually appear in the landings data
fao_species <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/CL_FI_SPECIES_GROUPS.csv")) %>% # Species ref
  rename(SPECIES = `3Alpha_Code`) %>% 
  filter(SPECIES %in% fao_species_vec) 



## Filter non-species name observation
##################################################

species_level <- fao_species %>% 
  select(SPECIES, Scientific_Name, Name_En) %>% 
  filter(!(str_detect(Scientific_Name,"^\\w*\\b$"))) %>% # Is only one word long
  filter(!(str_detect(Scientific_Name, " spp$"))) %>% # Ends with spp
  filter(!(str_detect(Scientific_Name,"^\\w*dae\\b"))) %>%  # one word Ends with dae (removes family entries)
  filter(!(str_detect(Scientific_Name,"^Ex "))) %>% # 
  filter(!(SPECIES == "SKH" | SPECIES == "APL")) %>% # misc sharks and aquatic plants
  mutate(id_level = "Species") %>% # left over is a species
  mutate(id_level = case_when(
    SPECIES == "FRZ" ~ "Nei",
    SPECIES == "HKC" ~ "Nei",
    T ~ id_level
  ))


## join back the species observations
fao_species <- fao_species %>%  
  left_join(., species_level) 
## code the rest as Nei's
fao_species$id_level[is.na(fao_species$id_level)] <- "Nei" # everything else is an NEI


## Section: look at taxonkeys
##################################################
tax <- fao_species %>% 
  mutate(length = str_length(Taxonomic_Code),
         x_s = str_count(Taxonomic_Code, "X")) %>% 
  mutate(tax_id = case_when(x_s==0 ~ "species",
                            x_s==2 & length == 10 ~ "genus",
                            x_s==5 & length == 10 ~ "family",
                            x_s==7 & length == 10 ~ "order")) %>% 
  filter(tax_id == "species")


  # left_join(., nei_prep) %>%
  # fitler()
  # select(SPECIES, Taxonomic_Code, id_level, tax_id, nei_levels, length, x_s, Scientific_Name, everything()) %>%
  # filter(tax_id == nei_levels)
  # filter(is.na(tax_id))

#### is in aquaculture? ####
#------------------------------------------------------------------------------#

aq_species <- fao_production %>% 
  filter(SOURCE != 4) %>% 
  distinct(SPECIES) %>% 
  mutate(farmed = "yes")


wc_species <- fao_production %>% 
  filter(SOURCE == 4) %>% 
  distinct(SPECIES) %>% 
  mutate(wild_caught = "yes")



## Break up NEI's by taxonomic resolution
##################################################
nei_genus <- fao_species %>% 
  filter(id_level == "Nei") %>% 
  mutate(nei_levels = case_when(
    str_detect(Scientific_Name, " spp$") ~ "genus",
    str_detect(SPECIES, "DGZ") ~ "genus",
    str_detect(Scientific_Name, pattern = ",..\\.") ~ "mixed species"
  )) 


nei_family <- nei_genus %>% 
  filter(is.na(nei_levels)) %>% 
  mutate(nei_levels = case_when(
    (str_detect(Scientific_Name, "dae$") & !is.na(Family)) ~ "family",
    Scientific_Name == "Gigartinaceae" ~ "family",
    SPECIES == "GRX" ~ "family"
  )) 


nei_major <- nei_family %>% 
  filter(is.na(nei_levels)) %>% 
  mutate(nei_levels = case_when(
    str_detect(Order, "MISCELL") ~ "major group"
  ))


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
  select(SPECIES,nei_levels)

unitless <- fao_production %>% 
  filter(is.na(UNIT)) %>% 
  distinct(SPECIES) %>% 
  left_join(fao_species)

nei_final <- fao_species %>% 
  left_join(nei_prep) %>% 
  mutate(excluded = case_when(
    CPC_Class == "Coral and similar products, shells of molluscs, crustaceans or echinoderms and cuttle-bone; live aquatic plants and animals for ornamental purpose" ~"excluded",
    SPECIES %in% unitless$SPECIES ~ "excluded",
    Major_Group == "MAMMALIA" ~ "excluded",
    Major_Group == "AMPHIBIA, REPTILIA" ~ "excluded",
    T ~ "included")) %>% 
  select(SPECIES, id_level, nei_levels, excluded)



add_aq <- nei_final %>% 
  left_join(aq_species) %>% 
  left_join(wc_species) %>% 
  mutate(farmed_only = if_else(
    farmed== "yes" & is.na(wild_caught), "yes", NA_character_
  ))

#write_csv(add_aq, "data/nei_codes.csv")



test <- add_aq %>% #200
  filter(excluded == "included"&
           id_level == "Species") %>% 
  filter(farmed== "yes" &
           is.na(wild_caught))

test <- add_aq %>% #611 - 465 explicit species
  filter(excluded == "included"&
           id_level == "Species") %>% 
  filter(farmed== "yes")


test <- add_aq %>% #411
  filter(excluded == "included"&
           id_level == "Species") %>% 
  filter(farmed== "yes"&
           wild_caught=="yes") 



ram_codes <- read_csv("data/RAM_to_FAO.csv") %>% 
  distinct(SPECIES) %>% 
  mutate(RAM = "yes")

#ram data
iucn_codes <- read_csv("data/IUCN_to_FAO.csv") %>% 
  distinct(SPECIES) %>% 
  mutate(IUCN = "yes")

## Clean and prep data
################################################################################

spp_info <- add_aq %>%
  left_join(ram_codes) %>% 
  left_join(iucn_codes) %>% 
  mutate(FAO = "yes") %>% 
  left_join(fao_species) %>% 
  filter(RAM == "yes",
         farmed_only == "yes")


