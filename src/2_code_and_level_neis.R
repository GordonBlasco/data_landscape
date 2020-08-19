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

nei_final <- fao_species %>% 
  left_join(nei_prep) %>% 
  mutate(excluded = if_else(
    CPC_Class == "Coral and similar products, shells of molluscs, crustaceans or echinoderms and cuttle-bone; live aquatic plants and animals for ornamental purpose", 
    "excluded", "included"
  )) %>% 
  select(SPECIES, id_level, nei_levels, excluded)

write_csv(nei_final, "data/nei_codes.csv")

