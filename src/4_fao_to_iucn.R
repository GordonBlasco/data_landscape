##################################################
## Project: Data Landscape Paper
## Script purpose: Link FAO codes to RAM names
## Date: 5/27/2020
## Author: Gordon Blasco
##################################################
library(tidyverse)
library(rredlist)
library(furrr)

apikey = Sys.getenv("IUCN_API_KEY")

source("src/file_names.R") # Sets file directories 
fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/TS_FI_PRODUCTION.csv")) # Raw fao data
nei_levels <- read_csv("data/nei_codes.csv")

fao_species <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_SPECIES_GROUPS.csv")) %>% # Species ref
  rename(SPECIES = `3Alpha_Code`) %>% 
  #filter(SPECIES %in% nei_levels$SPECIES) %>% 
  left_join(., nei_levels)

validated_names <- read_csv("data/worms_validated_names.csv") %>% 
  rename(SPECIES = alpha_code)

val_fishbase <- read_csv("data/fao_species_fishbase_validated")


################################

fao_species_raw <- read_csv(file.path(dir_raw_data, "/FAO/production/TS_FI_PRODUCTION.csv")) %>% # Raw fao data
  distinct(SPECIES) %>% 
  left_join(fao_species) %>% 
  select(SPECIES, Scientific_Name)


api_pull <- fao_species_raw %>% 
  mutate(info = map(Scientific_Name, rredlist::rl_search, key = apikey))


api_clean <- api_pull %>% 
  mutate(category = map(info, pluck, "result", "category")) %>% 
  mutate(cat_clean = as.character(category)) 


api_temp_1 <- api_clean %>% 
  select(SPECIES, Scientific_Name, cat_clean) %>% 
  filter(cat_clean != "NULL")


api_failed_1 <- api_clean %>% 
  select(SPECIES, Scientific_Name, cat_clean) %>% 
  filter(cat_clean == "NULL") %>% 
  select(SPECIES) %>% 
  left_join(val_fishbase) %>% 
  filter(!is.na(validated_name_final)) %>% 
  mutate(info = map(validated_name_final, rredlist::rl_search, key = apikey))


api_failed_1_clean <- api_failed_1 %>% 
  mutate(category = map(info, pluck, "result", "category")) %>% 
  mutate(cat_clean = as.character(category)) 


api_temp_2 <- api_failed_1_clean %>% 
  select(SPECIES, Scientific_Name,validated_name_final, cat_clean) %>% 
  filter(cat_clean != "NULL")

api_temp_1 <- api_temp_1 %>% select(-Scientific_Name)



api_levels <- api_temp_2 %>% 
  select(SPECIES, cat_clean) %>% 
  rbind(api_temp_1) %>% 
  filter(cat_clean != "DD") %>% 
  rename(iucn_category = cat_clean)


write_csv(api_levels, "data/IUCN_to_FAO.csv")