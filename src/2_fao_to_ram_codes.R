##################################################
## Project: 
## Script purpose: 
## Date:
## Author: Gordon Blasco
##################################################

## Load libraries
##################################################
library(tidyverse)
library(ramlegacy)
library(rfishbase)

## Load data from FAO and RAM
##################################################

source("src/file_names.R") # Sets file directories 
fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/TS_FI_PRODUCTION.csv")) # Raw fao data
nei_levels <- read_csv("data/nei_codes.csv")

fao_species <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_SPECIES_GROUPS.csv")) %>% # Species ref
  rename(SPECIES = `3Alpha_Code`) %>% 
  filter(SPECIES %in% nei_levels$SPECIES) %>% 
  left_join(., nei_levels)


RAM_all <- load_ramlegacy()
RAM_meta <- RAM_all[["metadata"]]



## Join RAM and FAO
##################################################

### first join on scientific name
ram_names <- RAM_meta %>% 
  distinct(scientificname) %>% 
  left_join(., fao_species, by = c("scientificname" = "Scientific_Name")) 

val_prep <- val_fishbase_final %>% 
  select(SPECIES, validated_name_final) %>% 
  filter(!is.na(validated_name_final))

ram_temp1 <- ram_names %>% 
  filter(is.na(SPECIES)) %>% 
  select(scientificname) %>% 
  left_join(val_prep, by = c("scientificname"="validated_name_final"))

fao_to_ram <- ram_names %>% # must be 360 obs long
  filter(!is.na(SPECIES)) # at 284, 76 more to go.













