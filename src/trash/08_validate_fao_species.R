##################################################
## Project: 
## Script purpose: Validate FAO names
## Date:
## Author: Gordon Blasco
##################################################
library(tidyverse)
library(rfishbase)

fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/TS_FI_PRODUCTION.csv")) # Raw fao data
fao_spp <- fao_production %>% 
  distinct(SPECIES) %>% 
  pull(SPECIES)


species_codes <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_SPECIES_GROUPS.csv")) %>%  # Species ref
  rename(SPECIES = "3Alpha_Code") %>% 
  filter(SPECIES %in% fao_spp) %>% 
  select(SPECIES, Scientific_Name)




#makes new columns for fishbase and sealifebase validations
val_fishbase <- species_codes %>%
 mutate(fishbase_name = lapply(.$Scientific_Name, validate_names, server = "fishbase")) %>%
 mutate(sealifebase_name = lapply(.$Scientific_Name, validate_names, server = "sealifebase")) %>%
 mutate(Scientific_Name = as.character(Scientific_Name),
        fishbase_name = as.character(fishbase_name),
        sealifebase_name = as.character(sealifebase_name))

saveRDS(val_fishbase, "data/fao_fishbase_validation_raw.rds")
#val_fishbase <- readRDS("data/fao_fishbase_validation_raw.rds")) # read this in instead of running validation code ~ 1 hour


val_fishbase_final <- val_fishbase %>% 
  mutate(
    validated_name_final =
      case_when(
        fishbase_name == "character(0)" & sealifebase_name == "character(0)" ~ "nope",
        fishbase_name != "character(0)" & sealifebase_name == "character(0)" ~ fishbase_name,
        fishbase_name == "character(0)" & sealifebase_name != "character(0)" ~ sealifebase_name,
        fishbase_name != "character(0)" & sealifebase_name != "character(0)" ~ fishbase_name
      )
  ) %>% 
  mutate(validated_name_final = na_if(validated_name_final, "nope")) %>% 
  select(-fishbase_name, -sealifebase_name) %>% 
  select(SPECIES, Scientific_Name, validated_name_final, everything())









