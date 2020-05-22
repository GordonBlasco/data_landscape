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

validated_names <- read_csv("data/worms_validated_names.csv") %>% 
  rename(SPECIES = alpha_code)


RAM_all <- load_ramlegacy()
RAM_meta <- RAM_all[["metadata"]]



## Join RAM and FAO
##################################################

### first join on scientific name
ram_names <- RAM_meta %>% 
  distinct(scientificname) %>% 
  left_join(., fao_species, by = c("scientificname" = "Scientific_Name")) 

val_prep <- validated_names %>% 
  select(SPECIES, resolved_sci_name) %>% 
  filter(!is.na(resolved_sci_name))

ram_temp1 <- ram_names %>% 
  filter(is.na(SPECIES)) %>% 
  select(scientificname) %>% 
  left_join(val_prep, by = c("scientificname"="resolved_sci_name")) 


ram_temp2 <- ram_temp1 %>% 
  filter(is.na(SPECIES)) %>% 
  select(scientificname) %>% 
  mutate(fishbase_name = lapply(.$scientificname, validate_names, server = "fishbase")) %>% 
  mutate(sealifebase_name = lapply(.$scientificname, validate_names, server = "sealifebase")) %>% 
  mutate(scientificname = as.character(scientificname),
         fishbase_name = as.character(fishbase_name),
         sealifebase_name = as.character(sealifebase_name))


ram_val <- ram_temp2 %>% 
  mutate(
    validated_name_final =
      case_when(
        fishbase_name == "character(0)" & sealifebase_name == "character(0)" ~ "nope",
        fishbase_name != "character(0)" & sealifebase_name == "character(0)" ~ fishbase_name,
        fishbase_name == "character(0)" & sealifebase_name != "character(0)" ~ sealifebase_name,
        fishbase_name != "character(0)" & sealifebase_name != "character(0)" ~ fishbase_name
      )
  ) %>% 
  select(validated_name_final) %>% 
  rename(scientificname = validated_name_final) %>% 
  left_join(., fao_species, by = c("scientificname" = "Scientific_Name")) 


ram_val_2 <- ram_val
  filter(is.na(SPECIES)) %>% 
  select(scientificname) %>% 
    left_join(val_prep, by = c("scientificname"="validated_name_final")) 



ram_val_4 <- ram_val_2 %>% 
  filter(is.na(SPECIES)) %>% 
  select(scientificname) %>% 
  left_join(validated_names, by = c("scientificname"="resolved_sci_name")) 





fao_to_ram <- ram_names %>% # must be 360 obs long
  filter(!is.na(SPECIES)) %>% # at 284, 76 more to go.
  select(SPECIES, scientificname) %>% 
  rbind(ram_temp1) %>% 
  filter(!is.na(SPECIES)) # at 301 - 59  more to go!

