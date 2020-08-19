library(tidyverse)
library(ramlegacy)
library(rfishbase)
library(janitor)

source("src/file_names.R") # Sets file directories 
fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/TS_FI_PRODUCTION.csv")) # Raw fao data

species_mask <- fao_production %>% 
  distinct(SPECIES) %>% 
  pull(SPECIES)


nei_levels <- read_csv("data/nei_codes.csv")

fao_species <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_SPECIES_GROUPS.csv")) %>% # Species ref
  rename(SPECIES = `3Alpha_Code`) %>% 
  filter(SPECIES %in% species_mask) %>% 
  left_join(., nei_levels)

validated_names <- read_csv("data/worms_validated_names.csv") %>% 
  rename(SPECIES = alpha_code) %>% 
  filter(SPECIES %in% species_mask) 

val_fishbase <- read_csv("data/fao_species_fishbase_validated") %>% 
  filter(SPECIES %in% species_mask)


RAM_all <- load_ramlegacy()
RAM_meta <- RAM_all[["metadata"]]


ram_names <- RAM_meta %>% 
  distinct(scientificname) %>% 
  left_join(., fao_species, by = c("scientificname" = "Scientific_Name")) %>% 
  select(scientificname, SPECIES)


ram_temp1 <- ram_names %>% 
  filter(!is.na(SPECIES)) %>% 
  select(scientificname, SPECIES)

ram_failed <- ram_names %>% 
  filter(is.na(SPECIES)) %>% 
  select(scientificname) %>% 
  mutate(fishbase_name = lapply(.$scientificname, validate_names, server = "fishbase")) %>% 
  mutate(sealifebase_name = lapply(.$scientificname, validate_names, server = "sealifebase")) %>% 
  mutate(scientificname = as.character(scientificname),
         fishbase_name = as.character(fishbase_name),
         sealifebase_name = as.character(sealifebase_name))


ram_val <- ram_failed %>% 
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

ram_temp2 <- ram_val %>% 
  filter(!is.na(SPECIES)) %>% 
  select("scientificname", SPECIES)

ram_val2 <- ram_val %>% 
  filter(is.na(SPECIES)) %>% 
  select(scientificname) %>%
  left_join(., val_fishbase, by = c("scientificname" = "validated_name_final")) 

ram_temp3 <- ram_val2 %>% 
  filter(!is.na(`SPECIES`)) %>%
  select("scientificname", SPECIES)


ram_to_fao_species <- ram_temp1 %>% 
  bind_rows(ram_temp2) %>% 
  bind_rows(ram_temp3) %>% 
  select(scientificname, SPECIES)%>% 
  filter(!is.na(SPECIES)) %>% 
  distinct(scientificname, SPECIES)


# 59 species not identified via automatic name validation
ram_final_check <- RAM_meta %>% 
  distinct(scientificname) %>% 
  left_join(ram_to_fao_species) %>% 
  filter(is.na(SPECIES)) %>% 
  mutate(SPECIES = 
           case_when(
             scientificname == "Pleuronectes quadrituberculatus" ~ "ALP",
             scientificname == "Makaira mazara" ~ "BUM",
             
             scientificname == "Pandalus eous" ~ "NA",
             scientificname == "Clupea bentincki" ~ "NA",
             scientificname == "Sebastes variabilis" ~ "NA",
             scientificname == "Sebastes norvegicus" ~ "NA",
             scientificname == "Chrysophrys auratus" ~ "NA",
             scientificname == "Ammodytes hexapterus" ~ "NA",
             scientificname == "Neoplatycephalus richardsoni" ~ "NA",
             scientificname == "Pseudopleuronectes herzensteini" ~ "NA"
           ))


#write_csv(ram_to_fao_species, "data/RAM_to_FAO.csv")
