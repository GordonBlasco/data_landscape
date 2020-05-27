library(tidyverse)
library(ramlegacy)
#library(seaaroundus)
#library(readxl)
library(rfishbase)
library(janitor)

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


ram_names <- RAM_meta %>% 
  distinct(scientificname) %>% 
  left_join(., fao_species, by = c("scientificname" = "Scientific_Name")) %>% 
  select(scientificname, SPECIES)


ram_val <- ram_names %>% 
  filter(is.na(SPECIES)) %>% 
  select(scientificname) %>% 
  mutate(fishbase_name = lapply(.$scientificname, validate_names, server = "fishbase")) %>% 
  mutate(sealifebase_name = lapply(.$scientificname, validate_names, server = "sealifebase")) %>% 
  mutate(scientificname = as.character(scientificname),
         fishbase_name = as.character(fishbase_name),
         sealifebase_name = as.character(sealifebase_name)) %>% 
  mutate(
    validated_name_final =
      case_when(
        fishbase_name == "character(0)" & sealifebase_name == "character(0)" ~ "nope",
        fishbase_name != "character(0)" & sealifebase_name == "character(0)" ~ fishbase_name,
        fishbase_name == "character(0)" & sealifebase_name != "character(0)" ~ sealifebase_name,
        fishbase_name != "character(0)" & sealifebase_name != "character(0)" ~ fishbase_name
      )
  ) %>%
  mutate(validated_name_final = 
           case_when(
             validated_name_final == "nope"~scientificname,
            validated_name_final != "nope"~validated_name_final)) %>% 
  select(scientificname, scientificname, validated_name_final) 


ram_val_fao_not <- ram_val %>% 
  left_join(fao_species, by = c("validated_name_final" = "Scientific_Name")) %>% 
  select(scientificname, SPECIES)

prep1 <- ram_val_fao_not %>% 
  filter(!is.na(SPECIES))


so_far <- ram_names %>% 
  filter(!is.na(SPECIES)) %>% 
  rbind(prep1)






fao_spp_val <- fao_species %>% 
  filter(!(SPECIES %in% so_far$SPECIES)) %>% 
  select(SPECIES, Scientific_Name) %>% 
  mutate(fishbase_name = lapply(.$Scientific_Name, validate_names, server = "fishbase")) %>% 
  mutate(sealifebase_name = lapply(.$Scientific_Name, validate_names, server = "sealifebase")) %>% 
  mutate(Scientific_Name = as.character(Scientific_Name),
         fishbase_name = as.character(fishbase_name),
         sealifebase_name = as.character(sealifebase_name)) %>% 
  mutate(
    validated_name_final =
      case_when(
        fishbase_name == "character(0)" & sealifebase_name == "character(0)" ~ "nope",
        fishbase_name != "character(0)" & sealifebase_name == "character(0)" ~ fishbase_name,
        fishbase_name == "character(0)" & sealifebase_name != "character(0)" ~ sealifebase_name,
        fishbase_name != "character(0)" & sealifebase_name != "character(0)" ~ fishbase_name
      )
  ) 



fao_spp_val_final <- fao_spp_val %>%
  mutate(validated_name_final = 
           case_when(
             validated_name_final == "nope"~Scientific_Name,
             validated_name_final != "nope"~validated_name_final)) %>% 
  select(SPECIES, validated_name_final) 


ram_val_fao_val <- ram_val_fao_not %>% 
  filter(is.na(SPECIES)) %>% 
  select(-SPECIES) %>% 
  left_join(fao_spp_val_final, by = c("scientificname" = "validated_name_final"))

prp2 <- ram_val_fao_val %>% 
  filter(!is.na(SPECIES))



so_far_2 <- ram_names %>% 
  filter(!is.na(SPECIES)) %>% 
  rbind(prep1) %>% 
  rbind(prp2) # at 302 58 more to go!



## now lets use the worms data

worms_left <- validated_names %>% 
  filter(!(SPECIES %in% so_far_2$SPECIES)) 



after_worms <- ram_val_fao_val %>% 
  filter(is.na(SPECIES)) %>% 
  select(-SPECIES) %>% 
  left_join(worms_left, by = c("scientificname" = "resolved_sci_name"))



teast <- read_csv("/home/shares/clean-seafood/data/aquaculture_data/RAM_2019.csv")



#
fao_shit <- ram_names %>% 
  filter(!is.na(SPECIES))

ram_left <- ram_names %>% 
  filter(is.na(SPECIES))
