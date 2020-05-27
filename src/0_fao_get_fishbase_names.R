##################################################
## Project: 
## Script purpose: 
## Date:
## Author: Gordon Blasco
##################################################

Val_all_FAO <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_SPECIES_GROUPS.csv")) %>% 
  rename(SPECIES="3Alpha_Code") %>% 
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
  ) %>%
  mutate(validated_name_final = 
           case_when(
             validated_name_final == "nope"~Scientific_Name,
             validated_name_final != "nope"~validated_name_final)) %>% 
  select(SPECIES, Scientific_Name, validated_name_final) 

write_csv(Val_all_FAO, "data/fao_species_fishbase_validated")
