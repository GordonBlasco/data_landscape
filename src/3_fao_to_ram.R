library(tidyverse)
library(ramlegacy)
library(rfishbase)
library(janitor)

source("src/file_names.R") # Sets file directories 
fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/TS_FI_PRODUCTION.csv")) # Raw fao data

species_mask <- fao_production %>% 
  distinct(SPECIES) %>% 
  pull(SPECIES)


nei_levels <- read_csv("data/nei_codes.csv")
fao_neis       <- read_csv("data/nei_codes.csv") 


fao_species <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/CL_FI_SPECIES_GROUPS.csv")) %>% # Species ref
  rename(SPECIES = `3Alpha_Code`) %>% 
  filter(SPECIES %in% species_mask) %>% 
  left_join(., nei_levels)
  
prod_spp_info <- fao_species %>% 
    filter(SPECIES %in% species_mask)
  
spp_tons <- fao_production %>% 
    group_by(SPECIES) %>% 
    summarise(total = sum(QUANTITY, na.rm = TRUE)) %>% 
    left_join(fao_species)

validated_names <- read_csv("data/worms_validated_names.csv") %>% 
  rename(SPECIES = alpha_code) %>% 
  filter(SPECIES %in% species_mask) 

val_fishbase <- read_csv("data/fao_species_fishbase_validated") %>% 
  filter(SPECIES %in% species_mask)


RAM_all <- load_ramlegacy()
RAM_meta <- RAM_all[["metadata"]]
RAM_taxa <- RAM_all[["taxonomy"]]


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
  filter(is.na(SPECIES)) #%>% 
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


ram_final_added <- RAM_meta %>% 
    distinct(scientificname) %>% 
    left_join(ram_to_fao_species) %>% 
    filter(is.na(SPECIES)) %>% 
  mutate(SPECIES = 
           case_when(
             scientificname == "Sebastes fasciatus" ~ NA_character_,#1
             scientificname == "Pleuronectes quadrituberculatus" ~ "ALP",#2
             scientificname == "Bathyraja parmifera" ~ NA_character_,#3
             scientificname == "Clupeonella engrauliformis" ~ NA_character_,#4
             scientificname == "Theragra chalcogramma" ~ "ALK",#5
             scientificname == "Pandalus eous" ~ "PRA",#6
             scientificname == "Decapterus muroadsi" ~ "RSA",#7
             scientificname == "Centroberyx gerrardi" ~ NA_character_,#8
             scientificname == "Makaira mazara" ~ "BUM",#9
             scientificname == "Sebastes paucispinis" ~ NA_character_,#10
             scientificname == "Sebastes auriculatus" ~ NA_character_,#11
             scientificname == "Thamnaconus modestus" ~ NA_character_,#12
             scientificname == "Dipturus laevis" ~ NA_character_,#13
             scientificname == "Scorpaena guttata" ~ NA_character_,#14
             scientificname == "Clupea bentincki" ~ "CKI",#15
             scientificname == "Pristipomoides filamentosus" ~ NA_character_,#16
             scientificname == "Raja eglanteria" ~ NA_character_,#17
             scientificname == "Platycephalus conatus" ~ NA_character_,#18
             scientificname == "Sebastes variabilis" ~ NA_character_,#19
             scientificname == "Hippoglossoides dubius" ~ "FTS",#20
             scientificname == "Etelis coruscans" ~ NA_character_,#21
             scientificname == "Epinephelus itajara" ~ NA_character_,#22
             scientificname == "Sebastes norvegicus" ~ "REG",#23
             scientificname == "Enteroctopus dofleini" ~ NA_character_,#24
             scientificname == "Strongylocentrotus droebachiensis" ~ NA_character_,#25
             scientificname == "Branchiostegus japonicus" ~ NA_character_,#26
             scientificname == "Paracaesio caerulea" ~ NA_character_,#27
             
             scientificname == "Sebastolobus macrochir" ~ NA_character_,#28
             scientificname == "Pristipomoides sieboldii" ~ NA_character_,#29
             scientificname == "Raja rhina" ~ NA_character_,#30
             scientificname == "Sebastes polyspinis" ~ NA_character_,#31
             scientificname == "Lepidopsetta polyxystra" ~ "ROS",#32
             scientificname == "Chrysophrys auratus" ~ "GSU",#33
             scientificname == "Cleisthenes herzensteini" ~ NA_character_,#34
             scientificname == "Ammodytes hexapterus" ~ NA_character_,#35
             scientificname == "Sebastes maliger" ~ NA_character_,#36
             scientificname == "Glyptocephalus zachirus" ~ NA_character_,#37
             scientificname == "Sebastes aleutianus" ~ NA_character_,#38
             scientificname == "Eopsetta grigorjewi" ~ NA_character_,#39
             
             
             scientificname == "Leucoraja garmani" ~ NA_character_,#40
             scientificname == "Clidoderma asperrimum" ~ NA_character_,#41
             scientificname == "Epigonus crassicaudus" ~ NA_character_,#42
             scientificname == "Oratosquilla oratoria" ~ NA_character_,#43
             scientificname == "Sebastes zacentrus" ~ NA_character_,#44
             scientificname == "Malacoraja senta" ~ NA_character_,#45
             scientificname == "Squalus suckleyi" ~ NA_character_,#46
             scientificname == "Loligo bleekeri" ~ NA_character_,#47
             scientificname == "Sebastes borealis" ~ NA_character_,#48
             scientificname == "Pandalopsis dispar" ~ NA_character_,#49
             
             
             scientificname == "Uroteuthis edulis" ~ NA_character_,#50
             scientificname == "Sillago flindersi" ~ NA_character_,#51
             scientificname == "Pseudocarcinus gigas" ~ NA_character_,#52
             scientificname == "Neoplatycephalus richardsoni" ~ NA_character_,#53
             scientificname == "Tanakius kitaharae" ~ NA_character_,#54
             scientificname == "Leucoraja ocellata" ~ NA_character_,#55
             scientificname == "Lophius litulon" ~ NA_character_,#56
             scientificname == "Pseudopleuronectes herzensteini" ~ "YFL",#57
             
             
             scientificname == "" ~ NA_character_,#58
             scientificname == "" ~ NA_character_,#59
             scientificname == "" ~ NA_character_,#60
             scientificname == "" ~ NA_character_,#61
             scientificname == "" ~ NA_character_,#62
             scientificname == "" ~ NA_character_,#63
           ))
  
#ram_to_fao_species %>% filter(scientificname %in% ram_to_fao_species_final$scientificname)

ram_to_fao_species_final <- ram_final_added %>% 
  filter(!is.na(SPECIES)) %>% 
  bind_rows(ram_to_fao_species)

# species with no FAO code 
farmed_spp_only_in_ram <- fao_production %>% 
  distinct(SPECIES) %>% 
  left_join(fao_species) %>% 
  left_join(fao_neis) %>% 
  filter(farmed_only == "yes") %>% 
  left_join(ram_to_fao_species) %>% 
  filter(!is.na(scientificname)) %>% 
  select(scientificname) %>% 
  mutate(farmed_only = "yes")


ram_taxon_info <- RAM_meta %>% 
  distinct(scientificname, .keep_all = TRUE) 



ram_table_s2 <- RAM_meta %>% 
  distinct(scientificname) %>% 
  filter(!(scientificname %in% ram_to_fao_species_final$scientificname)) %>% 
  mutate(farmed_only = NA_character_) %>% 
  bind_rows(farmed_spp_only_in_ram) %>% 
  left_join(ram_taxon_info) %>% 
  select(
    scientificname,
    commonname,
    FisheryType,
    farmed_only
  )


ram_to_fao_species_final
ram_table_s2

write_csv(ram_to_fao_species_final, "data/RAM_to_FAO.csv")
write_csv(ram_table_s2, "figures/table_S2.csv")
  


# looking at duplicate species. 

duplicate_codes <- ram_to_fao_species_final %>% 
  group_by(SPECIES) %>% 
  summarize(n = n()) %>% 
  filter(n>1)

duplicates <- ram_to_fao_species_final %>% 
  filter(SPECIES %in% duplicate_codes$SPECIES)



# supplement tables come after nei and farming code. run script #2
# 
# fao_neis <- read_csv("data/nei_codes.csv")
#   
# farmed_spp_only_in_ram <- fao_production %>% 
#   distinct(SPECIES) %>% 
#   left_join(fao_species) #%>% 
#   left_join(fao_neis) %>% 
#   filter(farmed_only == "yes") %>% 
#   left_join(ram_to_fao_species) %>% 
#   filter(!is.na(scientificname)) %>% 
#   select(scientificname)
#   
#   
# 
# ram_taxon_info <- RAM_meta %>% 
#   distinct(scientificname, .keep_all = TRUE) 
#   
# ram_table_s2 <- ram_final_check %>% 
#   select(-SPECIES) %>% 
#   rbind(farmed_spp_only_in_ram) %>% 
#   left_join(ram_taxon_info) %>% 
#   select(
#     scientificname,
#     commonname,
#     FisheryType
#   ) %>% 
#   filter(!str_detect(scientificname, pattern = "spp$"))
# 
# 
# 
# 
# tester3 <- RAM_meta%>% 
#   distinct(scientificname) %>% 
#   filter(str_detect(scientificname, pattern = "spp$"))


