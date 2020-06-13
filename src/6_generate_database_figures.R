################################################################################
## Project :  Data Landscape Paper
## Purpose :  Generate figure 1
##  Date   :  5/29/2020
## Author  :  Gordon Blasco
################################################################################


## Load data & libraries 
################################################################################
library(tidyverse)
library(patchwork)


source("~/github/aquaculture/src/directories.R") # Sets file directories 

# Raw fao data
fao_production <- read_csv(file.path(dir_raw_data, 
                           "/FAO/production/TS_FI_PRODUCTION.csv")) 

# Species ref
fao_species    <- read_csv(file.path(dir_raw_data, 
                           "/FAO/production/CL_FI_SPECIES_GROUPS.csv")) %>% 
  rename(SPECIES = "3Alpha_Code")

# Countries refrence
fao_country    <- read_csv(file.path(dir_raw_data, 
                           "/FAO/production/CL_FI_COUNTRY_GROUPS.csv")) 

fao_neis       <- read_csv("data/nei_codes.csv")


#iucn data
ram_codes <- read_csv("data/RAM_to_FAO.csv") %>% 
  distinct(SPECIES) %>% 
  mutate(RAM = "yes")

#ram data
iucn_codes <- read_csv("data/IUCN_to_FAO.csv") %>% 
  distinct(SPECIES) %>% 
  mutate(IUCN = "yes")

## Clean and prep data
################################################################################

spp_info <- fao_production %>% 
  distinct(SPECIES) %>% 
  left_join(fao_species) %>% 
  left_join(fao_neis) %>% 
  left_join(ram_codes) %>% 
  left_join(iucn_codes) %>% 
  mutate(FAO = "yes") %>% 
  mutate(excluded = case_when(
    CPC_Class == "Coral and similar products, shells of molluscs, crustaceans or echinoderms and cuttle-bone; live aquatic plants and animals for ornamental purpose"
    ~ "excluded", 
    TRUE ~ "included"
  )) 


#%>% 
  #filter(excluded == "included")#%>% 
  #mutate(ram=if_else(is.na(ram), "no", "yes")) #%>% 
  #mutate(iucn=if_else(is.na(iucn), "no", "yes"))



## Create species comparison dataset
################################################################################
 
tile_df <- spp_info %>% 
  select(SPECIES, Major_Group, FAO, RAM, IUCN) %>% 
  #mutate(major_group = factor())
  pivot_longer(cols = c("FAO", "RAM", "IUCN"), 
               names_to = "database", 
               values_to = "status") %>% 
  filter(!is.na(status)) %>% 
  arrange(Major_Group) %>% 
  mutate(SPECIES = fct_inorder(SPECIES))


ggplot(tile_df, aes(x = database, y = SPECIES, fill = Major_Group))+
  geom_tile() +
  theme(axis.text.y = element_blank())





## Create biomass species comparison
################################################################################
fao_core <- fao_production 

prod_all <- fao_core %>% 
  filter(YEAR == 2016) %>% 
  filter(SOURCE == 4) %>% 
  group_by(SPECIES) %>% 
  summarise(
    biomass = sum(QUANTITY)
  ) %>% 
  ungroup() %>% 
  arrange(-biomass) %>% 
  left_join(spp_info) %>% 
  group_by(Major_Group) %>% 
  summarize(
    all_bio = sum(biomass)
  ) %>% 
  ungroup()
 
prod_none <- fao_core %>% 
  filter(SPECIES %in% neither$SPECIES) %>% 
  filter(YEAR == 2016) %>% 
  filter(SOURCE == 4) %>% 
  group_by(SPECIES) %>% 
  summarise(
    biomass = sum(QUANTITY)
  ) %>% 
  ungroup() %>% 
  arrange(-biomass) %>% 
  left_join(spp_info) %>% 
  group_by(Major_Group) %>% 
  summarize(
    none_bio = sum(biomass)
  ) %>% 
  ungroup() %>% 
  left_join(prod_all) %>% 
  pivot_longer(cols=c("all_bio", "none_bio"))

ggplot(prod_none, aes(x = Major_Group, y = value, fill = name))+
  geom_col(position = "dodge")




prod_none <- fao_core %>% 
  filter(SPECIES %in% neither$SPECIES) %>% 
  filter(YEAR == 2016) %>% 
  filter(SOURCE == 4) %>% 
  group_by(SPECIES) %>% 
  summarise(
    biomass = sum(QUANTITY)
  ) %>% 
  ungroup() %>% 
  arrange(-biomass) %>% 
  left_join(spp_info) %>% 
  filter(id_level == "Species") %>% 
  filter(biomass >= 0) %>% 
  group_by(ISSCAAP_Group) %>% 
  top_n(1, wt = biomass)


ggplot(prod_none, aes(x = Major_Group, y = biomass, fill = SPECIES))+
  geom_col(position="stack", stat="identity")+
  theme(legend.position = "none")











## table counts
################################################################################

total_spp <- spp_info %>% 
  group_by(Major_Group) %>% 
  summarise(
    total_spp = n()
  )

total_spp_listed <- spp_info %>% 
  filter(id_level != "Nei") %>% 
  group_by(Major_Group) %>% 
  summarise(
    specific_species = n()
  )

total_spp_ram <- spp_info %>% 
  filter(!is.na(ram)) %>% 
  group_by(Major_Group) %>% 
  summarise(
    in_ram = n()
  )

total_spp_iucn <- spp_info %>% 
  filter(!is.na(iucn)) %>% 
  group_by(Major_Group) %>% 
  summarise(
    in_iucn = n()
  )


final_table <- total_spp %>% 
  left_join(total_spp_listed) %>% 
  left_join(total_spp_ram) %>% 
  left_join(total_spp_iucn) %>% 
  adorn_totals()



##  supplement tables
################################################################################


all <- spp_info %>% 
  filter(FAO == "yes"&
           RAM == "yes"&
           IUCN == "yes") %>% 
  group_by(ISSCAAP_Group) %>% 
  summarise(total = n())


neither <- spp_info %>% 
  filter(FAO == "yes"&
           is.na(RAM)&
           is.na(IUCN)) %>%  
  select(Name_En, Scientific_Name, SPECIES)








all_tbl <- spp_info %>% 
  filter(FAO == "yes"&
           RAM == "yes"&
           IUCN == "yes") %>%  
select(Name_En, Scientific_Name)

#write_csv(all_tbl, "data/SI_list_of_species_in_fao_ram_iucn.csv")






# neither counts
neither_counts <- spp_info %>% 
  filter(id_level == "Species") %>% 
  mutate(total_number = 1,
         neither = if_else((is.na(RAM) & is.na(IUCN)), 1, 0),
         all = if_else((FAO == "yes" & RAM == "yes" & IUCN == "yes"), 1, 0),
         one_of = if_else((RAM == "yes" & is.na(IUCN))| is.na(RAM) & IUCN == "yes", 1, 0),
         in_ram = if_else(RAM == "yes", 1, 0),
         only_ram = if_else(RAM == "yes" & is.na(IUCN), 1, 0),
         only_iucn = if_else(IUCN == "yes" & is.na(RAM), 1, 0),
         in_iucn = if_else(IUCN == "yes", 1, 0)
         ) %>% 
  group_by(Major_Group) %>% 
  summarize(
    total_spp = n(),
    in_ram = sum(in_ram, na.rm = TRUE),
    in_iucn = sum(in_iucn, na.rm = TRUE),
    neither = sum(neither, na.rm = TRUE),
    all = sum(all, na.rm = TRUE),
    one_of = sum(one_of, na.rm = TRUE),
    only_ram = sum(only_ram, na.rm = TRUE),
    only_iucn = sum(only_iucn, na.rm = TRUE),
  ) %>% 
  adorn_totals() %>% 
  mutate(perc_neither = neither/total_spp)






