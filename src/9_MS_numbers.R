#------------------------------------------------------------------------------#
## Project :  Data Landscape
## Purpose :  Get all the numbers for the manuscripte
##  Date   :  08/05/2020
## Author  :  Gordon Blasco
#------------------------------------------------------------------------------#

#### Load Data ####
#------------------------------------------------------------------------------#
library(tidyverse)
library(stringr)
library(sf)
library(rmapshaper)
library(patchwork)
library(viridis)
library(janitor)

source("~/github/aquaculture/src/directories.R") # Sets file directories 

# load in FAO data
fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/TS_FI_PRODUCTION.csv")) 
fao_country    <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_COUNTRY_GROUPS.csv")) 
fao_neis       <- read_csv("data/nei_codes.csv")
fao_country    <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_COUNTRY_GROUPS.csv")) 
fao_species    <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_SPECIES_GROUPS.csv")) %>% 
  rename(SPECIES = "3Alpha_Code")


# categorize freshwater species based on if they have "freshwater" in their
# cpc_class, isscaap_group, or in their name (freshwater nei's)


ram_codes <- read_csv("data/RAM_to_FAO.csv") %>% 
  distinct(SPECIES) %>% 
  mutate(RAM = "yes")


iucn_codes <- read_csv("data/IUCN_to_FAO.csv") %>% 
  distinct(SPECIES) %>% 
  mutate(IUCN = "yes")


spp_info <- fao_neis %>% 
  left_join(fao_species) %>% 
  mutate(habitat = if_else(
    (str_detect(string = ISSCAAP_Group, regex("freshwater", ignore_case = TRUE)) | 
       str_detect(string = CPC_Class, regex("freshwater", ignore_case = TRUE)) |
       str_detect(string = Name_En, regex("freshwater", ignore_case = TRUE))),
    "Freshwater", "Marine")) %>% 
  left_join(ram_codes) %>% 
  left_join(iucn_codes) 

# link up ISO3 codes
country_prep <- fao_country %>% 
  select(ISO3_Code, UN_Code) %>% 
  rename(
    ISO3 = ISO3_Code,
    COUNTRY = UN_Code
  )



# add in database info


####  ####
#------------------------------------------------------------------------------#

relevant_prod <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(YEAR == 2016,
         SOURCE == 4,
        # id_level == "Species",
         excluded ==  "included")

check <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(YEAR == 2016,
         SOURCE == 4) %>% 
  filter(is.na(UNIT)) %>% 
  distinct(SPECIES, .keep_all = TRUE)

aquaculture_species <- fao_production %>% 
  filter(
    SOURCE != 4
  ) %>% 
  distinct(SPECIES)



ram_prod <- relevant_prod %>% 
  filter(SPECIES %in% ram_codes$SPECIES) %>% 
  summarise(
    ram_spp = length(unique(SPECIES)),
    ram_total = sum(QUANTITY, na.rm = TRUE)
  )


iucn_prod <- relevant_prod %>% 
  filter(SPECIES %in% iucn_codes$SPECIES) %>% 
  summarise(
    iucn_spp = length(unique(SPECIES)),
    iucn_total = sum(QUANTITY, na.rm = TRUE)
  ) 

total_prod <- relevant_prod %>% 
  summarise(
    total_spp = length(unique(SPECIES)),
    total_total = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  bind_cols(ram_prod) %>% 
  bind_cols(iucn_prod) %>% 
  mutate(
    ram_perc = ram_total/total_total,
    iucn_perc = iucn_total/total_total
  )

####  ####
#------------------------------------------------------------------------------#
