#-----------------------------------------------------------------------------#
## Project :   Data AL
## Purpose :   script purpose
##  Date   :   mm/dd/yyyy
## Author  :   Gordon Blasco
#------------------------------------------------------------------------------#


#### Load Data ####
#------------------------------------------------------------------------------#
library(tidyverse)
library(stringr)
library(sf)
library(rmapshaper)

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

spp_info <- fao_neis %>% 
  left_join(fao_species) %>% 
  mutate(habitat = if_else(
         (str_detect(string = ISSCAAP_Group, regex("freshwater", ignore_case = TRUE)) | 
          str_detect(string = CPC_Class, regex("freshwater", ignore_case = TRUE)) |
          str_detect(string = Name_En, regex("freshwater", ignore_case = TRUE))),
         "Freshwater", "Marine"))

# link up ISO3 codes
country_prep <- fao_country %>% 
  select(ISO3_Code, UN_Code) %>% 
  rename(
    ISO3 = ISO3_Code,
    COUNTRY = UN_Code
  )

#### Calcuate national freshwater NEI's and totals ####
#------------------------------------------------------------------------------#
 
national_fresh <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(habitat == "Freshwater" &
         YEAR == 2016 &
         SOURCE == 4) %>% 
  group_by(COUNTRY) %>% 
  summarise(
    total_landed = sum(QUANTITY, na.rm = TRUE),
    total_nei = sum(QUANTITY[id_level == "Nei"]), 
    total_spp = sum(QUANTITY[id_level == "Species"]),
  ) %>% 
  filter(!(total_landed == 0 & total_nei == 0 & total_spp == 0)) %>% 
  mutate(prop_nei = total_nei/total_landed) %>% 
  left_join(country_prep)


national_marine <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(habitat == "Marine" &
           YEAR == 2016 &
           SOURCE == 4) %>% 
  group_by(COUNTRY) %>% 
  summarise(
    total_landed = sum(QUANTITY, na.rm = TRUE),
    total_nei = sum(QUANTITY[id_level == "Nei"]), 
    total_spp = sum(QUANTITY[id_level == "Species"]),
  ) %>% 
  filter(!(total_landed == 0 & total_nei == 0 & total_spp == 0)) %>% 
  mutate(prop_nei = total_nei/total_landed) %>% 
  left_join(country_prep)


#### Load in maps ####
#------------------------------------------------------------------------------#
# read in world map
world_fn <- "/home/shares/clean-seafood/raw_data/world_vec/world_simplified.shp"
world_raw <- st_read(world_fn)
world_df <- st_set_geometry(world_raw, NULL) %>% # remove geometry
  distinct(ISO3) %>% 
  mutate(connected = "yes")


# make fresh and marine maps 
map_freshwater <- world_raw %>% 
  filter(poly_type == "GADM") %>% 
  left_join(national_fresh) #%>% 
  #mutate_if(is.numeric, replace_na, 0)

test <- st_set_geometry(map_freshwater, NULL)


map_marine <- world_raw %>% 
  #filter(poly_type != "GADM") %>% 
  left_join(national_marine) %>% 
  mutate(total_landed = case_when(
    poly_type == "GADM" ~ NA_real_, 
    TRUE ~ total_landed
  ))

test <- st_set_geometry(map_marine, NULL)




#### Plotting them up! ####
#------------------------------------------------------------------------------#


ggplot(map_freshwater, aes(fill = total_landed))+
  geom_sf()


ggplot(map_marine, aes(fill = total_landed))+
  geom_sf(color = "black")




