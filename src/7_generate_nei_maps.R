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
library(patchwork)
library(viridis)

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
world_fn <- "/home/shares/clean-seafood/raw_data/world_vec/world_vector.shp"
world_raw <- st_read(world_fn)
world_df <- st_set_geometry(world_raw, NULL) %>% # remove geometry
  distinct(ISO3) %>% 
  mutate(connected = "yes")


# make fresh and marine maps 
map_freshwater <- world_raw %>% 
  filter(poly_type == "GADM") %>% 
  left_join(national_fresh) #%>% 
  #mutate_if(is.numeric, replace_na, 0)

test_1 <- st_set_geometry(map_freshwater, NULL)


map_marine <- world_raw %>% 
  #filter(poly_type != "GADM") %>% 
  left_join(national_marine) %>% 
  mutate(total_landed = case_when(
    poly_type == "GADM" ~ NA_real_, 
    TRUE ~ total_landed
  ))

test_2 <- st_set_geometry(map_marine, NULL)




#### Plotting them up! ####
#------------------------------------------------------------------------------#

# nei tonnage plot
fresh_plot <- ggplot(map_freshwater, aes(fill = log(total_landed+1)))+
  geom_sf(size = .1) +
  labs( title = "Freshwater NEI landings in 2016")


marine_plot <- ggplot(map_marine, aes(fill = log(total_landed+1)))+
  geom_sf(size = .1) +
  labs(title = "Marine NEI landings in 2016")

# prop NEI plots
fresh_prop_plot <- ggplot(map_freshwater, aes(fill = prop_nei))+
  geom_sf(size = .1) +
  labs( title = "Freshwater NEI landings in 2016")

marine_prop_plot <- ggplot(map_marine, aes(fill = prop_nei))+
  geom_sf(size = .1) +
  labs(title = "Marine NEI landings in 2016")


patches_prop <- (fresh_prop_plot  + plot_layout(guides = 'keep')) +
                 marine_prop_plot + plot_layout(guides = 'collect')

patches_prop

patches <- (fresh_plot  + plot_layout(guides = 'keep')) + 
            marine_plot + plot_layout(guides = 'collect')

patches <- patches & 
  theme_bw() &
  scale_fill_viridis(direction = 1, limits = c(0, 16.5), option = "magma") &
  scale_y_continuous(expand = c(0,0)) &
  scale_x_continuous(expand = c(0,0)) &
  guides(fill = guide_legend(title = "Nei Biomass Tonnes (log scale)"#, 
                             #title.position = "top", 
                             #title.hjust = 0.5)
  )) &
  theme(legend.title.align = 0.5,
        legend.direction = "horizontal",
        legend.position = 'bottom',
        legend.box.just = "center")

final_plot <- patches + plot_layout(guides = 'collect')#, ncol = 3#, 
#widths = c(5, 1, 5)#,
# heights = c(1,1,1)
#)




ggsave(final_plot, "figures/figure_3.png", 
       width = 11, 
       height = 8, 
       device = "png", 
       units = "in")
