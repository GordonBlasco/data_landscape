#------------------------------------------------------------------------------#
## Project :  Data Landscape
## Purpose :  Generate Figure 4
##  Date   :  06/22/2020
## Author  :  Gordon Blasco
#------------------------------------------------------------------------------#


#### Libraries and Data ####
#------------------------------------------------------------------------------#
library(tidyverse)
library(patchwork)
library(viridis)
library(viridisLite)
library(ggpubr)

source("~/github/aquaculture/src/directories.R") # Sets file directories 

fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/TS_FI_PRODUCTION.csv")) 
fao_country    <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_COUNTRY_GROUPS.csv")) 
fao_neis       <- read_csv("data/nei_codes.csv")
# fao_country    <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_COUNTRY_GROUPS.csv")) 
fao_species    <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_SPECIES_GROUPS.csv")) %>% 
  rename(SPECIES = "3Alpha_Code")


#### Prep the data ####
#------------------------------------------------------------------------------#

spp_info <- fao_neis %>% 
  left_join(fao_species)


fao_prep <- fao_production %>% 
  filter(YEAR == 2016&
         SOURCE == 4) %>% 
  left_join(spp_info)

totals_nei_prep <- fao_prep %>% 
  summarize(
    total_landed = sum(QUANTITY, na.rm = TRUE),
    total_nei = sum(QUANTITY[id_level == "Nei"], na.rm = TRUE), 
    total_spp = sum(QUANTITY[id_level == "Species"], na.rm = TRUE),
  ) %>% 
  ungroup() %>% 
  mutate(prop_nei = total_nei/total_landed) %>% 
  mutate(Major_Group = "Total_catch") #%>% 

totals_nei <- totals_nei_prep %>% 
  select(Major_Group, total_nei)

totals_percent_nei <- totals_nei_prep %>% 
  select(Major_Group, prop_nei) %>% 
  rename(prop_nei_level = prop_nei) %>% 
  mutate(nei_levels = "Percent NEI")


prop_maj <- fao_prep %>% 
  group_by(Major_Group) %>% 
  summarize(
    total_landed = sum(QUANTITY, na.rm = TRUE),
    total_nei = sum(QUANTITY[id_level == "Nei"], na.rm = TRUE), 
    total_spp = sum(QUANTITY[id_level == "Species"], na.rm = TRUE),
  ) %>% 
  ungroup() %>% 
  mutate(prop_nei = total_nei/total_landed)

prop_nei_totals <- prop_maj %>% 
  select(Major_Group, total_nei)

prop_maj_prep <- prop_maj %>% 
  select(Major_Group, prop_nei) %>% 
  rename(prop_nei_level = prop_nei) %>% 
  mutate(nei_levels = "Percent NEI")


totals_level <- fao_prep %>% 
  filter(id_level == "Nei") %>% 
  group_by(nei_levels) %>% 
  summarise(
    total_landed = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(Major_Group = "Total_catch") %>% 
  left_join(totals_nei) %>% 
  mutate(prop_nei_level = total_landed/total_nei) %>% 
  bind_rows(totals_percent_nei)

prop_level <- fao_prep %>% 
  filter(id_level == "Nei") %>% 
  group_by(Major_Group, nei_levels) %>% 
  summarise(
    total_landed = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  left_join(prop_nei_totals) %>% 
  mutate(prop_nei_level = total_landed/total_nei) %>% 
  bind_rows(prop_maj_prep) %>% 
  bind_rows(totals_level) %>% 
 # add_row(
 #   Major_Group = "CRUSTACEA", 
 #   nei_levels = "blank"
 # ) %>% 
  mutate(nei_levels = factor(nei_levels, 
                                levels = c("Percent NEI",
                                          # "blank", 
                                           "mixed species",
                                           "genus", 
                                           "family", 
                                           "order",
                                           "major group"
                                           ))) %>% 
  mutate(percent = paste0(round(prop_nei_level*100), "%")) %>% 
  filter((nei_levels == "Percent NEI" | percent != "0%")) %>% 
  mutate(clean_majors = case_when(
    Major_Group == "Total_catch" ~ "Total Landings", 
    Major_Group == "AMPHIBIA, REPTILIA" ~ "Amphibia, Reptilia", 
    Major_Group == "CRUSTACEA" ~ "Crustacea", 
    Major_Group == "INVERTEBRATA AQUATICA" ~ "Invertebrata Aquatica", 
    Major_Group == "MAMMALIA" ~ "Mammalia", 
    Major_Group == "MOLLUSCA" ~ "Mollusca", 
    Major_Group == "PISCES" ~ "Pisces", 
    Major_Group == "PLANTAE AQUATICAE" ~ "Plantae Aquatica", 
    
  ))


ggplot(prop_level, aes(x = nei_levels, y = clean_majors, fill = prop_nei_level)) +
  geom_tile(color = "black")+
  scale_fill_viridis(direction = 1,
                     option = "magma")+
  theme_classic() +
  geom_hline(yintercept = 7.5, size = 2)+
  geom_vline(xintercept = 1.5, size = 2)+
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(
    #title = "Major Group's NEI taxonomic resolution",
    y = "", 
    x = "NEI taxonomic resolution"
  ) +
  ggpubr::labs_pubr()+
  theme(legend.position = "none")+
  geom_label(aes(label = percent), size = 6, fill = "white")

  

