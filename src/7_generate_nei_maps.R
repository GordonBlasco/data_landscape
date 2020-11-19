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
library(janitor)

source("~/github/aquaculture/src/directories.R") # Sets file directories 

# load in FAO data
fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/TS_FI_PRODUCTION.csv"), 
                           col_types = cols(COUNTRY = col_character())) # Raw fao data
fao_country    <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/CL_FI_COUNTRY_GROUPS.csv")) 
fao_country    <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/CL_FI_COUNTRY_GROUPS.csv")) 
fao_species    <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/CL_FI_SPECIES_GROUPS.csv")) %>% 
                  rename(SPECIES = "3Alpha_Code")


# categorize freshwater species based on if they have "freshwater" in their
# cpc_class, isscaap_group, or in their name (freshwater nei's)

fao_neis <- read_csv("data/nei_codes.csv") %>% 
  filter(excluded == "included")

excluded_spp <- fao_neis %>% 
  pull(SPECIES)

fao_production <- fao_production %>% 
  filter(SPECIES %in% excluded_spp)


spp_info <- fao_neis %>% 
  left_join(fao_species) %>% 
  mutate(habitat = if_else(
         (str_detect(string = ISSCAAP_Group, regex("freshwater", ignore_case = TRUE)) | 
          str_detect(string = CPC_Class, regex("freshwater", ignore_case = TRUE)) |
          str_detect(string = Name_En, regex("freshwater", ignore_case = TRUE))),
         "Freshwater", "Marine")) %>% 
  mutate(habitat_fixed = case_when(habitat == "Freshwater"~ "Freshwater",
                                   habitat == "Marine ~" ~ "Marine",
                                   T ~ "Marine")) %>% 
  select(-habitat) %>% 
  rename(habitat = habitat_fixed)

no_marine_and_fresh <- spp_info %>% 
  filter(id_level == "Species",
         excluded == "included") %>% 
  group_by(habitat) %>% 
  summarise(n = n())

# link up ISO3 codes
country_prep <- fao_country %>% 
  select(ISO3_Code, UN_Code) %>% 
  rename(
    ISO3 = ISO3_Code,
    COUNTRY = UN_Code
  )

max_val_y <- 8
#### Calcuate national freshwater NEI's and totals ####
#------------------------------------------------------------------------------#
 
national_fresh <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(habitat == "Freshwater" &
         YEAR == 2018 &
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
           YEAR == 2018 &
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
  left_join(national_fresh) # %>% 
  #filter(ISO3 %in% c("USA", "ATA", "ARC", "AUS"))#%>% 
  #mutate_if(is.numeric, replace_na, 0)

test_1 <- st_set_geometry(map_freshwater, NULL)


map_marine <- world_raw %>% 
  #filter(poly_type != "GADM") %>% 
  left_join(national_marine) %>% 
  mutate(total_landed = case_when(
    poly_type == "GADM" ~ NA_real_, 
    TRUE ~ total_landed
  )) #%>% 
  #filter(ISO3 %in% c("USA", "ATA", "ARC", "AUS"))

test_2 <- st_set_geometry(map_marine, NULL)




#### Plotting them up! ####
#------------------------------------------------------------------------------#

# nei tonnage plot
fresh_plot <- ggplot(map_freshwater, aes(fill = log10(total_landed+1)))+
  geom_sf(size = .1) +
  theme_bw() +
  theme(axis.ticks = element_line(colour = "white"),
        axis.text = element_text(color = "white"))


marine_plot <- ggplot(map_marine, aes(fill = log10(total_landed+1)))+
  geom_sf(size = .1) +
  theme_bw() +
  theme(axis.ticks = element_line(colour = "white"),
        axis.text = element_text(color = "white"))

# prop NEI plots
fresh_prop_plot <- ggplot(map_freshwater, aes(fill = prop_nei))+
  geom_sf(size = .1) +
  labs( title = "Freshwater NEI landings in 2018")

marine_prop_plot <- ggplot(map_marine, aes(fill = prop_nei))+
  geom_sf(size = .1) +
  labs(title = "Marine NEI landings in 2018")+
  


patches_prop <- (fresh_prop_plot  + plot_layout(guides = 'keep')) +
                 marine_prop_plot + plot_layout(guides = 'collect')

#patches_prop

patches <- (fresh_plot  + plot_layout(guides = 'keep')) + 
            marine_plot + plot_layout(guides = 'collect')

patches <- patches & 
  #theme_bw() &
  scale_fill_viridis(direction = 1, limits = c(0, max_val_y), option = "magma") &
  scale_y_continuous(expand = c(0,0)) &
  scale_x_continuous(expand = c(0,0)) &
  #theme(axis.ticks = element_line(colour = "white"),
  #      axis.text = element_text(color = "white"))&
  guides(fill = guide_legend(title = "NEI Production\n(log tonnes)",
                             title.hjust = 0.5
                             #title.position = "top", 
                             #title.hjust = 0.5)
  )) #&
  #theme(#
        #legend.title = element_text(hjust = 0.5),
        #legend.title.align = 0.5
        #legend.direction = "horizontal",
        #legend.position = 'bottom',
        #legend.box.just = "center"
        #)
#patches


final_plot <- patches + plot_layout(guides = 'collect')#, ncol = 3#, 
#widths = c(5, 1, 5)#,
# heights = c(1,1,1)
#)




#ggsave(final_plot, "figures/figure_4.png", 
#       width = 11, 
#       height = 8, 
#       device = "png", 
#       units = "in")


#### fresh and marine regression ####
#------------------------------------------------------------------------------#
library(plotly)
library(rnaturalearth)

fresh_combo <- national_fresh %>% 
  select(-COUNTRY) %>% 
  rename(
    fresh_total_landed = total_landed, 
    fresh_total_nei = total_nei, 
    fresh_total_spp = total_spp, 
    fresh_prop_nei = prop_nei
  )

combined <- national_marine %>% 
  select(-COUNTRY) %>% 
  rename(
    marine_total_landed = total_landed, 
    marine_total_nei = total_nei, 
    marine_total_spp = total_spp, 
    marine_prop_nei = prop_nei
  ) %>% 
  inner_join(fresh_combo) %>% 
  mutate(ratio_nei = fresh_prop_nei/marine_prop_nei)



color_comb <- combined %>% 
  mutate(category = case_when(
    fresh_prop_nei >= .5 & marine_prop_nei >= .5 ~ "high_fresh_high_marine",
    fresh_prop_nei >= .5 & marine_prop_nei < .5 ~ "high_fresh_low_marine",
    fresh_prop_nei < .5 & marine_prop_nei >= .5 ~ "low_fresh_high_marine",
    fresh_prop_nei < .5 & marine_prop_nei < .5 ~ "low_fresh_low_marine"
  ))

color_map <- ne_countries(returnclass = "sf") %>% 
  rename(ISO3 = iso_a3) %>% 
  inner_join(color_comb)

ggplot(color_map, aes(fill = category))+
  geom_sf()



ggplot(combined, aes(x = log(fresh_total_landed+1), y = log(marine_total_landed+1)))+
  geom_point()+
  geom_smooth(method = "lm")+
  coord_equal()

ggplot(color_comb, aes(x = fresh_prop_nei, y = marine_prop_nei))+
  geom_point(aes(color = category))#+
  #geom_smooth(method = "lm")

plot <- ggplot(combined, aes(x = log(fresh_total_nei+1), 
                             y = log(marine_total_nei+1), 
                             color = log(marine_total_landed+1),
                             fill = ISO3))+
  geom_point()+
  geom_smooth(method = "lm") +
  coord_equal(ratio = 1)



ggplot(combined, aes(x = log(fresh_total_nei+1), 
                     y = log(marine_total_nei+1), 
                     color = log(marine_total_landed+1)))+
  geom_point()+
  geom_smooth(method = "lm") +
  coord_equal(ratio = 1)


cor(combined$marine_total_nei, 
    combined$fresh_total_nei, 
    method = "pearson", 
    use = "complete.obs")

cor(color_comb$marine_prop_nei, 
    color_comb$fresh_prop_nei, 
    method = "pearson", 
    use = "complete.obs")


cor.test(combined$marine_total_nei, 
         combined$fresh_total_nei)

cor.test(combined$marine_prop_nei, 
         combined$fresh_prop_nei)

fit <- lm(marine_total_nei ~ fresh_total_nei, data = combined)

summary(fit)


#chi test for proportions

chi_df <- color_comb %>% 
  select(marine_total_nei, fresh_total_nei) %>% 
  mutate(mar = round(marine_total_nei))

cstest <- chisq.test(chi_df)

t.test(combined$marine_prop_nei,  combined$fresh_prop_nei, 
       paired = TRUE, alternative = "two.sided")


#### looking at aquaculture and freshwaters ####
#------------------------------------------------------------------------------#
 

## are all of indias freshwater landings really NEI???

check <- combined %>% 
  filter(fresh_prop_nei == 1)

india_check <- fao_production %>% 
  filter(SOURCE == 4) %>% 
  left_join(spp_info) %>% 
  filter(excluded == "included",
         COUNTRY == 356,
         YEAR == 2018, 
         habitat == "Freshwater") #%>%


group_by(YEAR, id_level,) %>% 
  summarise(
    total = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = id_level, values_from = total) %>% 
  mutate(total = Species + Nei, 
         prop_nei = (Nei / total))


india_check <- fao_production %>% 
  mutate(source = if_else(SOURCE == 4, "wild_capture", "aquaculture")) %>% 
  left_join(spp_info) %>% 
  filter(excluded == "included",
         #COUNTRY == 356,
         YEAR == 2018,
         !is.na(habitat)) %>%
  group_by(id_level, source, habitat) %>% 
  summarise(tonnage = sum(QUANTITY, na.rm = TRUE))



nei_aqua <- fao_production %>% 
  filter(SOURCE != 4) %>% 
  left_join(spp_info) %>% 
  filter(excluded ==  "included", 
         YEAR == 2018) %>% 
  group_by(COUNTRY, id_level) %>% 
  summarise(
    tonnage = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  left_join(country_prep) %>% 
  ungroup() %>% 
  pivot_wider(names_from = id_level, values_from = tonnage) %>% 
  mutate(Nei = replace_na(Nei, 0), 
         Species = replace_na(Species, 0), 
         total = Nei + Species,
         percent_nei = Nei/total) %>% 
  select(-COUNTRY) %>% 
  # mutate(Nei = log(Nei+1),
  #        Species = log(Species+1),
  #        total = log(total+1)) %>% 
  
  pivot_longer(-ISO3, 
               names_to = "metric")


basic_map <- ne_countries(returnclass = "sf") %>% 
  rename(ISO3 = iso_a3) %>% 
  inner_join(nei_aqua)


mapping_fxn <- function(x){
  title = paste0(unique(x$metric))
  
  ggplot(x, aes(fill = value))+
    geom_sf()+
    labs(title = title)
}

# these are the aquaculture NEI's
basic_map %>% 
  group_split(metric) %>% 
  map(~mapping_fxn(.))%>% 
  plot_grid(plotlist = ., align = 'hv', ncol = 2)
        


#### aquaculture ####
#------------------------------------------------------------------------------#

nei_aquaculture_compared <- fao_production %>% 
  mutate(source = if_else(SOURCE == 4, "wild_capture", "aquaculture")) %>% 
  left_join(spp_info) %>% 
  filter(excluded ==  "included", 
         YEAR == 2018) %>% 
  group_by(id_level, source) %>% 
  summarise(
    tonnage = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  pivot_wider(source, names_from = id_level, values_from = tonnage) %>% 
  mutate(total = Nei + Species,
         nei_prop = Nei/total)



#### Comparison of wild caught and aquaculture ####
#------------------------------------------------------------------------------#


nei_aqua_comp <- fao_production %>% 
  mutate(source = if_else(SOURCE == 4, "wild_capture", "aquaculture")) %>% 
  left_join(spp_info) %>% 
  filter(excluded ==  "included", 
         YEAR == 2018,
         id_level == "Nei") %>% 
  group_by(COUNTRY, source) %>% 
  summarise(
    tonnage = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  pivot_wider(COUNTRY, names_from = source, values_from = tonnage)

fit_new <- lm(aquaculture ~ wild_capture, data = nei_aqua_comp)

cor.test(nei_aqua_comp$aquaculture, 
         nei_aqua_comp$wild_capture, 
    method = "pearson", 
    use = "complete.obs")

summary(fit_new)

ggplot(nei_aqua_comp, aes(x = log(aquaculture+1), y = log(wild_capture+1)))+
  geom_point()+
  geom_smooth(method = "lm")


nei_props


no_fao_species <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(id_level == "Species") %>% 
  distinct(SPECIES)



no_fao_species <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(YEAR == 2018, 
         SOURCE == 4) %>% 
  group_by(id_level) %>% 
  summarise(
    total = sum(QUANTITY, na.rm = TRUE)
  )




total_comparisons <- fao_production %>% 
  mutate(source = if_else(SOURCE == 4, "wild_capture", "aquaculture")) %>% 
  left_join(spp_info) %>% 
  filter(excluded ==  "included") %>% 
  group_by(id_level, source) %>% 
  summarise(
    tonnage = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  pivot_wider(source, names_from = id_level, values_from = tonnage) %>% 
  mutate(total = Nei + Species) %>% 
  adorn_totals() %>% 
  mutate(nei_pro = Nei/total)
  

#### Bi-plots ####
#------------------------------------------------------------------------------#


country_prep2 <- fao_country %>% 
  select(ISO3_Code, UN_Code, Continent_Group) %>% 
  rename(
    ISO3 = ISO3_Code,
    COUNTRY = UN_Code
  )


test100 <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(YEAR == 2018) %>% 
  filter(excluded == "included") %>% 
  group_by(COUNTRY) %>% 
  summarise(
    
    total_production = sum(QUANTITY,
                     na.rm = TRUE),
    
    marine_nei = sum(QUANTITY[id_level == "Nei" &
                              habitat == "Marine"&
                              SOURCE == 4],
                     na.rm = TRUE),
    
    fresh_nei  = sum(QUANTITY[id_level == "Nei" &
                                habitat == "Freshwater"&
                                SOURCE == 4],
                     na.rm = TRUE),
    
    wild_capture_nei  = sum(QUANTITY[id_level == "Nei" &
                                SOURCE == 4],
                            na.rm = TRUE),
    
    aquaculture_nei  = sum(QUANTITY[id_level == "Nei" &
                                       SOURCE != 4],
                           na.rm = TRUE)) %>% 
  left_join(country_prep2) %>% 
  filter(!is.na(Continent_Group))

max_val_y <- log10(max(test100$total_production) + 1)
max_val_y <- 17.5
max_val_y <- 8

set_theme <- theme_classic()

alpha_level = .7
dot_size = 4

color_pal_biplot = c("#F58F32", "#F5D751", "#F549AD", "#31CDF5", "#3E3DF5")


test100$Continent_Group <- factor(test100$Continent_Group, 
                                  levels = c("Asia", "Africa", "Americas", "Europe", "Oceania"))

fresh_marine_biplot <- ggplot(test100, aes(x = log10(marine_nei+1),
                    y = log10(fresh_nei+1),
                    fill = Continent_Group#,
                   # size = log(total_production+1)
                    ))+
  set_theme+
  scale_fill_manual(values=color_pal_biplot)+
  scale_x_continuous(limits = c(0,max_val_y))+
  scale_y_continuous(limits = c(0,max_val_y))+
  geom_point(alpha = alpha_level,
             pch=21,
             size = dot_size)+
  theme(legend.position = "none")+
  labs(
    x="NEI Marine Production (log tonnes)",
    y="NEI Freshwater Production (log tonnes)"
  )

aq_wc_biplot <-ggplot(test100, aes(x = log10(wild_capture_nei+1),
                    y = log10(aquaculture_nei+1),
                    fill = Continent_Group#,
                    #size = log(total_production+1)
                    ))+
  set_theme+
  scale_fill_manual(values=color_pal_biplot)+
  scale_x_continuous(limits = c(0,max_val_y))+
  scale_y_continuous(limits = c(0,max_val_y))+
  geom_point(alpha = alpha_level,
             pch=21,
             size = dot_size)+
  labs(
    x="NEI Wild Capture Production (log tonnes)",
    y="NEI Aquaculture Production (log tonnes)",
    fill ="Region",
    size = "Total Production\n(log tonnes)"
  )+ guides(fill = guide_legend(override.aes = list(size=5),
                                title.hjust = 0.5),
            size = guide_legend(title.hjust = 0.5))





biplot_patches <- fresh_marine_biplot|aq_wc_biplot
 # plot_layout(guides = 'collect')
  
biplot_patches&
  ggpubr::labs_pubr()

figure_4 <- final_plot/biplot_patches+ 
  plot_annotation(tag_levels = 'A')&
  ggpubr::labs_pubr(base_size = 8)


ggsave(plot = figure_4, 
       device = "tiff",
       filename = "figures/figure_4.tiff",
       dpi = 300,
       units = "cm",
       width = 20,#13,
       height = 15.4)#10)




#### aquaculture numbers ####
#------------------------------------------------------------------------------#


aq_numbers <- fao_production %>% 
  mutate(source = if_else(SOURCE == 4, "wild_capture", "aquaculture")) %>% 
  left_join(spp_info) 

aq_no_species <- aq_numbers %>% 
  filter(id_level == "Species",
         excluded ==  "included",
         source == "aquaculture") %>% 
  distinct(SPECIES, .keep_all = TRUE)

ram_codes <- read_csv("data/RAM_to_FAO.csv") %>% 
  distinct(SPECIES) %>% 
  mutate(RAM = "yes")


iucn_codes <- read_csv("data/IUCN_to_FAO.csv") %>% 
  distinct(SPECIES) %>% 
  mutate(IUCN = "yes")

db_spp <- fao_production %>% 
  distinct(SPECIES) %>% 
  left_join(spp_info) %>% 
  left_join(ram_codes) %>% 
  left_join(iucn_codes) 

no_iucn <- nrow(iucn_codes)
no_ram <- nrow(ram_codes)

aq_db_species <- aq_no_species %>% 
  left_join(db_spp)

aq_iucn <- length(which(aq_db_species$IUCN == "yes"))
aq_ram <- length(which(aq_db_species$RAM == "yes"))
aq_both <- length(which(aq_db_species$RAM == "yes" &
                          aq_db_species$IUCN == "yes"))

aq_iucn/no_iucn
aq_ram/no_ram



aq_no_groups <- aq_no_species %>% 
  group_by(Major_Group) %>% 
  summarise(
    n = n()
  )




tot_no_species <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(id_level == "Species",
         excluded ==  "included") %>% 
  distinct(SPECIES, .keep_all = TRUE)%>% 
  left_join(ram_codes) %>% 
  left_join(iucn_codes) #%>% 
  #filter(is.na(IUCN) | is.na(RAM))


tot_no_spp_no_info <- length(which(is.na(tot_no_species$IUCN)&
               is.na(tot_no_species$RAM)))


tot_no_spp_no_info/length(tot_no_species$SPECIES)


#### chi test for database ####
#------------------------------------------------------------------------------#


db_spp <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(id_level == "Species",
         excluded ==  "included") %>% 
  distinct(SPECIES, .keep_all = TRUE) %>% 
  left_join(ram_codes) %>% 
  left_join(iucn_codes) %>% 
  group_by(Major_Group) %>% 
  summarise(
    RAM = length(which(RAM == "yes")),
    IUCN = length(which(IUCN == "yes"))
  ) %>% 
  ungroup() %>% 
  remove_rownames() %>% 
  column_to_rownames(., "Major_Group")


chisq.test(db_spp)

#### aq and wc overlap map ####
#------------------------------------------------------------------------------#

fao_overlap <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(excluded == "included"&
           id_level == "Species")


countries_wc_species <- fao_overlap %>% 
  filter(SOURCE == 4) %>% 
  distinct(COUNTRY,YEAR, SPECIES) %>% 
  mutate(wild_caught = "yes")


countries_aq_species <- fao_overlap %>% 
  filter(SOURCE != 4) %>% 
  distinct(COUNTRY,YEAR, SPECIES) %>% 
  left_join(countries_wc_species) %>% 
  filter(wild_caught == "yes") %>% 
  group_by(COUNTRY, YEAR) %>% 
  summarise(
    no_spp_aq_wc = n()
  ) %>% 
  left_join(country_prep) 

no_countr_aq_sp <- countries_aq_species %>% 
  distinct(COUNTRY, YEAR) %>% 
  group_by(YEAR) %>% 
  summarise(
    no_spp_aq_wc = n()
  )

points <- ggplot(no_countr_aq_sp, aes(x = YEAR, y = no_spp_aq_wc))+
  geom_point(size = 0.5)+
  scale_y_continuous(limits = c(0,120), expand = c(0,0))+
  geom_line()+
  theme_minimal()+
  labs(
    y = "No. countries with >=1 species\nboth farmed and captured",
    x = "Year"
  )

ggplot(countries_aq_species, aes(x = YEAR, y = no_spp_aq_wc))+
  geom_jitter(alpha = .5)+
  theme_minimal()+
  labs(
    y = "No of Species both\nFarmed and Captured"
  )

countries_data_for_overlap <- countries_aq_species %>% 
  filter(YEAR == 2018)

overlap_map <- world_raw %>% 
  filter(poly_type == "GADM") %>% 
  left_join(countries_data_for_overlap) %>% 
  mutate(no_co = if_else(
    is.na(no_spp_aq_wc), NA_real_,as.numeric(no_spp_aq_wc)
  ))

overlap_spp_plot <- ggplot(overlap_map, aes(fill = no_co))+
  geom_sf(size = .1) +
  theme_bw() +
  scale_fill_viridis(direction = 1, 
                     limits = c(1,32), 
                     breaks = c(1, 10, 20, 30),
                     option = "viridis")+
  theme(axis.ticks = element_line(colour = "white"),
        axis.text = element_text(color = "white"))+
  labs(
    fill = "No of Species both\nFarmed and Captured"
  )+
  #scale_fill_continuous()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))

patch2<- points|overlap_spp_plot

patch2_final <- patch2+plot_annotation(tag_levels = 'A')&
 # ggpubr::labs_pubr()&
  theme(text = element_text(size = 8))
  
ggsave(plot = patch2_final, 
       device = "tiff",
       filename = "figures/figure_2.tiff",
       dpi = 300,
       units = "cm",
       width = 20,#15,
       height = 17.3)#13)
