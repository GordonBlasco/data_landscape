#------------------------------------------------------------------------------#
## Project :  Data Landscape
## Purpose :  Generate figure 2
##  Date   :  06/22/2020
## Author  :  Gordon Blasco
#------------------------------------------------------------------------------#

library(tidyverse)
library(broom)
library(patchwork)
library(ggpubr)
library(jtools)
library(rworldmap)
source("~/github/aquaculture/src/directories.R") # Sets file directories 

fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/TS_FI_PRODUCTION.csv"), 
                           col_types = cols(COUNTRY = col_character())) # Raw fao data
fao_species    <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/CL_FI_SPECIES_GROUPS.csv")) %>% # Species ref
  rename(SPECIES = "3Alpha_Code")
fao_country    <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/CL_FI_COUNTRY_GROUPS.csv")) # Species ref
fao_neis       <- read_csv("data/nei_codes.csv")

freshwater <- fao_production %>% 
  distinct(SPECIES) %>% 
  left_join(fao_species) %>% 
  filter(ISSCAAP_Group == "Miscellaneous freshwater fishes"|
           ISSCAAP_Group == "Freshwater crustaceans"|
           CPC_Class     == "Freshwater fish, live, fresh or chilled"|
           ISSCAAP_Group == "Freshwater molluscs"|
           SPECIES       == "FSH"|
           SPECIES       == "QEX"|
           SPECIES       == "QPD") %>% 
  pull(SPECIES)

spp_info <- fao_production %>% 
  distinct(SPECIES) %>% 
  left_join(fao_species) %>% 
  left_join(fao_neis) %>% 
  mutate(is_freshwater = if_else(SPECIES %in% freshwater, "freshwater", "marine"))



####  section_name ####
#------------------------------------------------------------------------------#

all_time <- fao_production %>% 
  filter(SOURCE == 4) %>% 
  left_join(spp_info) %>% 
  filter(excluded == "included") %>% 
  group_by(id_level,) %>% 
  summarise(
    total = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = id_level, values_from = total) %>% 
  mutate(total = Species + Nei, 
         prop_nei = (Nei / total)) 

over_time <- fao_production %>% 
  filter(SOURCE == 4) %>% 
  left_join(spp_info) %>% 
  filter(excluded == "included") %>% 
  group_by(YEAR, id_level,) %>% 
  summarise(
    total = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = id_level, values_from = total) %>% 
  mutate(total = Species + Nei, 
         prop_nei = (Nei / total))

ggplot(over_time, aes(x = YEAR, y = Nei))+
  geom_point()+
  geom_line()

prop_plot <- ggplot(over_time, aes(x = YEAR, y = prop_nei))+
  geom_point()+
  geom_line()+
  labs(
    x = "Year", 
    y = "Proportion NEI"
  )

lm_growth_prop <- lm(formula = prop_nei ~ YEAR, over_time)
tidy(lm_growth_prop)
summ(lm_growth_prop)

lm_total_prop <- lm(formula = Nei ~ YEAR, over_time)
tidy(lm_total_prop)
summ(lm_total_prop)


####  section_name ####
#------------------------------------------------------------------------------#
 

by_country <- fao_production %>% 
  filter(SOURCE == 4) %>% 
  left_join(spp_info) %>% 
  group_by(YEAR, COUNTRY, id_level) %>% 
  summarise(
    total = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = id_level, values_from = total) %>% 
  mutate(Nei = if_else(is.na(Nei), 0 , Nei),
         Species = if_else(is.na(Species), 0 , Species)) %>% 
  filter(!(Nei == 0 & Species == 0)) %>% 
  mutate(total = Species + Nei, 
         prop_nei = (Nei / total)*100) %>% 
  mutate(prop_nei = if_else(Nei == 0, 0, prop_nei))

median <- by_country %>% 
  group_by(YEAR) %>% 
  summarise(
    median_prop_nei = median(prop_nei)
  ) %>% 
  ungroup()

ggplot(median, aes(x = YEAR, y = median_prop_nei))+
  geom_point()+
  geom_line()

ggplot(by_country, aes(x = YEAR, y = prop_nei, group = YEAR))+
  geom_boxplot()

lm_median <- lm(formula = median_prop_nei ~ YEAR, data = median)
tidy(lm_median)
summ(lm_median)

confint(lm_median, level = 0.95)



country_prep <- fao_country %>% 
  select(UN_Code, Continent_Group) %>% 
  #mutate(Continent_Group = if_else(UN_Code == 156, "China", Continent_Group)) %>% 
  rename(
    COUNTRY = UN_Code,
    region  = Continent_Group
  ) %>% 
  distinct(COUNTRY, .keep_all = TRUE) %>% 
  filter(!(is.na(region))) 



country_contrib <- fao_production %>% 
  filter(SOURCE == 4) %>% 
  left_join(spp_info) %>% 
  left_join(., country_prep) %>% 
  filter(!(is.na(region))) %>% 
  filter(id_level == "Nei") %>% 
  group_by(YEAR, region) %>% 
  summarise(
    nei_total = sum(QUANTITY, na.rm = TRUE)
  ) %>% 
  mutate(percentage = nei_total / sum(nei_total)) %>% 
  ungroup() %>% 
  mutate(perc = percentage *100)

country_order <- country_contrib %>% 
  filter(YEAR == 2018) %>% 
  arrange(-percentage) %>% 
  pull(region)
country_contrib$region_new <- ordered(country_contrib$region, levels =country_order)


ggplot(country_contrib, aes(x=YEAR, y=percentage, fill=region_new)) + 
  geom_area(alpha=0.6 , size=1, colour="black")



prop_plot <- ggplot(over_time, aes(x = YEAR, y = prop_nei*100))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm", color = "gray40")+
  labs(
    x = "Year", 
    y = "Global NEI\nCapture"#,
   # title = "A) Global Proportion of NEI Resolved Catch"
  )+
  ggpubr::theme_pubclean()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(limits = c(0,43),
                     labels = function(x) paste0(x , '%'))+
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = 'serif'),
    plot.title.position = "plot"
  )

box_plot <- ggplot(by_country, aes(x = YEAR, y = prop_nei, group = YEAR))+
  geom_boxplot()+
  ggpubr::theme_pubclean()+
  labs(
    x = "Year", 
    y = "National NEI\nComposition"#,
    #title = "B) Median of NEI Resolved Catch for All Reporting Countries"
  )+
  scale_x_continuous(expand = c(0,0))+
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = 'serif'),
    plot.title.position = "plot"
  )+
  scale_y_continuous(labels = function(x) paste0(x , '%'))

region_palette = (c("#F58F32", "#F5D751", "#F549AD", "#31CDF5", "#3E3DF5"))

by_region <- ggplot(country_contrib, aes(x=YEAR, y=perc, fill=region_new)) + 
  geom_area(alpha=0.6 , size=1, colour="black")+
  scale_fill_manual(values=region_palette)+
  ggpubr::theme_pubclean()+
  labs(
    x = "Year", 
    y = "Percent NEI\nContribution"#,
    #title = "C) Percent Contribution by Region"
  )+
  scale_x_continuous(expand = c(0,0))+
  theme(
    text = element_text(family = 'serif'),
    plot.title.position = "plot",
    legend.title = element_blank(),
    legend.position = "bottom"
  )+
  scale_y_continuous(labels = function(x) paste0(x , '%'))


figure_3 <- prop_plot/box_plot/by_region &
  labs_pubr(base_size = 16)&
  theme(plot.title.position = "plot",
        legend.title = element_blank(),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_blank())&
  scale_x_continuous(breaks = c(1950, 
                                1970,
                                1990,
                                2010),
                     expand = c(0,0))

fig_3_final <- figure_3 &
  plot_annotation(tag_levels = "A")

fig_3_final
#ggsave(plot = figure_3, filename = "figures/figure_3.png", device = "png")




ggsave(plot = fig_3_final, 
       device = "tiff",
       filename = "figures/figure_3.tiff",
       dpi = 300,
       width = 7.5,
       height = 6)
