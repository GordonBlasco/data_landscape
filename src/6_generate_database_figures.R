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
library(janitor)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)


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
  mutate(Major_Group2 = factor(Major_Group, levels = c("NEI",
                                                       "AMPHIBIA, REPTILIA", 
                                                       "CRUSTACEA", 
                                                       "INVERTEBRATA AQUATICA",
                                                       "MAMMALIA", 
                                                       "MOLLUSCA",                 
                                                       "PLANTAE AQUATICAE",
                                                       "PISCES"))) %>% 
  arrange(Major_Group2) %>% 
  mutate(SPECIES = fct_inorder(SPECIES))


pal <- c("#D53E4F", "#FC8D59", "#FEE08B", "#FFFFBF", "#E6F598", "#99D594", "#3288BD")
pal2 <- c("grey", pal)


db_tiles <- ggplot(tile_df, aes(x = database, y = SPECIES, fill = Major_Group2))+
  geom_tile() +
  labs_pubr() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_fill_manual(values = pal)+
  theme(legend.position = "none")+
  labs(
    #title = "A) Species presence in datasets",
    x = "",
    y = ""
   ) + 
   theme(
   #plot.title.position = "plot"
   )+
  scale_x_discrete(expand = c(0,0))+
  theme(
    plot.title = element_text(size = 20, colour = "black", lineheight = 1, face = "bold"), 
  )
  


db_tiles



## Database Tonnage Comparison
################################################################################
fao_core <- fao_production %>% 
  left_join(spp_info) %>% 
  filter(YEAR == 2016) %>% 
  filter(SOURCE == 4) 

fao_tonnage <- fao_core %>% 
  filter(FAO == "yes") %>% 
  mutate(Major_fixed = case_when(
    id_level == "Nei" ~ "NEI", 
    id_level == "Species" ~ Major_Group)) %>% 
  group_by(Major_fixed) %>% 
  summarise(tonnage = sum(QUANTITY, na.rm = TRUE)) %>% 
  rename(Major_Group = Major_fixed) %>% 
  mutate(database = "FAO")

iucn_tonnage <- fao_core %>% 
  filter(IUCN == "yes") %>% 
  group_by(Major_Group) %>% 
  summarise(tonnage = sum(QUANTITY, na.rm = TRUE))%>% 
  mutate(database = "IUCN")

ram_tonnage <- fao_core %>% 
  filter(RAM == "yes") %>% 
  group_by(Major_Group) %>% 
  summarise(tonnage = sum(QUANTITY, na.rm = TRUE))%>% 
  mutate(database = "RAM")

all_tonnage <- fao_core %>% 
  filter(RAM == "yes" &
         IUCN == "yes") %>% 
  group_by(Major_Group) %>% 
  summarise(tonnage = sum(QUANTITY, na.rm = TRUE))%>% 
  mutate(database = "In All")


blank_info <- tribble(
  ~database, ~Major_Group, ~tonnage, 
  "blank", "PISCES", 0, 
  "space", "PISCES", 0
)


tonnage_df <- fao_tonnage %>% 
  bind_rows(ram_tonnage,
            iucn_tonnage,
            all_tonnage, 
            blank_info) %>% 
  mutate(Major_Group = factor(Major_Group, levels = c("NEI" ,
                                                      "AMPHIBIA, REPTILIA", 
                                                      "CRUSTACEA", 
                                                      "INVERTEBRATA AQUATICA",
                                                      "MAMMALIA", 
                                                      "MOLLUSCA",                 
                                                      "PLANTAE AQUATICAE",
                                                      "PISCES"))) %>% 
  mutate(database = factor(database, levels = c("space",
                                                "blank", 
                                                "In All", 
                                                "IUCN", 
                                                "RAM",
                                                "FAO"))) %>% 
  mutate(text = case_when(
    database == "FAO" & Major_Group == "PISCES" ~ "FAO ",
    database == "RAM" & Major_Group == "PISCES"~ "RAM ",
    database == "IUCN"& Major_Group == "PISCES" ~ "IUCN ",
    database == "In All"& Major_Group == "PISCES" ~ "All ",
    database == "blank"& Major_Group == "PISCES" ~ "",
    database == "space"& Major_Group == "PISCES" ~ ""
  )) 
  
  



all_biomass_limit <- sum(fao_core$QUANTITY)

geom.text.size = 14
theme.size = (14/5) * geom.text.size


db_ton <- ggplot(tonnage_df, aes(x = database, y = tonnage, fill = Major_Group))+
  geom_bar(width = 0.9, stat="identity", color = "black") +
  coord_polar(theta = "y")+
  scale_y_continuous(limits = c(0,(all_biomass_limit*1.05))) +
  scale_fill_manual(values = pal2)+
 # scale_fill_brewer(palette = "Spectral") +
  #geom_text(data = biomass_plot_df, hjust = 1.02, size = 3.5, aes(x = database, y = 0, label = text)) +
  #ggtitle("B) Tonnage accounted by each database") +
  theme_minimal() +
  theme(#legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  #geom_text(aes(y = total_bio + 0.5,label = total_bio)) + # add value
  geom_text(aes(y = (0),
                hjust = "right",
                label = text,
                fontface = "bold"),
            size = 5)+
  theme(text = element_text(face = "plain", 
                            colour = "black", 
                            size = 14, 
                            #lineheight = 0.9, 
                            #hjust = 0.5, 
                            #vjust = 0.5, 
                            #angle = 0, 
                            #margin = margin(), 
                            debug = FALSE), 
              #axis.text.x = element_text(size = rel(0.86), colour = "black", face = "bold"), 
              #axis.text.y = element_text(size = rel(0.86), colour = "black", face = "bold"), 
              #axis.title = element_text(size = rel(1), colour = "black", face = "bold"), 
              plot.title = element_text(size = 20, colour = "black", lineheight = 1, face = "bold"), 
              legend.title = element_text(size = 14, 
                                          face = "bold", 
                                          colour = "black"), 
              legend.text = element_text(size = rel(0.7), face = "plain", colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))

  

db_ton



patch1 <- db_tiles+db_ton+ plot_layout(guides = 'collect')
  


## Create biomass species comparison - maybe delete -
################################################################################
fao_core <- fao_production 

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


ram_spp_list <- spp_info %>% 
  filter(RAM == "yes") %>%  
  select(Name_En, Scientific_Name, SPECIES)



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


prod_ram <- fao_core %>% 
  filter()

ggplot(prod_none, aes(x = Major_Group, y = value, fill = name))+
  geom_col(position = "dodge")




prod_none1 <- fao_core %>% 
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


ggplot(prod_none1, aes(x = Major_Group, y = biomass, fill = SPECIES))+
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

total_nei_spp_items <- spp_info %>% 
  filter(id_level == "Nei") %>% 
  group_by(Major_Group) %>% 
  summarise(
    nei_species_items = n()
  )


total_spp_ram <- spp_info %>% 
  filter(!is.na(RAM)) %>% 
  group_by(Major_Group) %>% 
  summarise(
    in_ram = n()
  )

total_spp_iucn <- spp_info %>% 
  filter(!is.na(IUCN)) %>% 
  group_by(Major_Group) %>% 
  summarise(
    in_iucn = n()
  )

num_aqua_spp <- fao_production %>% 
  filter(SOURCE !=4) %>% # all species that are farmed
  distinct(SPECIES) %>% 
  left_join(spp_info) %>% 
  filter(id_level == "Species") %>% 
  group_by(Major_Group) %>% 
  summarise(
    aq_prod = n()
  )


total_spp_in_all <- spp_info %>% 
  filter(!is.na(RAM)&
        !is.na(IUCN)) %>% 
  group_by(Major_Group) %>% 
  summarise(
    in_all = n()
  )



final_table <- total_spp %>% 
  left_join(total_nei_spp_items) %>% 
  left_join(total_spp_listed) %>% 
  left_join(total_spp_ram) %>% 
  left_join(total_spp_iucn) %>% 
  left_join(total_spp_in_all) %>% 
  left_join(num_aqua_spp) %>% 
  mutate_if(is.integer, replace_na, 0) %>% 
  adorn_totals() %>% 
  arrange(total_spp) %>% 
  rename(
    "Major group"="Major_Group",
    "Num. total taxa"="total_spp",
    "Num. NEI"="nei_species_items",
    "Num. resolved to species"="specific_species",
    "Included in RAM"="in_ram",
    "Included in IUCN"="in_iucn",
    "Included in Both"="in_all",
    "Aquaculture Produced"="aq_prod"
  )

final_table

layout <- c(
  patchwork::area(t = 1, l = 1, b = 6, r = 5), 
  patchwork::area(t = 1, l = 5, b = 6, r = 11),
  patchwork::area(t = 7, l = 1, b = 9, r = 10)
)
plot(layout)

layout2 <- c(
  patchwork::area(t = 1, l = 1, b = 9, r = 4), 
  patchwork::area(t = 1, l = 5, b = 6, r = 11),
  patchwork::area(t = 7, l = 5, b = 9, r = 11)
)

plot(layout2)

Tmin <- ttheme_minimal()




db_tiles + db_ton + gridExtra::tableGrob(final_table, rows = NULL, theme=Tmin) + 
  plot_layout(design = layout)+ plot_annotation(tag_levels = 'A'#,  tag_suffix = ')'
                                                )


##  supplement tables
################################################################################











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



#### AQUACULTURE ####
#------------------------------------------------------------------------------#
aq_core <- fao_production %>% 
  left_join(spp_info) %>% 
  #filter(YEAR == 2016) %>% 
  filter(SOURCE != 4,
         id_level == "Species")

aq_tot_species <- aq_core %>% 
  distinct(SPECIES, .keep_all = TRUE)

aq_ram <- aq_tot_species %>% 
  filter(RAM == "yes") %>% 
  group_by(Major_Group) %>% 
  summarise(
    no_species = length(unique(SPECIES))
  )

aq_iucn <- aq_tot_species %>% 
  filter(IUCN == "yes") %>% 
  group_by(Major_Group) %>% 
  summarise(
    no_species = length(unique(SPECIES))
  ) %>% 
  adorn_totals()


the_2016_tonnage <- fao_production %>% 
  filter(SOURCE == 4,
         YEAR == 2016) %>% 
  summarise(tonnage = sum(QUANTITY, na.rm = TRUE))


aq_tonnage <- fao_production %>% 
  filter(SOURCE == 4,
         YEAR == 2016) %>% 
  filter(SPECIES %in% aq_tot_species$SPECIES) %>% 
  summarise(tonnage = sum(QUANTITY, na.rm = TRUE))#%>% 
  mutate(database = "IUCN")

