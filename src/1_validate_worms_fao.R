# Lightly adapted from code originally written by Juan Mayorga.

# 1. Setup
library(tidyverse)
library(rredlist)
library(taxize)
library(tictoc)
library(worrms)
library(furrr)

taxize_sources <- c(worms = 9, 
                    #fishbase = 155, 
                    eol = 12, 
                    col = 1, 
                    gbif = 11)

source("src/file_names.R") # Sets file directories 
apikey = "PASTE_KEY_HERE"

fao_spp <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/TS_FI_PRODUCTION.csv")) %>% # Raw fao data
  distinct(SPECIES) %>% 
  pull(SPECIES)

# 2. Prepare data
msy_spp <- read_csv(file.path(dir_raw_data, "/FAO/production/2020_1.0/CL_FI_SPECIES_GROUPS.csv")) %>% # Species ref
  rename(alpha_code = `3Alpha_Code`) %>% 
  filter(alpha_code %in% fao_spp) %>% 
  select(alpha_code, Scientific_Name) %>% 
  rename(species = Scientific_Name) 

pipe_sleep <- function(x){
  Sys.sleep(10)
  return(x)
}

poss_resolve <- possibly(taxize::gnr_resolve, otherwise = "failed")

# 3. Resolve species names

## Get resolved species names
tictoc::tic()
msy_spp_resolved <- taxize_sources %>% 
  map(~ taxize::gnr_resolve(data_source_ids = .x, 
                            names = msy_spp$species,
                            best_match_only = T, 
                            fields = c("all"), 
                            canonical = T)) %>% 
  ## Convert to data frame
  map(~ .x %>%
        #filter(!match_value %in% c("Could only match genus") & str_count(matched_name2, "\\w+") >= 2) %>% 
        filter(!match_value %in% c("Could only match genus")) %>% # str_count() above removes one word taxa
        select(user_supplied_name, matched_name2, taxon_id)) %>% 
  reduce(full_join, by = "user_supplied_name") %>% 
  set_names(c("user_supplied_name", 
              "worms_sci_name", 
              "worms_id", 
              #"fishbase_sci_name", 
              #"fishbase_id", 
              "eol_sci_name", 
              "eol_id", 
              "col_sci_name", 
              "col_id",
              "gbif_sci_name", 
              "gbif_id"))
tictoc::toc()


## Select a preferred resolved name based on dataset hierarchy
msy_spp_pref <- msy_spp_resolved %>% 
  mutate(resolved_scientific_name = case_when(
    !is.na(worms_sci_name) ~ worms_sci_name,
    #!is.na(fishbase_sci_name) ~ fishbase_sci_name,
    !is.na(eol_sci_name) ~ eol_sci_name,
    !is.na(col_sci_name) ~ col_sci_name,
    TRUE ~ user_supplied_name
  )) %>% 
  select(user_supplied_name, resolved_scientific_name, everything()) 

## Join back to original species list
msy_spp <- msy_spp %>% 
  left_join(msy_spp_pref %>% 
              select(user_supplied_name, resolved_sci_name = resolved_scientific_name), 
            by = c("species" = "user_supplied_name")) %>% 
  select(species, resolved_sci_name, everything())

## How many NAs are there? Replace them with the supplied taxon name where they exist.
msy_spp %>% map_df(~sum(is.na(.)))

# No NAs, so no need to coalesce.
msy_spp <- msy_spp %>% 
   mutate(resolved_sci_name = coalesce(resolved_sci_name, species)) 


# 4. Validate names and get taxonomic info from WoRMS ----

get_worms_info <- function(sci_name){
  worrms::wm_records_name(name = sci_name) %>% 
    mutate(provided_sci_name = sci_name) %>% 
    select(provided_sci_name, scientificname, status, valid_name, kingdom, phylum, class, order, family, genus)
}

## Get taxonomy from WoRMS in parallel
plan(multiprocess)

tictoc::tic()
msy_worms_info <- msy_spp %>%
  pull(resolved_sci_name) %>% 
  furrr::future_map(safely(get_worms_info), .progress = TRUE) %>% 
  transpose()
tictoc::toc()

## Pull out the results and bind into data frame
msy_worms_info <- msy_worms_info$result %>% 
  compact() %>% 
  bind_rows()

## First, take those entries that have no NAs, where the name queried matches the name supplied, and when there are only one valid name per entry
valid_msy_worms_info <- msy_worms_info %>% 
  filter(!is.na(scientificname), !is.na(valid_name)) %>% # Delete entries with NAs
  group_by(provided_sci_name) %>% 
  filter(provided_sci_name == scientificname) %>% # Keep only rows that match the sci_name we supplied
  filter(n_distinct(valid_name, na.rm = T) == 1) %>%  # Get those for which there is only one valid name
  ungroup() %>% 
  group_by(provided_sci_name) %>% 
  summarise_at(vars(valid_name, kingdom, phylum, class, order, family, genus), .funs = unique)

## Second, from those with multiple valid names per row, lets get only the row for which status == "accepted", which means that the supplied name was already valid
accepted_msy_worms_info <- msy_worms_info %>% 
  filter(!is.na(scientificname), !is.na(valid_name)) %>% # Delete entries with NAs
  group_by(provided_sci_name) %>% 
  filter(provided_sci_name == scientificname) %>% # Keep only rows that match the sci_name we supplied
  filter(n_distinct(valid_name, na.rm = T) > 1) %>% 
  filter(status == "accepted") %>% 
  group_by(provided_sci_name) %>% 
  summarise_at(vars(valid_name, kingdom, phylum, class, order, family, genus), .funs = unique)

## Add WoRMS validated names back to original species list
msy_spp <- msy_spp %>% 
  left_join(bind_rows(accepted_msy_worms_info,
                      valid_msy_worms_info),
            by = c("resolved_sci_name" = "provided_sci_name")
  )

## How many original Reg names are not equal to the WoRMS valid name? 
msy_spp %>% 
  filter(!is.na(valid_name))

## How many NAs? Replace NAs in validated name with the resolved name
msy_spp %>% map_df(~sum(is.na(.)))

msy_spp <- msy_spp %>% 
  mutate(valid_sci_name = coalesce(valid_name, resolved_sci_name)) %>% 
  select(species, resolved_sci_name,  valid_sci_name, everything(), -valid_name)


final_names <- msy_spp %>% 
  select(alpha_code, resolved_sci_name) %>% 
  distinct(alpha_code, .keep_all = TRUE)
  
  
write_csv(final_names, "data/worms_validated_names.csv")
  
  
  