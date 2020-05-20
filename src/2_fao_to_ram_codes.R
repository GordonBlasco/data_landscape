##################################################
## Project: 
## Script purpose: 
## Date:
## Author: Gordon Blasco
##################################################

## Load libraries
##################################################
library(tidyverse)
library(ramlegacy)
library(rfishbase)

## Load data
##################################################

source("src/file_names.R") # Sets file directories 
fao_production <- read_csv(file.path(dir_raw_data, "/FAO/production/TS_FI_PRODUCTION.csv")) # Raw fao data
nei_levels <- read_csv("data/nei_codes.csv")

fao_species <- read_csv(file.path(dir_raw_data, "/FAO/production/CL_FI_SPECIES_GROUPS.csv")) # Species ref
val_fishbase <- read_csv(file.path(dir_data, "fishbase_name_val.csv"))

