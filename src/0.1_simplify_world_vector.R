#------------------------------------------------------------------------------#
## Project :  Data Landscape
## Purpose :  Simplify world vector
##  Date   :  6/19/2020
## Author  :  Gordon Blasco
#------------------------------------------------------------------------------#
library(sf)
library(rmapshaper)

# read in the world 
world_fn           <- "/home/shares/clean-seafood/raw_data/world_vec/world_vector.shp"
world_raw          <- st_read(world_fn)

# simplify the world plot
world_simp         <- rmapshaper::ms_simplify(st_geometry(world_raw), keep = 0.2)

# save it to the server
st_write(world_simp, "/home/shares/clean-seafood/raw_data/world_vec/simplified_world.shp")
