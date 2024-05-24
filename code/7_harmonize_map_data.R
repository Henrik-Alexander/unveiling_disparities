########################################
# Project: unveiling differences       # 
# Purpose: Regression modelling        #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 29.02.2024                     #
########################################

# Load packages
library(shiny)
library(sf)
library(tidyverse)


# Define the coordinate system
coord_sys <- "EPSG:3035"

# Harmonize the map data ------------------------------------

# Load the data
map_files <- list.files("U:/projects/3 Regional birth squeezes/Subnational_birth_squeezes/data/map_data", full.names = T)
for (file in map_files) load(file)

# Harmonize the data
map_tfr_fra$country <- "France"
map_fin <- rename(map_fin, region = nuts_name)
map_tfr_fra <- rename(map_tfr_fra, region = region_new)
map_tfr_mex <- rename(map_tfr_mex, region = state) |> mutate(country = "Mexico")
map_aus <- rename(map_aus, region = state) |> mutate(country = "Australia")

# Combine the data
map_tfr_fra <- st_transform(map_tfr_fra, coord_sys)
tfr_map_ger <- st_transform(map_tfr_ger, coord_sys)
map_fin <- st_transform(map_fin, coord_sys)
map_tfr_mex <- st_transform(map_tfr_mex, coord_sys)
map_aus <- st_transform(map_aus, coord_sys)

# Combine the data
map_data <- bind_rows(map_tfr_fra, tfr_map_ger, map_fin, map_aus, map_tfr_mex)

# Save the data
save(map_data, file = "data/map_data/combined_map_data.Rda")
