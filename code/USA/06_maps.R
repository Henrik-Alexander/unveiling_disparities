########################################
# Purpose: Regression modelling        #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

### Settings and packages ---------------------------------------

# States
states <- 1:51
years <- 1969:2004

# Packages
library(usmap)
library(tidyverse)
library(viridis)

# Load the graphics functions
source("functions/graphics.R")

# Funcitons ----------------------------------------------------

# Estimate the mean age at childbearing
calc_mac <- function(asfr, age, na.rm = T) sum(age * age, na.rm = T) / sum(age, na.rm = T)

### Load data --------------------------------------------------

# Fertility data
load("data/asfr_us.Rda")

# Estimate the fertility data
tfr_us <- asfr_us %>% 
  group_by(year, state) %>% 
  summarise(mac_male = calc_mac(asfr_female, age),
            mac_female = calc_mac(asfr_female, age),
            tfr_female = sum(asfr_female),
            tfr_male = sum(asfr_male),
            .groups = "drop")

# US map
us_states <- us_map(regions="states")
us_states <- us_states[, c("x", "y", "full")]

# Merge with the fertility data
map_us <- full_join(tfr_us, us_states, by = c("state" = "full" ), relationship = "many-to-many")

# Estimate the TFR-ratio
map_us$tfr_ratio <- with(map_us, tfr_male / tfr_female)
map_us$mac_diff <- with(map_us, mac_male - mac_female)

# Save the map data for the US
save(map_us, file = "data/map_data/map_us.Rda")

### END ##################################