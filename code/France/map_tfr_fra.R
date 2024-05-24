#######################################
# Project: Subnational birth squeeezs #
# Purpose: France map                 #
# Author: Henrik-Alexander Schubert   #
# E-mail: schubert@demogr.mpg.de      #
########################################

# Load the packages
library(tidyverse)
library(sf)

# Load the data
load("data/asfr_fra.Rda")

# Funcitons ------------------------------------------------

calc_tfr <- function(asfr) sum(asfr, na.rm = T)

calc_mac <- function(asfr, age) sum(asfr * age, na.rm = T) / sum(asfr, na.rm = T)

# Clena the fertility data -------------------------------------

# Estimate the aggregate indicators
fert_fra <- asfr_fra %>% 
  group_by(year, region_new) %>% 
  summarise(tfr_female = calc_tfr(asfr_female),
            tfr_male = calc_tfr(asfr_male),
            mac_male = calc_mac(asfr_male, age),
            mac_female = calc_mac(asfr_female, age), .groups = "drop") %>% 
  mutate(tfr_ratio = tfr_male / tfr_female,
         mac_diff = mac_male  - mac_female)

# Clean the map data -------------------------------------------

# Load the map data
map_fra <- read_sf("data/france_map/FRA_adm1.shp")
map_fra <- map_fra %>% 
  rename(region = NAME_1,
         country = NAME_0) %>% 
  select(region, country, geometry)


# Rename the regions
map_fra$region_new <- NA
map_fra$region_new[map_fra$region %in% c("Auvergne", "Rhône-Alpes")] <- "Auvergne-Rhône-Alpes" 
map_fra$region_new[map_fra$region %in% c("Bourgogne", "Franche-Comté")] <- "Bourgogne-Franche-Comté" 
map_fra$region_new[map_fra$region %in% c("Bretagne")] <- "Bretagne" 
map_fra$region_new[map_fra$region %in% c("Centre-Val de Loire", "Centre")] <- "Centre-Val-de-Loire" 
map_fra$region_new[map_fra$region %in% c("Corse")] <- "Corse" 
map_fra$region_new[map_fra$region %in% c("Alsace", "Champagne-Ardenne", "Lorraine")] <- "Grand Est" 
map_fra$region_new[map_fra$region %in% c("Nord-Pas-de-Calais", "Picardie")] <- "Hauts-de-France" 
map_fra$region_new[map_fra$region %in% c("Île-de-France")] <- "Île-de-France" 
map_fra$region_new[map_fra$region %in% c("Languedoc-Roussillon", "Midi-Pyrénées")] <- "Occitanie" 
map_fra$region_new[map_fra$region %in% c("Pays de la Loire")] <- "Pays de la Loire" 
map_fra$region_new[map_fra$region %in% c("Aquitaine", "Poitou-Charentes", "Limousin")] <- "Nouvelle-Aquitaine" 
map_fra$region_new[map_fra$region %in% c("Haute-Normandie", "Basse-Normandie")] <- "Normandie" 
map_fra$region_new[map_fra$region %in% c("Provence-Alpes-Côte d'Azur")] <- "Provence-Alpes-Côte d'Azur" 
map_fra$region_new[map_fra$region %in% c("Guadeloupe")] <- "Guadeloupe" 
map_fra$region_new[map_fra$region %in% c("Martinique")] <- "Martinique" 
map_fra$region_new[map_fra$region %in% c("Guyane")] <- "Guyane" 
map_fra$region_new[map_fra$region %in% c("DOM")] <- "DOM" 
map_fra$region_new[map_fra$region %in% c("La Réunion")] <- "La Réunion"
map_fra$region_new[map_fra$region %in% c("Mayotte")] <- "Mayotte" 

# Aggregate the map
map_fra <- map_fra %>% group_by(region_new) %>% summarise()

# Combine with the fertility data
map_tfr_fra <- inner_join(map_fra, fert_fra)

# Save the data
save(map_tfr_fra, file = "data/map_data/map_fra.Rda")

### END ################################################