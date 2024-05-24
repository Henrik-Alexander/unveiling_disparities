### Spatial analysis  ###################################
# Purpose: Analyse the male and female fertility rates  #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: functions                              #
#########################################################

# Load the packages
library(sf)
library(tidyverse)
library(eurostat)
library(janitor)

# Load the map --------------------------------


# Get the map
map_fin <- eurostat_geodata_60_2016 %>% 
  filter(CNTR_CODE == "FI" & LEVL_CODE == 2) %>% 
  select(NUTS_NAME, geometry) %>% 
  mutate(NUTS_NAME = str_replace(NUTS_NAME, "- ", "-"),
         NUTS_NAME = str_replace(NUTS_NAME, "ä", "a")) %>% 
  clean_names()

# Make the map a bit more rough
map_fin <- st_simplify(map_fin, preserveTopology = TRUE, dTolerance = 100)

# Prepere the Finnish data
fert_fin <- read.csv("Data/regional_tfrs_macs.csv")
fert_fin <- fert_fin |> 
  rename(region = province) |> 
  mutate(tfr_ratio = tfr_male / tfr_female,
         mac_diff = mac_male - mac_female, 
         country = "Finland",
         )

# Merge the data
map_fin <- full_join(map_fin, fert_fin, c("nuts_name" = "region"))

# Plot
ggplot(map_fin, aes(fill = tfr_ratio)) +
  geom_sf() +
  facet_wrap(~ year)

# Save the data
save(map_fin, file = "data/map_data/map_fin.Rda")

### END #############################################