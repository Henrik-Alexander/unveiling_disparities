#######################################
# Project: Subnational birth squeeezs #
# Purpose: Germany map                #
# Author: Henrik-Alexander Schubert   #
# E-mail: schubert@demogr.mpg.de      #
########################################

rm(list = ls())

# Load the packages
library(sf)
library(tidyverse)

# Load the functions
source("functions/functions.R")
source("functions/graphics.R")

# Load the  fertility data
load("data/fertility_germany.Rda")
load("data/development.Rda")
map <- read_sf(list.files("data/germany_map", pattern = ".shp$", full.names = T))

# Change the theme
theme_update(
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  legend.key.width = unit(2, "cm")
)

# Wrangling ---------------------------------------------

# Rename the columns
map <- map %>% 
  select(gen, geometry) %>% 
  rename(region = gen)

# Rename the 
dev <- dev %>% 
  filter(country == "Germany") %>% 
  mutate(region = str_replace(region, "Thur", "Thür"),
         region = str_replace(region, "Wurtt", "Württ"))

# Plot the maps ------------------------------------------

# Merge map with development data
reg_map <- inner_join(map, dev)

# Plot the map for human development
ggplot(subset(reg_map, year %in% seq(1990, 2020, 10)), aes(fill = hdi)) +
  geom_sf() +
  facet_wrap(~ year) +
  scale_fill_steps(name = "HDI", low = "grey", high = "darkred")
figs(last_plot(), "ger_map_dev", height = 25, width = 15)

# Plot the map for the gender development
ggplot(subset(reg_map, year %in% seq(1990, 2020, 10)), aes(fill = gdi)) +
  geom_sf() +
  facet_wrap(~ year) +
  scale_fill_steps(name = "GDI", low = "grey", high = "darkred")
figs(last_plot(), "ger_map_dev", height = 25, width = 15)

# Plot the fertility maps ---------------------------------

# Prepare the fertility data
fert_deu <- fert_deu %>% 
  pivot_wider(names_from = "sex", values_from = c("tfr", "mac")) %>% 
  mutate(mac_diff = mac_male - mac_female,
         tfr_ratio = tfr_male / tfr_female,
         country = "Germany") |> 
  rename(region = bundesland) 

# Merge map with development data
map_tfr_ger <- inner_join(map, fert_deu, c("region"))

# Plot the map for human development
ggplot(subset(map_tfr_ger, year %in% c(1995, 2005, 2018)), aes(fill = tfr_female)) +
  geom_sf() +
  facet_wrap(~ year) +
  scale_fill_steps(name = "TFR female", low = "grey", high = "darkred", n.breaks = 10)
figs(last_plot(), "ger_map_tfr")

# Plot the map for the gender development
ggplot(subset(map_tfr_ger, year %in% c(1995, 2005, 2018)), aes(fill = mac_female)) +
  geom_sf() +
  facet_wrap(~ year) +
  scale_fill_steps(name = "Mean age of childbearing (female)", low = "grey", high = "darkred", n.breaks = 5)
figs(last_plot(), "ger_map_mac")


# Plot the ratios -----------------------------------------------

# Plot the map for human development
ggplot(subset(map_tfr_ger, year %in% c(1995, 2005, 2018)), aes(fill = tfr_ratio)) +
  geom_sf() +
  facet_wrap(~ year) +
  scale_fill_steps2(name = "Male TFR/Female TFR", low = "darkblue", mid = "grey", high = "darkred", midpoint = 1, n.breaks = 6)
figs(last_plot(), "ger_map_tfr_ratio")

# Plot the map for the gender development
ggplot(subset(map_tfr_ger, year %in% c(1995, 2005, 2018)), aes(fill = mac_diff)) +
  geom_sf() +
  facet_wrap(~ year) +
  scale_fill_steps(name = "Sex differences in mean age of childbearing", low = "grey", high = "darkred", n.breaks = 6)
figs(last_plot(), "ger_map_mac_diff")

# Save the map data
save(map_tfr_ger, file = "data/map_data/map_ger.Rda")

### END ##########################################################