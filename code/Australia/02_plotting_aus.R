########################################
# Purpose: Prepare the australian data #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

# Load the packagges
library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(rnaturalearth)
library(ggspatial)

# Functions
source("Functions/Graphics.R")

# Load the data
load("Data/tfr_aus.Rda")
load("Data/asfr_aus.Rda")

# Load the data world
world <- ne_countries(scale = "medium", returnclass = "sf")

# Set path to australian data
path_aus <- "U:/data/aus/"


### Quality check ----------------------------

# 1. comparison of the ASFR
asfr_aus |> 
  filter(year %in% c(1990, 2020)) |> 
  pivot_longer(cols = starts_with("asfr_"), names_prefix = "asfr_") |> 
  ggplot(aes(x = age_group, y = value, group = name, colour = sex)) +
    geom_point() +
    facet_grid(year ~ region) 


### Estimate the TFR data --------------------

load("Data/tfr_aus.Rda")


### Load the shape file ----------------------

# Load the shape file
map_aus <- read_sf(paste0(path_aus, "shape/STE_2021_AUST_GDA2020.shp"))

# Reduce the data
map_aus <- map_aus |> 
  clean_names() |> 
  select(ste_code21, ste_name21, geometry) |> 
  rename(region_code = ste_code21,
         region = ste_name21) |> 
  mutate(country = "Australia")

# Simplify the map
map_aus <- st_simplify(map_aus, preserveTopology = TRUE, dTolerance = 10000)

# Save the australian map data
save(map_aus, file = "data/map_data/map_aus.Rda")

### Sex differences in fertility --------------

# Plot the trend
ggplot(tfr_aus, aes(x = year, y = tfr_ratio, group = region, colour = region, shape = region)) +
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_point() +
  ylab("male TFR / female TFR") + 
  xlab("Year") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(expand = c(0, 0.2)) +
  scale_colour_viridis_d(name = "") +
  scale_shape_discrete(name = "")

# Save the Plot
ggsave(last_plot(), filename = "Figures/trend_aus_tfr_ratio.pdf", height = 20, width = 25, units = "cm")

### Merge data --------------------------------

# Full join
fert <- tfr_aus |> filter(year %in% c(1990, 2000, 2010, 2020))

# Combine the data
map_aus <- inner_join(shape, fert, by = c("state" = "region"))

# Plot the map of the TFR ratio
map_aus |> 
  ggplot(aes(fill = tfr_ratio)) +
  #geom_sf(data = world, fill = "grey") +
  geom_sf() +
  scale_fill_steps2(low = "#CB181D", mid = "white", high = "darkblue", midpoint = 1, name = "TFR ratio:") +
  ggtitle("TFR-ratio in the states of Australia") +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true",pad_y = unit(0.2, "in"),  style = north_arrow_fancy_orienteering) +
  theme(panel.background =  element_rect(fill = "aliceblue"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(), 
        legend.key.width = unit(2, "cm")) +
  facet_wrap(~ year) +
  coord_sf(xlim = c(100, 170), ylim = c(-5, -45), expand = FALSE)

ggsave(last_plot(), filename = "Figures/map_aus_tfr_ratio.pdf", height = 20, width = 25, unit = "cm")


# Plot the map of the male TFR
map_aus |> 
  ggplot(aes(fill = mac_diff)) +
  #geom_sf(data = world, fill = "grey") +
  geom_sf() +
  scale_fill_steps(low = "grey",high = "#CB181D", name = "MAC difference:") +
  ggtitle("Female TFR in the states of Australia") +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"), style = north_arrow_fancy_orienteering) +
  theme(panel.background =  element_rect(fill = "aliceblue"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        legend.key.width = unit(2, "cm")) +
  facet_wrap(~ year) +
  coord_sf(xlim = c(100, 170), ylim = c(-5, -45), expand = FALSE)

ggsave(last_plot(), filenmame = "Figures/map_aus_tfr_ratio.pdf", height = 20, width = 25, unit = "cm")


### 

tfr_aus |> 
  mutate(capital = if_else(region == "Australian Capital Territory", "Capital", "Non-Capital")) |> 
  group_by(capital, year) |> 
  summarise(across(starts_with("tfr_"), mean)) |> 
  ungroup() |> 
  ggplot(aes(x = year, linetype = capital)) +
  geom_line(aes(y = tfr_female, colour = "Female"), size = 1.5) +
  geom_line(aes(y = tfr_male, colour = "Male"), size = 1.5) +
  scale_colour_manual(name = "TFR:", values = c("navyblue", "firebrick")) +
  scale_y_continuous("Total fertility rate", limits = c(1, 2.2)) 
  
ggsave(last_plot(), filename = "Figures/capital_tfr_aus.pdf")


### END #############################################
