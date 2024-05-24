### Spatial analysis  ###################################
# Purpose: Analyse the male and female fertility rates  #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: functions                              #
#########################################################

### Settings -----------------------------------------------------------------

rm(list = ls())

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")

# Load the fertiltiy data
load("data/asfr_regional_mexico.Rda")

## Functions ------------------------------------------------------------------

calc_tfr <- function(asfr) sum(asfr, na.rm = T)
calc_mac <- function(asfr, age) sum(asfr * age, na.rm = T) / sum(asfr, na.rm = T)

## Data cleaning ---------------------------------------------------

# Estimate the aggregate measures
map_tfr_mex <- asfr_reg %>% 
  group_by(entity, entity_name, year) %>% 
  summarise(tfr_female = calc_tfr(asfr_f),
            tfr_male = calc_tfr(asfr_m),
            mac_female = calc_mac(asfr_f, age),
            mac_male = calc_mac(asfr_m, age),
            .groups = "drop") %>% 
  mutate(tfr_ratio = tfr_male / tfr_female,
         mac_diff = mac_male - mac_female)


# Set the coordinate reference system
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
coord_sys <- "EPSG:3035"


# Load the data world
world <- ne_countries(scale = "medium", returnclass = "sf")

### Data wrangling -----------------------------------------------------------

# Load the shape data
shape <- read_sf("data/mexico_map/00ent.shp")

# Create names
names(shape) <- c("entity", "code", "state", "geometry")

# Mutate the values
shape <- shape |> select(entity, state, geometry) |> 
  mutate(entity = factor(as.integer(entity)))

# Transform the coordinate system
shape <- st_transform(shape, crs = crs)

# Combine the shape data
map_tfr_mex <- inner_join(shape, map_tfr_mex, by = c("entity" = "entity"))
save(map_tfr_mex, file = "data/map_data/map_mex.Rda")

### Plotting  ---------------------------------------------------------------

# Plot
tfr_spat |> filter(year == 2018) |> 
  ggplot(aes(fill = tfr_female)) +
  #geom_sf(data = world, fill = "grey") +
  geom_sf() +
  scale_fill_viridis_c(option = "B", direction = -1, name = "Female TFR") +
  ggtitle("Female TFR in the states of Mexico") +
  #annotation_scale(location = "bl", width_hint = 0.5) + 
  #annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  theme(panel.background =  element_rect(fill = "aliceblue"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  coord_sf(xlim = c(-120, -85), ylim = c(12, 35), expand = FALSE)

# Plot
tfr_spat |> filter(year == 2019) |> 
  ggplot(aes(fill = tfr_m)) +
  geom_sf(data = world, fill = "grey") +
  geom_sf() +
  scale_fill_viridis_c(option = "B", direction = -1, name = "Male TFR") +
  ggtitle("Male TFR in the states of Mexico") + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  theme(panel.background =  element_rect(fill = "aliceblue"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  annotate(geom = "text", x = -91, y = 25, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey22", size = 6 ) +
  coord_sf(xlim = c(-120, -85), ylim = c(12, 35), expand = FALSE)


### Male to female TFR Ratio ---------------------------------------

# Estimate the TFR male to TFR female ratio
map_tfr_mex <- map_tfr_mex |>
                      mutate(tfr_ratio = tfr_m / tfr_f,
                      tfr_ratio_cat = cut(tfr_ratio, breaks = c(0, 0.95, 1.05, 100)))

# Join with spatial information
map_tfr_mex <- inner_join(map_tfr_mex, shape, by = c("entity" = "entity"))

# Plot
plot_panel_ratio <- map_tfr_mex |>
  filter(year %in% c(1990, 2000, 2010, 2020)) |> 
  ggplot(aes(geometry = geometry, fill = tfr_ratio)) +
    geom_sf(data = world, fill = "grey") +
    geom_sf() +
    facet_wrap(~ year, ncol = 2) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1, name = "TFR men / TFR women:") +
    ggtitle("Birth squeezes in the states of Mexico") +
    annotation_scale(location = "bl", width_hint = 0.2) + 
    annotation_north_arrow(location = "bl", pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"), which_north = "true", style = north_arrow_fancy_orienteering) +
    theme(panel.background =  element_rect(fill = "aliceblue"),
          axis.title = element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()) +
  annotate(geom = "text", x = -91, y = 25, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey22", size = 4 ) +
  coord_sf(xlim = c(-120, -85), ylim = c(12, 35), expand = FALSE)

plot_panel_ratio

# save the plot
ggsave(plot_panel_ratio, filename = "Figures/panel_birthsqueeze_mexico.pdf", height = 20, width = 25, units = "cm")


### Compare over time -----------------------------------------


# War oon drugs countries
wod <- c("Chihuahua", "Sinaloa", "Durango", "Guerrero", "Nayarit")

# Plot the trend
map_tfr_mex <- asfr_reg |> 
  group_by(year, entity_name) |> 
  summarise(tfr_ratio = sum(asfr_m) / sum(asfr_f, na.rm = T), .groups = "drop")
  
# Drugs 
tfr_wod <- map_tfr_mex |> 
  filter(entity_name %in% wod)
  
# Non-drugs
tfr_nor <- map_tfr_mex |> 
  filter(entity_name %!in% wod)
  
# Plot the data
ggplot(tfr_nor, aes(x = year, y = tfr_ratio, group = entity_name, colour = entity_name)) +
    geom_hline(yintercept = 1) +
    geom_line(colour = "grey") +
    geom_line(data = tfr_wod) +
    geom_point(data = tfr_wod, aes(shape = entity_name), size = 2) +
    scale_colour_viridis_d(name = "Drug states:") +
    scale_shape_discrete(name = "Drug states:") +
    scale_x_continuous(expand = c(0, 0.1)) +
    scale_y_continuous(trans = "log10") +
    ylab("male TFR / female TFR") +
    xlab("Year")

ggsave(last_plot(), filename = "Figures/mex_war_drugs.pdf")

### END ########################################################################  