########################################
# Purpose: Reviewer comments           #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 06.02.2026                     #
########################################

rm(list = ls())

library(data.table)
library(tidyverse)
library(sf)
library(usmap)

# Load the graphics package
source("functions/Graphics.R")

# Load the fertility data
source("functions/paths.R")

# Load the fertility data for the subnational units
lapply(paths_countries, function(path) {
  if (str_detect(path, ".csv")) {
    fert_fin <- read.csv(paths_countries$path_fin,
                         encoding = "latin1")
  } else {
    load(path, envir = .GlobalEnv)
  }
})

# Reviewer 1 ===================================================================



# Reviewer 2 ===================================================================

# R2 C1 ------------------------------------------------------------------------

# Purpose: Look at the subnational and national data
# Aggregate the subnational data

# Redistribute the male population as much as possible so that the data has 
# the smallest variation in sex ratio
asfr_us <- as.data.table(asfr_us)
df <- asfr_us[year==2004, ]
setnames(df, "state", "region")

# Create an age vector
ages <- sort(unique(df$age))

## Create the data formats -----------------------------------------------------

# Create the matrices
mat_pop_f <- as.matrix(dcast(df, formula = "age ~ region", value.var="pop_female"))[, 2:52]
mat_pop_m_count <- mat_pop_m <- as.matrix(dcast(df, formula = "age ~ region", value.var="pop_male"))[, 2:52]
mat_asfr_f <- as.matrix(dcast(df, formula = "age ~ region", value.var="asfr_female"))[, 2:52]
mat_asfr_m <- as.matrix(dcast(df, formula = "age ~ region", value.var="asfr_male"))[, 2:52]
mat_sr <- mat_pop_m / mat_pop_f

# The goal sr
goal_sr <- rowSums(mat_pop_m) / rowSums(mat_pop_f)
overall_sr <- sum(mat_pop_m) / sum(mat_pop_f)

## Start the algorithm ---------------------------------------------------------

# Set the baseline number of redistributed men
redist_men <- Inf
i <- 0

# Set a factor for the redistribution rate
lambda <- 0.2

while(i < 1000 & redist_men > 15000) {
  
  #if(i %% 10 == 0) {
    cat("Iteration:", i, '\n')
  #}
  
  # Estimate the deviation
  dev <- apply(mat_sr, 2, function(x) log(x / goal_sr))
  
  # Find the position of the state that has the largest age-specific deviation from the target SR
  pos_max_dev <- t(apply(dev, 1, function(x) x == max(x)))
  pos_min_dev <- t(apply(dev, 1, function(x) x == min(x)))
  
  # Ensure positive redistributions
  dev[dev<0] <- 0
  
  # Redistribute the men from the largest deviation to the smallest deviation
  reshuffle_men <- dev[pos_max_dev] * mat_pop_m_count[pos_max_dev]
  
  # Adjuste for the redistribution rate
  reshuffle_men <- round(reshuffle_men * lambda)
  
  # redistribute the men
  mat_pop_m_count[pos_max_dev] <- mat_pop_m_count[pos_max_dev] - reshuffle_men 
  mat_pop_m_count[pos_min_dev] <- mat_pop_m_count[pos_min_dev] + reshuffle_men
  
  # Re-estimate the sex ratio for the new population structure
  mat_sr <- mat_pop_m_count / mat_pop_f
  
  ## Reshuffle men from negative deviations (excess of men) to positive (excess ) ------------------------
  
  # Re-estimate the deviations
  dev <- apply(mat_sr, 2, function(x) log(goal_sr/x))
  
  # Find the position of the state that has the largest age-specific deviation from the target SR
  pos_max_dev <- t(apply(dev, 1, function(x) x == max(x)))
  pos_min_dev <- t(apply(dev, 1, function(x) x == min(x)))
  
  # Ensure positive redistributions
  dev[dev<0] <- 0
  
  # Redistribute the men from the largest deviation to the smallest deviation
  reshuffle_men <- dev[pos_max_dev] * mat_pop_m_count[pos_max_dev]
  
  # Adjuste for the redistribution rate
  reshuffle_men <- round(reshuffle_men * lambda)
  
  # redistribute the men
  mat_pop_m_count[pos_max_dev] <- mat_pop_m_count[pos_max_dev] + reshuffle_men 
  mat_pop_m_count[pos_min_dev] <- mat_pop_m_count[pos_min_dev] - reshuffle_men
  
  # Increase the counter
  i <- i + 1
  
  # Re-estimate the sex ratio for the new population structure
  mat_sr <- mat_pop_m_count / mat_pop_f
  
  # Re-estimate the deviations
  dev <- apply(mat_sr, 2, function(x) log(x / goal_sr))
  
  # Estimate the total number of men that could be redistributed
  redist_men <- sum(abs(dev[dev<0]) * mat_pop_m_count[dev<0]) + sum(dev[dev>0] * mat_pop_m[dev>0])
  
}


# Plot the sex ratios
sr <- mat_pop_m_count / mat_pop_f
plot(ages, sr[, 1], type="l")
for(i in 2:ncol(sr)) {
  lines(ages, sr[, i])
}
points(ages, goal_sr, col="red")




# R2 C4 ------------------------------------------------------------------------

# Define the coordinates
min_x <- 0.5
max_x <- 1.1
min_y <- 0.5
max_y <- 1.1

# Define the position of the subplot window
min_y_subplot <- 3
max_y_subplot <- 8
min_x_subplot <- 0.55
max_x_subplot <- 1.7


# Replot the Figure 2
main <- ggplot(fert_subnat, aes(x=tfr_female, y=tfr_male, colour = country, shape = country)) +
  geom_rect(aes(xmin = min_x, xmax=max_x, ymin = min_y, ymax=max_y), fill=NA, colour="black") +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(alpha = 0.5, size = 0.5) +
  scale_x_log10("TFR female", n.breaks = 15) +
  scale_y_log10("TFR male", n.breaks = 15) +
  scale_colour_viridis_d("Country") +
  scale_shape_manual("Country", values = 13:20) +
  theme(legend.position = c(0.8, 0.2)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  geom_segment(aes(x = min_x, y = max_y, xend = min_x_subplot, yend = min_y_subplot), colour = "grey") +
  geom_segment(aes(x = max_x, y = max_y, xend = max_x_subplot, yend = min_y_subplot), colour = "grey")
  


# Create the subplot
subplot_bl <- ggplot(fert_subnat, aes(x=tfr_female, y=tfr_male, colour = country, shape = country)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(alpha = 0.5, size = 2) +
  scale_x_log10("", breaks = c(0.6, 0.7, 0.8, 0.9, 1), expand = c(0, 0)) +
  scale_y_log10("", breaks = c(0.6, 0.7, 0.8, 0.9, 1), expand = c(0, 0)) +
  scale_colour_viridis_d("Country") +
  scale_shape_manual("Country", values = 13:20) +
  coord_cartesian(xlim = c(min_x, max_x), ylim = c(min_y, max_y)) +
  guides(colour = "none", shape = "none") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

# Combine the two graphs
main + annotation_custom(ggplotGrob(subplot_bl),
                         xmin = min_x_subplot, 
                         xmax = max_x_subplot,
                         ymin = min_y_subplot,
                         ymax = max_y_subplot)

# Save the figure
ggsave("figures/tfr_ratios.svg", height=10, width=10)

# Reviewer 3 ===================================================================

## R3. C1. ----------------------------------------------------------------------

## Note: three maps are missing: US, Spain and Colombia

# Load the country-data
source("functions/paths.R")

# Load the fertility data for the subnational units
lapply(paths_countries, function(path) {
  if (str_detect(path, ".csv")) {
    fert_fin <- read.csv(paths_countries$path_fin,
                         encoding = "latin1")
  } else {
    load(path, envir = .GlobalEnv)
  }
})


## Plot the TFR ratios by surface area

# Load the map data
load("data/map_data/combined_map_data.Rda")

# Select the important columns from the map data
map_data <- map_data[, c("country", "region", "year", "tfr_female", "tfr_male", "tfr_ratio")]

## Load the US map
us_map <- usmap::us_map(regions="state")
tfr_usa <- as.data.table(asfr_us)
tfr_usa <- tfr_usa[, .(tfr_male = sum(asfr_male),
                       tfr_female = sum(asfr_female),
                       tfr_ratio = sum(asfr_male) / sum(asfr_female)),
                   by = .(state, year)]
us_map <- merge(us_map, tfr_usa, by.x="full", by.y="state")
us_map <- us_map[, c("full", "year", "tfr_male", "tfr_female", "tfr_ratio")]
us_map <- us_map |> rename(region = full) |> mutate(country="United States")
us_map <- st_transform(us_map, "EPSG:3035")
st_geometry(us_map) <- "geometry"

## Load the spanish map
map_files_spain <- list.files("data/map_spain_provinces", ".shp$", full.names=T)
map_spain <- read_sf(map_files_spain)
map_spain <- map_spain[, 1:3]
asfr_esp <- asfr_esp_reg[asfr_esp_reg$region != "", ]
asfr_esp <- as.data.table(asfr_esp)
tfr_esp <- asfr_esp[, .(country = "Spain",
                        tfr_female = sum(asfr_female, na.rm=T),
             tfr_male = sum(asfr_male, na.rm=T),
             tfr_ratio = sum(asfr_male, na.rm=T)/sum(asfr_female, na.rm=T)),
         by = .(region, year)]
source("functions/rename_esp_to_eng.R")
map_spain$region <- recode_spanish_provinces(map_spain$Official_Na)
map_spain <- map_spain[, "region"]
map_esp <- merge(map_spain, tfr_esp, by="region", all.x=T, all.y=T)
map_esp <- st_transform(map_esp, "EPSG:3035")

## Load the Colombian map
map_col <- read_sf(list.files("data/columbia_map/", pattern=".shp$", full.names=T)[2])
map_col <- map_col[, c("ADM1_ES")]
tfr_col <- asfr_col[, .(country="Colombia",
                        tfr_ratio = sum(asfr_male, na.rm=T)/sum(asfr_female, na.rm=T),
                        tfr_male = sum(asfr_male * middle_age, na.rm=T),
                        tfr_female = sum(asfr_female * middle_age, na.rm=T)), by=.(region, year)]

# Harmonize the region names
map_col$ADM1_ES[map_col$ADM1_ES == "Bolívar"] <- "Bolivar"
map_col$ADM1_ES[map_col$ADM1_ES == "Archipiélago de San Andrés, Providencia y Santa Catalina"] <- "Archipelago of San Andrés, Providencia and Santa Catalina"
map_col$ADM1_ES[map_col$ADM1_ES == "Chocó"] <- "Choco"
map_col$ADM1_ES[map_col$ADM1_ES == "Bogotá, D.C."] <- "Bogotá"
map_col$ADM1_ES[map_col$ADM1_ES == "Córdoba"] <- "Cordoba"
map_col$ADM1_ES[map_col$ADM1_ES == "La Guajira" ] <- "La guajira"
map_col$ADM1_ES[map_col$ADM1_ES == "Valle del Cauca"] <- "Valle del Cauca"
map_col$ADM1_ES[map_col$ADM1_ES == "Caquetá"] <- "Caqueta"

# Rename the spanish names into english
col <- merge(map_col, tfr_col, by.x="ADM1_ES", by.y="region", all.x=T, all.y=F)
col <- col |> rename(region = ADM1_ES)

# Set the coordinate system
col <- st_transform(col, "EPSG:3035")

## Combine the maps ------------------------------------------------------------

# Bind rows
map_data <- bind_rows(map_data, col, map_esp, us_map)

# Check and transform to a projected CRS (e.g., UTM) for accurate area
shape_proj <- st_transform(map_data, 32633)

# Estimate the survface area
map_data$surface_area <- as.numeric(st_area(shape_proj))/1e+6

# Plot the TFR ratios by population size ---------------------------------------

# Estimate the population size for Colombia
pop_size_col <- asfr_col[, .(pop_size = sum(pop_males + pop_females)), by = .(region, year)]

# Estimate the population size for Germany
asfr_deu <- as.data.table(asfr_deu)
pop_size_deu <- asfr_deu[, .(pop_size = sum(exposure_female + exposure_male, na.rm=T)), by = .(region, year)]

# Estimate the population size for the US
asfr_us <- as.data.table(asfr_us)
pop_size_usa <- asfr_us[, .(pop_size = sum(pop_female + pop_male)), by = .(state, year)]
setnames(pop_size_usa, old="state", new="region")

# Estimate the population size for Spain
pop_size_esp <- asfr_esp_reg[region != "", .(pop_size = sum(exposure_female + exposure_male)), by = .(region, year)]

# Estimate the population size for France
pop_size_fra <- asfr_fra[, .(pop_size = sum(exposure_female + exposure_male, na.rm=T)), by = .(region, year)]

# Estimate the population size for Australia
load("U:/data/aus/fertility_rates/data/asfr_pop_aus.Rda")
d <- as.data.table(d)
pop_size_aus <- d[, .(pop_size=sum(exposure_male + exposure_female, na.rm=T)), by = .(region, year)]

# Estimate the population size for Finland
pop_size_fin <- fread("raw/pop_finland_reproductive_age.csv")
pop_size_fin <- pop_size_fin[, .(pop_size = sum(`Total Population 31 Dec`)), by = .(Area, Year)]
setnames(pop_size_fin, old=names(pop_size_fin), new=c("region", "year", "pop_size"))

# Estimate the population size for Mexico
asfr_mex_reg <- as.data.table(asfr_mex_reg)
pop_size_mex <- asfr_mex_reg[, .(pop_size = sum(exposure_female + exposure_male, na.rm=T)), by = .(region, year)]

# Combine the population sizes
pop_size <- rbindlist(mget(ls(pattern = "^pop_size_[a-z]{3}")))

# Merge the population size with the map data
map_data <- merge(map_data, pop_size, by=c("region", "year"), all=T)

# Estimate the populaton density
map_data$pop_density <- map_data$pop_size / map_data$surface_area

# Filter the non-missing observations
map_data <- map_data[!is.na(map_data$tfr_ratio), ]

## Plot the results ----------------------------

# Plot the relationship between surface area and tfr ratio
ggplot(data = map_data, aes(x=surface_area, y=tfr_ratio)) +
  geom_smooth(aes(group=country, colour=country), method = "lm", se=F) +
  geom_hline(yintercept = 1) +
  geom_point(aes(colour = country)) +
  geom_line(aes(colour=country, group = region), alpha=0.3, linewidth=0.5) +
  scale_x_log10("Surface area (km^2)", labels = scales::unit_format(unit = "K", scale = 1e-3), n.breaks=10) +
  scale_y_log10("TFR ratio (TFR men / TFR women)", n.breaks=10) +
  scale_colour_viridis_d("Country")

# Plot the relationship between surface area and tfr ratio
ggplot(data = map_data, aes(x=pop_size, y=tfr_ratio, colour=country)) +
  geom_smooth(aes(group=country), method = "lm", se=F) +
  geom_hline(yintercept = 1, linetype="dashed") +
  geom_point() +
  #geom_line(aes(colour=country, group = region), alpha=0.3, linewidth=0.5) +
  scale_x_log10("Population size (at reproductive age)", labels = scales::unit_format(unit = "K", scale = 1e-3), expand=c(0, 0)) +
  scale_y_log10("TFR ratio (TFR men / TFR women)", n.breaks=10) +
  scale_colour_viridis_d("Country")


# Plot the TFR ratios by population density = population size / surface area

# Plot the relationship between surface area and tfr ratio
ggplot(data = map_data, aes(x=pop_density, y=tfr_ratio, colour=country)) +
  geom_smooth(aes(group=country), method = "lm", se=F) +
  geom_hline(yintercept = 1) +
  geom_point() +
  geom_line(aes(colour=country, group = region), alpha=0.3, linewidth=0.5) +
  scale_x_log10("Population density (population/km^2)", expand=c(0, 0)) +
  scale_y_log10("TFR ratio (TFR men / TFR women)") +
  scale_colour_viridis_d("Country")

### END ########################################################################