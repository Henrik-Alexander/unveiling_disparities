########################################
# Project: Subnational birth squeezes #
# Purpose: Clean columbia data        #
# Author: Henrik-Alexander Schubert   #
# E-mail: schubert@demogr.mgp.de      #
# Date: 05.06.2024                    #
#######################################

# Load the packages
library(tidyverse)
library(data.table)
library(sf)

# Load the graphics
source("functions/graphics.R")

# Load the data
load("U:/data/col/pop/population_columbia.Rda")
load("data/births_columbia.Rda")

# Load the map
map_col <- read_sf("data/columbia_map/col_admbnda_adm1_mgn_20200416.shp")
map_col <- map_col[, c("ADM1_ES", "geometry")]
map_col <- rename(map_col, region = ADM1_ES)


# Harmonize the region names
map_col$region[map_col$region == "Archipiélago de San Andrés Providencia y Santa Catalina"] <- "Archipelago of San Andrés, Providencia and Santa Catalina"
map_col$region[map_col$region == "Bogotá D.C."] <- "Bogotá"
map_col$region[map_col$region == "Bolívar"] <- "13"
map_col$region[map_col$region == "Boyacá"] <- "Boyaca"
map_col$region[map_col$region == "Caquetá"] <- "Caqueta"
map_col$region[map_col$region == "Chocó"] <- "Choco"
map_col$region[map_col$region == "Córdoba"] <- "Cordoba"
map_col$region[map_col$region == "La Guajira"] <- "La guajira"

# Functions -------------------------------------

extract_middle_age <- function(age_group){
  tmp <- str_split(age_group, "-", simplify = T)
  tmp <- apply(tmp, 2, as.numeric)
  (tmp[, 1] + tmp[, 2]+1) / 2
}

# Estimate rates --------------------------------

# Estimate the exposures
pop_col[order(age_group, region, sex, year), exposure := (pop + lag(pop))/2, by = .(age_group, region, sex)]

# Merge the two data sets
births <- merge(births, pop_col[sex == "males"], by = c("age_group", "region", "year"), suffixes = c("", "_males"))
births <- merge(births, pop_col[sex == "female"], by = c("age_group", "region", "year"), suffixes = c("_males", "_females"))

# Estimate the age-specific fertility rates
births[, asfr_male := birth_father / exposure_males]
births[, asfr_female := birth_mother / exposure_females]

# Get the middle age
births$middle_age <- extract_middle_age(births$age_group)

# Rename the birth file
asfr_col <- births

# Estimat the tfrs and mean age of childbearing
tfrs_col <- births[, .(mac_female = sum(asfr_female * middle_age) / sum(asfr_female),
           mac_male = sum(asfr_male * middle_age) / sum(asfr_male),
           tfr_male = sum(asfr_male * 5),
           tfr_female = sum(asfr_female * 5)), 
       by = .(region, year)]

# Estimate the tfr ratio and the male to female age difference
tfrs_col[, tfr_ratio := tfr_male / tfr_female]
tfrs_col[, mac_diff := mac_male - mac_female]

# Save the result
save(tfrs_col, file = "data/tfr_col.Rda")
save(asfr_col, file = "data/asfr_col.Rda")

# Plot the results ---------------------------------------

# Plot the relationship between male and female TFR
ggplot(tfrs_col, aes(x = tfr_female, y = tfr_male, colour = region, alpha = year, group = region)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous("TFR female") +
  scale_y_continuous("TFR male") +
  guides(alpha = "none", colour = "none")

# Plot the time trend in the TFR ratio
ggplot(tfrs_col, aes(x = year, y = tfr_ratio, colour = region)) +
  geom_hline(yintercept = 1) +
  geom_line() +
  scale_x_continuous("Year", expand = c(0, 0)) +
  scale_y_continuous("TFR ratio") +
  guides()

# Create a map
map_col_fert <- inner_join(map_col, tfrs_col)

# Plot the TFR ratio over time and regions
ggplot(subset(map_col_fert, year %in% c(2002, 2005, 2010, 2020)), aes(fill = tfr_ratio)) +
  geom_sf() +
  facet_wrap(~ year) +
  scale_fill_viridis_c("TFR ratio", option = "D", na.value = "grey") +
  theme(
    axis.text = element_blank(),
    legend.position = "right", 
    axis.ticks = element_blank(),
    legend.key.height = unit(2, "cm")
  )

### END ############################################