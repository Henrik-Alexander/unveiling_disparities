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
library(quadprog)

# Load functions
source("functions/QOSplit.R")
source("functions/QOSplitPar.R")

# Set the path to the data
path_aus <- "U:/data/aus/birth_registers/"


### Clean the male data ------------------------------------

# Load the data
m <- read.csv(paste0(path_aus, "maternity_aus_1990-2020.csv"))

# Clean the variable names
m <- clean_names(m)

# Remove total age column
m <- m[m$age_age != "TOT: All ages", ]

# Remove age classes
m <- m |> filter(str_detect(age_age, "-"))

# Remove country
m <- m[m$region != "AUS: Australia", ]

# Create a tibble
region <- str_sub(m$region_region, start = 4, end = nchar(m$region_region))
year <- as.numeric(m$time_period_time_period)
indicator <- str_extract(m$measure_measure, "[A-Z][a-z]+")
value <- m$obs_value
age <- str_sub(m$age_age,6, 10)

# Combine the data
m <- tibble(region, year, indicator, value, age)

# Create wide data
m <- pivot_wider(m, names_from = "indicator", values_from = "value")

# Clean the names
m <- clean_names(m)

### Clean the maternity data ------------------------------------

# Load the data
p <- read.csv(paste0(path_aus, "paternity_aus_1990-2020.csv"))

# Clean the variable names
p <- clean_names(p)

# Remove total age column
p <- p[p$age_age != "TOT: All ages", ]

# Remove country
p <- p[p$region != "AUS: Australia", ]

# Create a tibble
region <- str_sub(p$region_region, start = 4, end = nchar(p$region_region))
year <- as.numeric(p$time_period_time_period)
indicator <- str_extract(p$measure_measure, "[A-Z][a-z]+")
value <- p$obs_value
age <- str_sub(p$age_age, 6, 10)

# Combine the data
p <- tibble(region, year, indicator, value, age)

# Create wide data
p <- pivot_wider(p, names_from = "indicator", values_from = "value")

# Clean the names
p <- clean_names(p)

### Combine the data --------------------------------------

# Join the two datasets
d <- full_join(m, p, by = c("region", "age", "year"), suffix = c("_female", "_male"))

# Replace the population
names(d) <- str_replace(names(d), "population", "exposure")
d <- rename(d, age_group = age)

### Impute missing data -----------------------------------

# Estimate the number of missing values per region
missings <- d |> 
  group_by(year, region) |> 
  summarise(across(starts_with("births"), ~ sum(.x, na.rm = T)), .groups = "drop")|> 
  mutate(missings = births_female - births_male) |> 
  select(region, year, missings)

# Combine
share <- d |> 
  group_by(year, region) |> 
  mutate(share = births_male / sum(births_male)) |> 
  ungroup() |> 
  select(region, year, age_group, share)


# Re-distribute the missing values
missings <- left_join(share, missings, by = c("region", "year")) |> 
  mutate(imputed_births = missings * share) |> 
  select(region, year, age_group, imputed_births)

# Combine with the original data
d <- full_join(d, missings) |> 
  mutate(births_m = births_male + imputed_births) |> 
  select(-imputed_births) |> 
  ungroup()

### Aggregate the data -----------------------------------

# Subset pre-2020 period
asfr_aus <- subset(d, year <= 2020)

# Estimate asfr
asfr_aus <- asfr_aus |>
  mutate(asfr_male = births_male / exposure_male,
         asfr_female = births_female / exposure_female)


# Estiamte the tfr
tfr_aus <- asfr_aus |> 
  mutate(age = 2.5 + as.numeric(str_sub(age_group, 1, 2))) |> 
  group_by(region, year) |> 
  summarise(tfr_male = sum(0.005 * asfr_male, na.rm = T),
            tfr_female = sum(0.005 * asfr_female, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            mac_male = sum(asfr_male * age, na.rm = T) / sum(asfr_male, na.rm = T),
            mac_female = sum(asfr_female * age, na.rm = T) / sum(asfr_female, na.rm = T),
            mac_diff = mac_male - mac_female,
            .groups = "drop")

# Save the data
save(tfr_aus, file = "Data/tfr_aus.Rda")
save(asfr_aus, file = "Data/asfr_aus.Rda")

### Ungroup population counts -----------------------------

# Load the data
load("Data/asfr_aus.Rda")

# Pivot longer
asfr_aus <- asfr_aus |> 
  select(region, year, age, asfr_male, asfr_female) |> 
  pivot_longer(cols = starts_with("asfr"), names_prefix = "asfr_", names_to = "sex", values_to = "asfr") |> 
  mutate(lower = as.numeric(str_sub(age, 1, 2))) |> 
  arrange(sex, region, year, age)

# Clean the overall data
asfr_aus <- as.data.table(asfr_aus)
asfr_aus <- asfr_aus[year < 2021, ]

# Create an result array
reg <- unique(asfr_aus$region)
yea <- unique(asfr_aus$year)
sex <- unique(asfr_aus$sex)
ages <- 15:59

# Create the vector for the new age groups
lower_age <- unique(asfr_aus$lower)
length_int <- rep(5, length = length(lower_age))

# Create a container
result <- array(NA, dim = c(2, length(reg), length(yea), length(ages)), 
                dimnames = list(sex, reg, yea, ages))

# Iterate for men
for (r in reg) {
    cat("Region:", r, "==================== \n")
    for (y in yea){
      # Selection
      sel <- asfr_aus[sex == "male" & region == r & year == y,  ]
      Fx <- sel$asfr
      
      # Run the model
      result["male", r, paste(y), paste(ages)] <- QOSplit(Fx, lower_age, length_int)$ASFR
      
      cat("Year:", y, "\n")
      
    }
}

# Dimensions
asfr_fem <- asfr_aus[sex == "female" & lower < 50, ]
ages <- 15:49
age <- unique(asfr_fem$age)
lower_age <- as.numeric(str_sub(age, 1, 2))
length_int <- rep(5, length = length(lower_age))

# Iterate for men
for (r in reg) {
  cat("Region:", r, "==================== \n")
  for (y in yea){
    # Selection
    sel <- asfr_fem[region == r & year == y,  ]
    Fx <- sel$asfr
    
    # Run the model
    result["female", r, paste(y), paste(ages)] <- QOSplit(Fx, lower_age, length_int)$ASFR
    
    cat("Year:", y, "\n")
    
  }
}

# Create the result
result <- as.data.frame.table(result)
names(result) <- c("sex", "region", "year", "age", "asfr_aus")
asfr_aus <- as.data.table(result)

# Make character vectors
vars <- c("sex", "region")
asfr_aus[, (vars) := lapply(.SD, as.character), .SDcols = vars]

# Make numeric vector
vars <- c("age", "year")
asfr_aus[, (vars) := lapply(.SD, function(x) as.integer(paste(x))), .SDcols = vars]

# Save the result
save(asfr_aus, file = "data/asfr_aus.Rda")

# Pivot wider
asfr_aus <- asfr_aus |> 
  pivot_wider(names_from = c(sex), values_from = asfr_aus, names_prefix = "asfr_")

# Esttimate tfr, tfr-ratio and mean age of childbearing
tfr_aus <- asfr_aus |> 
  group_by(year, region) |> 
  summarise(tfr_female = sum(asfr_female, na.rm = TRUE),
            tfr_male   = sum(asfr_male, na.rm = TRUE),
            tfr_ratio  = tfr_male / tfr_female,
            mac_female = sum(asfr_female * age, na.rm = T) / sum(asfr_female, na.rm = TRUE),
            mac_male = sum(asfr_male * age, na.rm = T) / sum(asfr_male, na.rm = TRUE),
            mac_diff = mac_male - mac_female,
            .groups = "drop")

# Save the result
save(tfr_aus, file = "Data/tfr_aus.Rda")

### Compare the trend -------------------------------------

# Plot the asfr
asfr_aus |> 
  filter(year %in% c(1990, 2018) & region %in% c("Queensland", "Australian Capital Territory")) |> 
  ggplot(aes(x = age, y = asfr_aus, group = sex, colour = sex)) +
  geom_line() +
  facet_wrap(region ~ year)

# Estimate the difference for usa
tfr_aus |> 
  group_by(year) |> 
  summarise(median = median(tfr_ratio),
            range = range(tfr_ratio),
            .groups = "drop") |> 
  filter(year %in% c(1990, 2020))

### END ###############################################