### Impute Births #######################################
# Purpose: Impute missing ages for father and mother    #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: cleaned birth data and functions       #
#########################################################

### Settings ------------------------------------------------------------------

rm(list = ls())

# load functions
source("Functions/Packages.R")
source("Functions/Functions.R")
source("Functions/Graphics.R")
source("Functions/Functions_Mexico.R")

# Age range for men and women
age_m <- 12:59
age_f <- 12:49

# Years range
years <- 1990:2021


### Read data -------------------------------------------------------------

# Read birth data
load("Data/births_complete_MEX.Rda")

# Filter the data
data <- data |>
  filter(entity %!in% c("USA", "Other Latin American Countries", "Other countries"))

# Split by year and region
data <- split(data, list(data$entity, data$year))

### Impute the births -----------------------------------------------------

# Impute mother's age by using father's age
d1 <- lapply(data, filter, !is.na(age_fat))
d1 <- lapply(d1, impute_variable, outcome = age_mot, predictor = age_fat)
d1 <- bind_rows(d1, .id = "year")

# Impute mother's age by using parity
d2 <- lapply(data, filter, is.na(age_fat) & !is.na(parity))
d2 <- lapply(d2, impute_variable, outcome = age_mot, predictor = parity)
d2 <- bind_rows(d2, .id = "year")

# Unconditional approach for births with parity and age of father missing
d3 <- lapply(data, filter, is.na(age_fat) & is.na(parity))
d3 <- lapply(d3, impute_unconditional, age_mot)
d3 <- bind_rows(d3, .id = "year")

# Get the births with available information
d4 <- lapply(data, filter, !is.na(age_mot))
d4 <- map(d4, function(x) x |> group_by(age_mot) |> summarise(births = n()))
d4 <- bind_rows(d4, .id = "year")

# Combine the results
births_mot <- bind_rows(d1, d2, d3, d4)

# Aggregate
births_mot <- aggregate(births ~ age_mot + year, data = births_mot, FUN = sum)

### Impute age of the father ---------------------------------------------

# Impute father's age by using mother's age
d1 <- lapply(data, filter, !is.na(age_mot))
d1 <- lapply(d1, impute_variable, outcome = age_fat, predictor = age_mot)
d1 <- bind_rows(d1, .id = "year")

# Impute mother's age by using parity
d2 <- lapply(data, filter, is.na(age_mot) & !is.na(parity))
d2 <- lapply(d2, impute_variable, outcome = age_fat, predictor = parity)
d2 <- bind_rows(d2, .id = "year")

# Unconditional approach for births with parity and age of father missing
d3 <- lapply(data, filter, is.na(age_mot) & is.na(parity))
d3 <- lapply(d3, impute_unconditional, age_fat)
d3 <- bind_rows(d3, .id = "year")

# Get the births with available information
d4 <- lapply(data, filter, !is.na(age_fat))
d4 <- map(d4, function(x) x |> group_by(age_fat) |> summarise(births = n()))
d4 <- bind_rows(d4, .id = "year")

# Combine the results
births_fat <- bind_rows(d1, d2, d3, d4)

# Aggregate
births_fat <- aggregate(births ~ age_fat + year, data = births_fat, FUN = sum)

### Clean the data ----------------------------------------------------

# Clean the data
births_fat <- births_fat |>
  mutate(entity = as.factor(str_split(year, pattern = "\\.", simplify = TRUE)[, 1]), 
         year   = as.integer(str_split(year, pattern = "\\.", simplify = TRUE)[, 2]))


# Clean the data
births_mot <- births_mot |>
  mutate(entity = as.factor(str_split(year, pattern = "\\.", simplify = TRUE)[, 1]), 
         year   = as.integer(str_split(year, pattern = "\\.", simplify = TRUE)[, 2]))


### Plot the data ------------------------------------------------------

# Plot females
ggplot(births_fat, aes(age_fat, births, group = year,  colour = year)) +
  geom_line() +
  facet_wrap(~ entity, scales = "free_y") +
  scale_colour_gradient(low = MPIDRgreen, high =  MPIDRyellow)

# Plot males
ggplot(births_mot, aes(age_mot, births, group = year, colour = year)) +
  geom_line() +
  facet_wrap(~ entity, scales = "free_y") +
  scale_colour_gradient(low = MPIDRgreen, high =  MPIDRyellow)

# Save the data
save(births_fat, file = "Data/births_father.Rda")
save(births_mot, file = "Data/births_mother.Rda")

### END ########################################################################  