#########################################
# Project: Sub-national birth squeezes  #
# Purpose: Estimate German Rates        #
# Author: Henrik-Alexander Schubert     #
# E-mail: schubert@demogr.mpg.de        #
# Date: 29.01.2024                      #
#########################################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(janitor)

# Load the functions
source("functions/functions.R")
source("functions/Graphics.R")

# Functions ------------------------------------

calc_tfr <- function (rate) sum(rate * 5)
calc_mac <- function (rate, upper, lower) {
  midage <- (upper + lower) / 2
  mac <- sum(midage * rate) / sum(rate)
  return(mac)
}

### Load the data ------------------------------

# Get the file names
path_data <- "U:/data/deu/birth_registers/"
files <- list.files(path_data, full.names = T, pattern = ".csv$")

# Load the data
pop <- fread(files[1], skip = 7, fill = T, sep = ";")

# Encoding of the bundesland
Encoding(pop$V3) <- "ISO/IEC 8859-15"

# Remove the last rows that are missing
pop <- pop[!is.na(V3)]

# Create the womens data
meta <- pop[, c(1:3)]
total <- pop[, c(4:14)]
males <- pop[, c(15:25)]
females <- pop[, c(26:36)]

# Create the data
males$sex <- "male"
females$sex <- "female"
names(meta) <- c("date", "state", "bundesland")

# Bind the data together
d <- rbindlist(list(males, females))
d <- cbind(meta, d)

# Clean the names
d <- clean_names(d)

# Create long data
d <- pivot_longer(d, cols = starts_with("x"), names_to = "age", values_to = "pop", names_prefix = "x")

# Clean the age-range
d$lower <- as.numeric(str_split_i(d$age, "_", i = 1))
d$upper <- as.numeric(str_split_i(d$age, "_", i = 4))

# Extract the year
d$year <- as.numeric(str_extract(d$date, "\\d{4}$"))

# Combine 15-18 and 18-20
d <- d %>% 
  mutate(lower = if_else(lower == 18, 15, lower),
         upper = if_else(upper == 18, 15, upper)) %>% 
  group_by(bundesland, state, sex, lower, upper, year) |> 
  summarize(pop = sum(pop), .groups = "drop") %>% 
  arrange(state, sex, lower, year) %>% 
  group_by(bundesland, state, sex, lower) %>% 
  mutate(exposure = (pop + lag(pop)) / 2,
         exposure = ifelse(is.na(exposure), lead(exposure), exposure)) |> 
  as_tibble()

# Create the female and male population
pop_f <- d[d$sex == "male", ]
pop_m <- d[d$sex == "female", ]

# Load the birth data ------------------------------

# Load the birth data
births_f <- read.csv(files[2], sep = ";")
births_m <- read.csv(files[3], sep = ";")


# Create the lower and upper values
births_f <- births_f |>
  mutate(birth = gsub(",", ".", birth)) |> 
  mutate(across(c(state, year, birth, total), as.numeric))

# Create numeric variables
births_m <- births_m |>
  mutate(birth = gsub(",", ".", birth)) |> 
  mutate(across(c(state, year, birth, total), as.numeric))

# Create the upper and lower boundaries
births_m$lower <- as.numeric(str_sub(births_m$age_mother, 2, 3))
births_f$lower <- as.numeric(str_sub(births_f$age_father, 2, 3))
births_m$upper <- as.numeric(str_sub(births_m$age_mother, 5, 6))
births_f$upper <- as.numeric(str_sub(births_f$age_father, 5, 6))

# Filter only complete cases
births_m <- births_m[!is.na(births_m$total), ]
births_f <- births_f[!is.na(births_f$total), ]

## Impute the missing births ---------------------------

## Impute for mothers 

# Expand the list with missing values
tmp <- births_m |> 
  complete(state, age_mother, year, fill = list(birth = NA, total = NA))

# Impute the missing data
birhts_missing_m <- tmp |> 
  group_by(year, state) |> 
  fill(total, .direction = "downup") |> 
  mutate(tot2 = sum(birth, na.rm = T),
         miss = ifelse(total > tot2, 1, 0),
         diff = total - tot2,
         n_miss = sum(ifelse(is.na(birth), 1, 0))) |> 
  filter(miss == 1 & is.na(birth)) %>% 
  arrange(state, year, lower) %>% 
  mutate(count = row_number(),
         birth = if_else(count == n_miss, 3, (diff - 3) / (n_miss - 1))) %>% 
  select(state, age_mother, year, birth, total, lower, upper)

# Complete the data
births_m <- bind_rows(births_m, birhts_missing_m)  
  

# Complete the data
births_missing_f <- births_f %>%
  dplyr::select(!X) %>% 
  complete(state, age_father, year, fill = list(birth = NA, total = NA)) %>% 
  group_by(year, state) %>% 
  fill(total, .direction = "downup") %>% 
  mutate(total2 = sum(birth, na.rm = T),
         missing = ifelse(round(total) > round(total2), 1, 0),
         diff    = round(total - total2),
         n_miss = sum(ifelse(is.na(birth), 1, 0))) %>% 
  filter(missing == 1 & is.na(birth)) %>% 
  arrange(state, year, lower) %>% 
  mutate(count = row_number(),
         birth = if_else(count == n_miss, 3, (diff - 3) / (n_miss - 1))) 
births_f <- bind_rows(births_f, births_missing_f)  


### Merge the data -------------------------------------

# Join the data
births_m <- left_join(births_m, pop_m, by = c("year", "lower", "upper", "state"))
births_f <- left_join(births_f, pop_f, by = c("year", "lower", "upper", "state"))

# Assign complete sex
births_m$sex <- "female"
births_f$sex <- "male"

# Change the name of the two parental age groups
births_m <- rename(births_m, age_group = age_mother)
births_f <- rename(births_f, age_group = age_father)

# Bind the data
births <- bind_rows(births_m, births_f)

# Rename columns
births <- births |> rename(region = state,
                           births = birth)

# Data correction --------------------------------------

# Select the countries
prob_cnt <- c("Brandenburg", "Saarland", "Bremen", "Mecklenburg-Vorpommern")
probs <- births[births$bundesland %in% prob_cnt, ]

# Missings
prob_births <- probs %>% 
  group_by(year) %>% 
  filter(year %in% 2011:2013) %>% 
  summarise(births = sum(births), .groups = "drop") %>%
  pull(births) 

# Estimate the ratio-factor
corr_factor <- ((prob_births[1] + prob_births[3])/2) / prob_births[2]

# Correct the birth count
births$birth[births$year == 2012 & births$bundesland %in% prob_cnt] <- births$birth[births$year == 2012 & births$bundesland %in% prob_cnt] * corr_factor

### Estimate the rates ---------------------------------

# Estimate the rates
births$asfr <- with(births, births / exposure)

# Filter the data
births <- births[births$year >= 1995 & !is.na(births$sex), ]

# Create asfr data
births$region <- births$bundesland
asfr_deu <- subset(births, select = c(region, year, births, exposure, asfr, sex, age_group, lower, upper))
asfr_deu <- asfr_deu |> pivot_wider(id_cols = c(region, year, age_group, lower, upper), 
              names_from = "sex",
              values_from = c("exposure", "births", "asfr"))


save(asfr_deu, file = "data/asfr_deu.Rda")

# Create East-West scheme
births <- births %>% 
  mutate(east_west = if_else(bundesland %in% c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen"), "East", "West"))


# Estimate the tfr and mac
tfr <- births |> 
  group_by(bundesland, year, state, sex, east_west) |> 
  summarise(tfr = calc_tfr(asfr), .groups = "drop")

# Estimate the mean age of childbearing for females
mac <- births |> 
  group_by(bundesland, year, state, sex, east_west) |> 
  summarise(mac = calc_mac(asfr, upper, lower), .groups = "drop")


# Join the data 
fert_deu <- inner_join(tfr, mac)

save(fert_deu, file = "data/fertility_germany.Rda")

### Plot the data ------------------------------------

# Plot the trends
ggplot(tfr, aes(x = year, y = tfr, colour = bundesland, group = bundesland)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~ sex) +
  scale_x_continuous("Year", expand = c(0.01, 0), n.breaks = 8) +
  scale_y_continuous("Total fertility rate") +
  scale_colour_manual("", values = pals::watlington())
figs(last_plot(), "tfr_trend_germany")

# Plot the mean age of childbearing
ggplot(mac, aes(x = year, y = mac, colour = bundesland, group = bundesland)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~ sex) +
  scale_x_continuous("Year", expand = c(0.01, 0), n.breaks = 8) +
  scale_y_continuous("Total fertility rate") +
  scale_colour_manual("", values = pals::watlington())
figs(last_plot(), "mac_trend_germany")


# Estimate the tfr ratio
mac %>% 
  pivot_wider(names_from = "sex", values_from = mac) %>% 
  mutate(mac_diff = male - female) %>% 
  ggplot(aes(x = year, y = mac_diff, colour = bundesland, group = bundesland)) +
  geom_line() + 
  geom_point() +
  scale_x_continuous("Year", expand = c(0.01, 0), n.breaks = 8) +
  scale_y_continuous("Mac difference") +
  scale_colour_manual("", values = pals::watlington())
figs(last_plot(), "mac_diff_germany")


# Estimate the difference in mean age of childearing
tfr %>% 
  pivot_wider(names_from = "sex", values_from = tfr) %>% 
  mutate(tfr_ratio = male / female) %>% 
  ggplot(aes(x = year, y = tfr_ratio, colour = bundesland, group = bundesland)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 1) +
  scale_x_continuous("Year", expand = c(0.01, 0), n.breaks = 8) +
  scale_y_continuous("male TFR / female TFR", trans = "log") +
  scale_colour_manual("", values = pals::watlington())
figs(last_plot(), "tfr_ratio_germany")

# Estimate the difference in mean age of childearing
tfr %>% 
  pivot_wider(names_from = "sex", values_from = tfr) %>% 
  mutate(tfr_ratio = male / female) %>% 
  ggplot(aes(x = year, y = tfr_ratio, colour = east_west, group = bundesland, shape = east_west)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 1) +
  scale_x_continuous("Year", expand = c(0.01, 0), n.breaks = 8) +
  scale_y_continuous("male TFR / female TFR", trans = "log") +
  scale_shape("", solid = T) +
  scale_colour_brewer("", palette = "Set1")
figs(last_plot(), "tfr_ratio_germany_eastwest")

### END ######################################################
