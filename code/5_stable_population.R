########################################
# Project: Subnational birth squeezes  #
# Purpose: Stable population theory    #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 18.07.2024                     #
########################################

rm(list = ls())

# Load the packages
library(brms)
library(tidyverse)
library(readxl)
library(HMDHFDplus)
library(patchwork)

source("functions/stable_population.R")
source("functions/Graphics.R")

# Load the dudel data
load("data/asfr_male_dudel21.Rda")

## Functions ------------------------------

# Write the Schoumaker approximation
tfr_stable_pop <- function(SRB, p_mac, p_maf, r, T_m, T_f) {
  1 / SRB * (p_mac / p_maf) * exp(r*(T_m - T_f)) 
}

# Function to harmonize the SRB and HMD country names
harmonize_country_srb <- function(country_names) {
  country_names[country_names == "United States of America"] <- "U.S.A"
  country_names[country_names == "United Kingdom"] <- "England and Wales"
  return(country_names)
}

# Plot the impact of one determinant on tfr approximation
plot_tfr_ratio <- function(det, label, data = stable_pop) {
  p <- ggplot(data, aes_string(x=det, y="tfr_ratio_stable_pop", colour="Country")) + 
    geom_hline(yintercept = 1) +
    geom_point() +
    scale_y_continuous("TFR ratio") +
    scale_colour_viridis_d(name = "") +
    theme(axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8))
  if (is.numeric(data[[det]])){
    p <- p +scale_x_continuous(label)
  } else {
    p <- p + scale_x_discrete(label) +
      coord_flip() +
      theme(axis.text.y = element_text(size = 6))
  }
  return(print(p))
}

# Growth rates to data
r_to_data <- function(growth_rates) {
  data.frame(country=str_extract(names(growth_rates),"^[A-Z]+"),
             year=as.numeric(str_extract(names(growth_rates), "[0-9]+$")),
             r = growth_rates)
}

# Function to estimate the mean age at childbearing (MAC), for father (MAF)
estimate_mac <- function(age, fx) {
  sum(age * fx) / sum(fx)
}

# Function to load life tables
load_life_table <- function(path) {
  tmp <- list.files(path, full.names = T)
  tmp <- lapply(tmp, read.table, skip = 2, header = T)
  tmp <- lapply(tmp, function(x) mutate(x, across(everything(), as.numeric)))
  names(tmp) <- str_extract(list.files(path), "^[A-Z]+(_[A-Z]+)?")
  tmp <- bind_rows(tmp, .id = "country")
  return(tmp)
}

# Estimation follows Keyfitz/Caswell 2005: Applied mathematical demography, p. 265
estimate_generation_length <- function(age, px, fx) {
  sum(age * px * fx) / sum(px * fx)
}


### Data wrangling ---------------------------------

# Get the meta information on countries and country codes in HFD
meta_hfd <- HMDHFDplus::getHFDcountries()
meta_hfd <- meta_hfd[, c("Country", "CNTRY")]

# Load the mean age of childbearing
mac <- read.table("U:/data/global/human_fertility_database/mabRR.txt", skip = 2, header = T)
maf <- hfc |> group_by(CNTRY, Year1) |> summarise(maf = estimate_mac(Age, ASFR), .groups = "drop")

# Clean the sex ratio at births
srb <- read_xlsx("U:/data/global/sex_ratios/pnas.1812593116.sd02.xlsx", range="A4:F43252")
srb <- srb[srb$Quantile == "Median", c("Country.Name", "Reference.Year", "Model.Estimate")]
names(srb) <- c("country", "year", "srb")
srb$country <- harmonize_country_srb(srb$country)

# Merge mean age of fatherhood
timing <- inner_join(mac, maf, by = c("Code"="CNTRY", "Year"="Year1"))
timing <- inner_join(timing, meta_hfd, by = c("Code"="CNTRY"))
timing <- timing |> select(-MAB40) |> rename(mac = MAB)

# Load the population growth rates log(p1 / p0)
growth_rates <- read.csv(list.files("U:/data/global/world_population_prospects", full.names=T))
growth_rates <- growth_rates[, c("Time", "Location", "Value")]
names(growth_rates) <- c("year", "country", "r")
growth_rates$country <- harmonize_country_srb(growth_rates$country)

### Estimate the generation length --------------------------------

# For Men
lt_m <- load_life_table(path = "U:/data/global/human_mortality_database/lt_male/mltper_1x1")
t_m <- left_join(lt_m, hfc, by = c("country"="CNTRY", "Year"="Year1","Age"="Age"))
t_m <- t_m |> group_by(country, Year) |> mutate(na = sum(!is.na(ASFR))) |> filter(na > 0)

# Estimate the stable population growth rate
leslies <- t_m |> group_by(country, Year) |> mutate(na = sum(!is.na(ASFR))) |> filter(na >0) |> select(-na)
leslies <- split(leslies, list(leslies$country, leslies$Year), drop = T)
leslies <- lapply(leslies, create_leslie, sex = "m")
growth_rates_m <- map_dbl(leslies, stable_pop_growth)
growth_rates_m <- r_to_data(growth_rates_m)

# Estimate the generation length
t_m <- t_m |> filter(Age < 55) |> group_by(country) |> mutate(px = cumprod(1-qx))
t_m$ASFR[is.na(t_m$ASFR)] <- 0
t_m <- t_m |> 
  group_by(Year, country) |>
  summarise(t_m = estimate_generation_length(age=Age, px=px, fx=ASFR), .groups = "drop")

# For women
lt_f <- load_life_table(path = "U:/data/global/human_mortality_database/lt_female/fltper_1x1")
asfr_f <- read.table("U:/data/global/human_fertility_database/asfrRR.txt", skip = 2, header=T)
asfr_f$Age <- as.numeric(asfr_f$Age)
t_f <- left_join(lt_f, asfr_f, by = c("country"="Code", "Year"="Year","Age"))

# Estimate the stable population growth rate
leslies <- t_f |> group_by(country, Year) |> mutate(na = sum(!is.na(ASFR))) |> filter(na >0) |> select(-na)
leslies <- split(leslies, list(leslies$country, leslies$Year), drop = T)
leslies <- lapply(leslies, create_leslie)
growth_rates_f <- map_dbl(leslies, stable_pop_growth)
growth_rates_f <- r_to_data(growth_rates_f)

  
# Estimate the mean gernation length
t_f$ASFR[is.na(t_f$ASFR)] <- 0
t_f <- t_f |> group_by(country, Year) |> mutate(na = sum(!is.na(ASFR))) |> filter(na > 0)
t_f <- t_f |> filter(Age < 55) |> group_by(country) |> mutate(px = cumprod(1-qx))
t_f <- t_f |>
  group_by(Year, country) |>
  summarise(t_f = estimate_generation_length(age=Age, px=px, fx=ASFR), .groups = "drop")

# Combine the generation lengths
generation_length <- left_join(t_m, t_f, by = join_by(Year, country))

### Merge timing with life tables
timing$age_mother <- round(timing$mac)
timing$age_father <- round(timing$maf)
timing <- left_join(timing, lt_f[, c("Age", "Year", "country", "lx")], by = c("Year", "age_mother" = "Age", "Code"="country"))
timing <- left_join(timing, lt_m[, c("Age", "Year", "country", "lx")],
                    by = c("Year", "age_father" = "Age", "Code"="country"),
                    suffix = c("_mother", "_father"))

# Divide by the life table radix
timing$px_mac <- timing$lx_mother / 100000
timing$px_maf <- timing$lx_father / 100000
timing <- timing[, c("Code", "Year", "Country", "maf", "mac", "px_mac", "px_maf")]

### Combine the data
stable_pop <- left_join(generation_length, timing, c("Year", "country"="Code"))
stable_pop <- left_join(stable_pop, srb, by = c("Country"="country", "Year"="year"))
growth_rates <- inner_join(growth_rates_f, growth_rates_m, by = join_by(country, year), suffix = c("_f", "_m"))
growth_rates$r <- with(growth_rates, 2 / (1/r_f + 1/r_m))
stable_pop <- left_join(stable_pop, growth_rates, by = c("Year"="year", "country"="country"))


# Drop Australia because it is not in the HFD
stable_pop <- na.omit(stable_pop)
stable_pop <- rename(stable_pop, country_code = country)

# Estimate the TFR ratio approximation
stable_pop$tfr_ratio_stable_pop <- with(stable_pop, 1/(srb) * px_mac/px_maf * exp(r*(t_m-t_f)))

# Save the data
save(stable_pop, file = "data/stable_population.Rda")

# Plot the data
variable_labels <- c("Year", "Country", "Generation length (men)", "Generation length (women)",
                     "Mean age at fatherhood", "Mean age at childbearing", "Probability to survive to MAC",
                     "Probability to survive to MAF", "Sex ratio at birth", "Growth rate")
variable_names <- c("Year", "Country", "t_m", "t_f", "maf", "mac", "px_mac", "px_maf", "srb", "r")
stable_pop$Country <- gsub(" and ", "/", stable_pop$Country)
plots <- map2(.x = variable_names,.y = variable_labels, .f = plot_tfr_ratio)
plot_panel <- wrap_plots(plots, nrow = 5, ncol = 2) + plot_layout(guides = "collect")
ggsave(plot_panel, filename = "figures/stable_pop_correlates.pdf", height = 25, width = 15, unit = "cm")

# Plot the distribution of sex ratio approximations
ggplot(stable_pop, aes(x = tfr_ratio_stable_pop)) +
  geom_vline(xintercept = 1, colour = "red") +
  geom_histogram(alpha = 0.8) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous("TFR ratio under stable population approximation")
ggsave(last_plot(), filename = "figures/stable_pop_tfr_ratio.pdf", height = 15, width = 15, unit = "cm")

### END #################################