########################################
# Purpose: Regression modelling        #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

# Clean the environment
rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(janitor)
library(stargazer)
library(patchwork)
library(ggrepel)
library(ggExtra)
library(ppcor)
library(reshape2)
library(broom)
library(ineq)
library(latex2exp)
library(lme4)
library(texreg)

# Load the functions
source("Functions/Functions.R")
source("Functions/Graphics.R")

### Prepare the fertility data ===================================

# Preperate the Finnish data
fert_fin <- read.csv("U:/data/fin/birth_registers/tfr_ratios_maakunta.csv",
                     encoding = "latin1")

# Estimate the regional TFRS
fert_fin <- fert_fin |> 
  rename(region = Maakunta) |> 
  mutate(tfr_ratio = tfr_male / tfr_female,
         country = "Finland")

# Estimate the Finnish fertility
fert_fin_nat <- fert_fin |> 
  group_by(year) |> 
  summarise(tfr_male = mean(tfr_male),
            tfr_female = mean(tfr_female),
            tfr_ratio = tfr_male / tfr_female,
            country = "Finland",
            .groups = "drop")

### Clean the Australian data -----------------------

# Prepare the Australian data
load("data/asfr_aus.Rda")
fert_aus <- asfr_aus |> 
  group_by(year, age_group, region) |> 
  summarise(asfr_male = sum(births_male) / sum(exposure_male),
            asfr_female = sum(births_female, na.rm = T) / sum(exposure_female, na.rm = T)) |> 
  group_by(year, region) |> 
  summarise(tfr_male = sum(asfr_male * 5, na.rm = T),
            tfr_female = sum(asfr_female * 5, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "Australia",
            .groups = "drop")


# Create the national data
fert_aus_nat <- asfr_aus |> 
  group_by(year, age_group) |> 
  summarise(asfr_male = sum(births_male) / sum(exposure_male),
            asfr_female = sum(births_female, na.rm = T) / sum(exposure_female, na.rm = T)) |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male * 5, na.rm = T),
            tfr_female = sum(asfr_female * 5, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "Australia",
            .groups = "drop")

### Clean the Spanish data -------------------------------

# Load the spanish data
load("data/tfr_esp_nat.Rda")
load("data/tfr_esp.Rda")

# Clean the regional data
fert_esp_nat <- tfr_esp_nat |>
  mutate(country = "Spain", tfr_ratio = tfr_male / tfr_female)

# Create regional tfr
fert_esp <- tfr_esp_reg |> 
  rename(region = Region) |> 
  mutate(tfr_ratio = tfr_male / tfr_female,
         country = "Spain")

### Prepare the french data -----------------------------

# Prepare the French data
load("data/asfr_fra.Rda")

# Create the regional data
fert_fra <- asfr_fra |> 
  group_by(year, region) |> 
  summarise(tfr_female = sum(asfr_female, na.rm = TRUE),
            tfr_male   = sum(asfr_male, na.rm = TRUE),
            tfr_ratio  = tfr_male / tfr_female,
            mac_female = sum(asfr_female * age, na.rm = T) / sum(asfr_female, na.rm = TRUE),
            mac_male = sum(asfr_male * age, na.rm = T) / sum(asfr_male, na.rm = TRUE),
            mac_diff = mac_male - mac_female,
            country = "France",
            .groups = "drop")

# Create the national data
fert_fra_nat <- asfr_fra |> 
  group_by(year, age) |> 
  summarise(asfr_male = sum(births_male, na.rm = T) / sum(exposure_male, na.rm = T),
            asfr_female = sum(births_female, na.rm = T) / sum(exposure_female, na.rm = T)) |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male, na.rm = T),
            tfr_female = sum(asfr_female, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "France",
            .groups = "drop")


### Prepare the Mexican data -----------------------------

# Load the Mexican data fertility data
load("data/asfr_regional_mexico.Rda")
fert_mex <- asfr_reg |>
  group_by(entity_name, year) |> 
  summarise(tfr_male = sum(asfr_m),
            tfr_female = sum(asfr_f),
            tfr_ratio  = sum(asfr_m) / sum(asfr_f),
            mac_male = sum(asfr_m * age) / sum(asfr_m),
            mac_female = sum(asfr_f * age) / sum(asfr_f),
            mac_diff = mac_male - mac_female,
            country = "Mexico",
            .groups = "drop") |> 
  mutate(mac_diff = mac_male - mac_female,
         tfr_ratio = tfr_male / tfr_female) |> 
  rename(region = entity_name) 

# Load the national data
load("data/asfr_national_mexico.Rda")

# Create the national ldata
fert_mex_nat <- asfr_nat |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male, na.rm = T),
            tfr_female = sum(asfr_female, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "Mexico",
            .groups = "drop")

### Prepare the US data ---------------------------

load("data/asfr_us.Rda")

# Estimate the regional TFR ratios
fert_us <- asfr_us |>
  group_by(state, year) |> 
  summarise(tfr_male = sum(asfr_male),
            tfr_female = sum(asfr_female),
            mac_male = sum(asfr_male * age) / sum(asfr_male),
            mac_female = sum(asfr_female * age) / sum(asfr_female),
            country = "United States",
            .groups = "drop") |> 
  mutate(tfr_ratio = tfr_male / tfr_female) |> 
  rename(region = state) 

# Estimate the regional TFR ratios
fert_us_nat <- asfr_us |>
  group_by(year, age) |> 
  summarise(asfr_male = sum(births_male) / sum(pop_male),
            asfr_female = sum(births_female) / sum(pop_female)) |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male),
            tfr_female = sum(asfr_female),
            tfr_ratio = tfr_male / tfr_female,
            country = "United States",
            .groups = "drop") 

### Prepare the german data ---------------------------

# Load the data
load("data/asfr_deu.Rda")

# Estimate the regional TFR ratios
fert_deu <- asfr_deu |> 
  group_by(region, year) |> 
  summarise(tfr_male = sum(asfr_male * 5, na.rm = T),
            tfr_female = sum(asfr_female * 5, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
         country = "Germany",
         .groups = "drop") 

# Estimate the national TFR ratios
fert_deu_nat <- asfr_deu |> 
  group_by(year, age_group) |> 
  summarise(asfr_male = sum(births_male) / sum(exposure_male),
            asfr_female = sum(births_female) / sum(exposure_female)) |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male * 5, na.rm = T),
            tfr_female = sum(asfr_female * 5, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "Germany",
            .groups = "drop") 


### Load the Colombian data ---------------------------------
load("data/asfr_col.Rda")

# Estimate the TFR ratios
fert_col <- asfr_col[, .(tfr_male = sum(asfr_male*5),
                         tfr_female = sum(asfr_female*5),
                         tfr_ratio = sum(asfr_male*5) / sum(asfr_female*5), country = "Colombia"), by = .(year, region)]

# National fertility
fert_col_nat <- asfr_col[, .(asfr_male = sum(birth_father)/sum(exposure_males), asfr_female = sum(birth_mother)/sum(exposure_females)), by = .(age_group, year)]
fert_col_nat <- fert_col_nat[, .(tfr_male = sum(asfr_male * 5),
                                 tfr_female = sum(asfr_female * 5), 
                                 country = "Colombia"), by = year]
fert_col_nat[, tfr_ratio := tfr_male / tfr_female]

### Merge the country data sets -----------------------------

# Bind the national data
fert_nat <- bind_rows(fert_mex_nat, fert_us_nat, fert_aus_nat, fert_deu_nat, 
                      fert_esp_nat, fert_fra_nat, fert_fin_nat, fert_col_nat)

# Bind the data
fert <- bind_rows(fert_mex, fert_us, fert_aus, fert_deu, fert_fra, fert_fin, fert_esp, fert_col)

# Save the data
save(fert, file = "data/fert_data_subnational.Rda")
write.csv(fert, "data/fert_data_subnational.csv", fileEncoding = "utf-8")
write.csv(fert_nat, "data/fert_data_national.csv", fileEncoding = "utf-8")

### END ############################################