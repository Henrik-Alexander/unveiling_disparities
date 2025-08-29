########################################
# Purpose: Regression modelling        #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

# Clean the environment
#rm(list = ls()); gc(TRUE)

# Load the packages
library(data.table)
library(tidyverse)
library(janitor)
library(stargazer)
library(patchwork)
library(reshape2)
library(texreg)
library(readxl)

# Load the functions
source("Functions/Functions.R")
source("Functions/Graphics.R")

# 1. Prepare the fertility data ===================================

### Finnish data -----------------------------------------

# Preperate the Finnish data
fert_fin <- read.csv("U:/data/fin/birth_registers/tfr_ratios_maakunta.csv",
                     encoding = "latin1")

# Remove the X column
fert_fin <- fert_fin[, names(fert_fin) != "X"]

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

### Australian data -----------------------

# Prepare the Australian data
load("U:/data/aus/fertility_rates/asfr_aus.Rda")

# Aggregate the ASFR to TFRs
fert_aus <- aggregate(asfr_aus ~ year + region + sex, data = asfr_aus, FUN = sum)
fert_aus <- pivot_wider(fert_aus, id_cols = c("year", "region"),
                        names_from = "sex", names_prefix = "tfr_", 
                        values_from = "asfr_aus")

# Estimate the TFR ratio
fert_aus$tfr_ratio <- fert_aus$tfr_male / fert_aus$tfr_female


# Create the national data
load("U:/data/aus/fertility_rates/data/tfr_aus_nat.Rda")

# Create the country column
tfr_aus_nat$country <- fert_aus$country <- "Australia"

### Spanish data -------------------------------


# Clean the national data
load("U:/data/esp/fertility_rates/tfr_esp_nat.Rda")
fert_esp_nat <- tfr_esp_nat
fert_esp_nat$tfr_ratio <- with(fert_esp_nat, tfr_male / tfr_female)
fert_esp_nat <- fert_esp_nat[, c("year", "tfr_female", "tfr_male", "tfr_ratio")]

# Create regional tfr
load("U:/data/esp/fertility_rates/tfr_esp.Rda")
fert_esp <- tfr_esp_reg[, c("region", "year", "tfr_female", "tfr_male", "tfr_ratio")]

# Create the country column
fert_esp$country <- fert_esp_nat$country <- "Spain"
rm(list = ls(pattern = "tfr_esp"))


### French data -----------------------------

# Prepare the French data
load("U:/data/fra/fertility_rates/data/asfr_fra.Rda")

# Create the regional data
fert_fra <- asfr_fra |> 
  group_by(year, region) |> 
  summarise(tfr_female = sum(asfr_female, na.rm = TRUE),
            tfr_male   = sum(asfr_male, na.rm = TRUE),
            tfr_ratio  = tfr_male / tfr_female,
            country = "France",
            .groups = "drop")

# Create the national data
fert_fra_nat <- asfr_fra |> 
  group_by(year, age) |> 
  summarise(asfr_male = sum(births_male, na.rm = T) / sum(exposure_male, na.rm = T),
            asfr_female = sum(births_female, na.rm = T) / sum(exposure_female, na.rm = T),
            .groups="drop") |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male, na.rm = T),
            tfr_female = sum(asfr_female, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "France",
            .groups = "drop")
rm(asfr_fra)

### Mexican data -----------------------------

# Load the Mexican data fertility data
load("U:/data/mex/fertility_rates/asfr_mex.Rda")

# Estimate the TFR
fert_mex <- asfr_mex_reg |>
  group_by(region, year) |> 
  summarise(tfr_male = sum(asfr_male),
            tfr_female = sum(asfr_female),
            tfr_ratio  = sum(asfr_male) / sum(asfr_female),
            country = "Mexico",
            .groups = "drop")
rm(asfr_mex_reg)

# Load the national data
load("U:/data/mex/fertility_rates/asfr_national_mexico.Rda")

# Create the national ldata
fert_mex_nat <- asfr_mex_nat |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male, na.rm = T),
            tfr_female = sum(asfr_female, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "Mexico",
            .groups = "drop")
rm(asfr_mex_nat)

### US data ---------------------------

load("U:/data/usa/fertility_rates/data/asfr_us.Rda")

# Estimate the regional TFR ratios
fert_us <- asfr_us |>
  group_by(state, year) |> 
  summarise(tfr_male = sum(asfr_male),
            tfr_female = sum(asfr_female),
            country = "United States",
            .groups = "drop") |> 
  mutate(tfr_ratio = tfr_male / tfr_female) |> 
  rename(region = state) 

# Estimate the regional TFR ratios
fert_us_nat <- asfr_us |>
  group_by(year, age) |> 
  summarise(asfr_male = sum(births_male) / sum(pop_male),
            asfr_female = sum(births_female) / sum(pop_female),
            .groups="drop") |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male),
            tfr_female = sum(asfr_female),
            tfr_ratio = tfr_male / tfr_female,
            country = "United States",
            .groups = "drop") 
rm(asfr_us)

### German data ---------------------------

# Load the data
load("U:/data/deu/fertility_rates/asfr_deu.Rda")

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
            asfr_female = sum(births_female) / sum(exposure_female),
            .groups="drop") |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male * 5, na.rm = T),
            tfr_female = sum(asfr_female * 5, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "Germany",
            .groups = "drop") 
rm(asfr_deu)

### Colombian data ---------------------------------

# Load the data
load("U:/data/col/fertility_rates/asfr_col.Rda")

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
rm(asfr_col)

### Merge the country data sets -----------------------------

# Bind the national data
fert_nat <- bind_rows(fert_mex_nat, fert_us_nat, tfr_aus_nat, fert_deu_nat, 
                      fert_esp_nat, fert_fra_nat, fert_fin_nat, fert_col_nat)

# Bind the data
fert <- bind_rows(fert_mex, fert_us, fert_aus, fert_deu, fert_fra, fert_fin, fert_esp, fert_col)

# Remove missing observations
fert <- na.omit(fert)

# Estimate Schoen's U
fert$U <- with(fert, (tfr_male-tfr_female)/(0.5*tfr_male + 0.5*tfr_female))

# Save the data
save(fert, file = "data/fert_data_subnational.Rda")
write.csv(fert, "raw/fert_data_subnational.csv", fileEncoding = "utf-8")
write.csv(fert_nat, "raw/fert_data_national.csv", fileEncoding = "utf-8")

### Load the Schoumaker and Dudel data -----------------

# Set the global path to the data directory
path_global <- "U:/data/global/"

# Load the schoumaker data:
# Data accessed from: https://perso.uclouvain.be/bruno.schoumaker/data/
schoumaker_path <- "schoumaker_male/A. Estimates of male and female fertility.xlsx"
sd <- read_xlsx(paste0(path_global, schoumaker_path), sheet = 1)
sd <- janitor::clean_names(sd)
mac_schoumaker <- sd[, c("country_name", "mean_age_at_childbearing_shown_on_figures", "mean_age_at_fatherhood_shown_on_figures")]
sd <- sd[, c("country_name", "female_tfr_shown_on_figures", "male_tfr_shown_on_figures")]
names(sd) <- c("country", "tfr_female", "tfr_male")
sd$data <- "Schoumaker's fertility estimates"
sd[, c("tfr_male", "tfr_female")] <- apply(sd[, c("tfr_male", "tfr_female")], 2, as.numeric)
sd <- sd[!is.na(sd$tfr_female) & !is.na(sd$tfr_male), ]
mac_schoumaker[, 2:3] <- apply(mac_schoumaker[, 2:3], 2, as.numeric)

# Load the male-fertility database:
# Data accessed from: https://www.fertilitydata.org/Home/Index
hfc_files <- list.files(paste0(path_global, "male_fertility_database"), full.names = T)
hfc <- lapply(hfc_files, read.delim, sep = ",")
hfc <- bind_rows(hfc)
hfc$CNTRY <- gsub(" +", "", hfc$Country)
save(hfc, file = "data/asfr_male_dudel21.Rda")
hfc <- aggregate(ASFR ~ CNTRY + Year1, data = hfc, FUN = sum)
names(hfc) <- c("CNTRY", "Year", "tfr_male") 

# Load the female fertility data from the human fertility database
hfd <- read_xlsx(paste0(path_global, "/human_fertility_database/TFR.xlsx"), sheet = 2, skip = 2)
hfd <- pivot_longer(hfd, cols = !PERIOD, names_to = "CNTRY", values_to = "tfr_female")

# Get the meta information on countries and country codes in HFD
meta_hfd <- HMDHFDplus::getHFDcountries()
meta_hfd <- meta_hfd[, c("Country", "CNTRY")]

# Combine the data
hfc <- full_join(hfc, hfd, by = c("Year" = "PERIOD", "CNTRY"))
names(hfc) <- c("CNTRY", "year", "tfr_male", "tfr_female")
hfc <- inner_join(hfc, meta_hfd, by = "CNTRY")
names(hfc) <- tolower(names(hfc))
hfc$data <- "Human Fertility Collection"
hfc <- hfc[!is.na(hfc$tfr_female) & !is.na(hfc$tfr_male), ]

### Load the Schoen(1985) data:
rsd <- read.csv("raw/schoen_birthsqueeze_1985.csv")
rsd$country <- gsub("^[0-I]+. ", "", rsd[, 1])
rsd$tfr_ratio <- rsd$tfr_male / rsd$tfr_female
rsd$data <- "Robert Schoen, 1985" 

# Combine the Schoumaker's and Christian's data
ccd <- bind_rows(hfc, sd, rsd)

# Estimate the outcome: TFR ratio
ccd$tfr_ratio <- ccd$tfr_male / ccd$tfr_female

# Estimate U
ccd$U <- with(ccd, (tfr_male-tfr_female)/(0.5*tfr_female + 0.5*tfr_male))

# Save the data
save(ccd, file = "data/male_national_fertility.Rda")
write.csv(ccd, "raw/country_male_fertility.csv")

### END ############################################