##############################################
# Purpose: National and regional TFR ratios  #
# Author: Henrik-Alexander Schubert          #
# Date: 15.11.2023                           #
# E-mail: schubert@demogr.mpg.de             #
##############################################

rm(list = ls())

# Load the packages
library(tidyverse)
library(data.table)
library(HMDHFDplus)
library(janitor)

# Functions
source("Functions/Graphics.R")
source("Functions/passwords.R")

# Load the regional data
load("Data/fert_data_subnational.Rda")

### Prepare national data -------------------------------------

# Set the path
path_mfd <- "u:/data/global/male_fertility_database"

# Load the data
data <- map(list.files(path_mfd, full.names = T), read_delim)

# Combine the results
data <- rbindlist(data)

# Estimate the total fertility rate
data <- clean_names(data)
data$age <- as.numeric(data$age)
data <- data[, .(tfr_male = sum(asfr), mac_male = sum(age * asfr)/sum(asfr)), by = .(year1, country)]
data$year1 <- as.integer(data$year1)
data$country <- str_remove_all(data$country, " ")

# Create vector of countries
countries <- unique(data$country)

# Create a container
fem_fert <- vector("list", length = length(countries))
names(fem_fert) <- countries

# Load the data
for (i in countries) {
  cat("Country:", i, "\n")
try(fem_fert[[i]] <- readHFDweb(CNTRY = "AUT", item = "asfrRR", username = un, password = pw, fixup = T))
}

# Assemble the results
fem_fert <- rbindlist(fem_fert, idcol =  TRUE)

# Estimate the total fertility rate
fem_fert <- fem_fert[, .(tfr_fem = sum(ASFR), mac_fem = sum(Age * ASFR)/sum(ASFR)), by = .(.id, Year)]

# Combine the national data
nat <- merge(fem_fert, data, by.x = c(".id", "Year"), by.y = c("country", "year1"))
        
# Clean the data
nat <- clean_names(nat) |> 
  mutate(tfr_ratio = tfr_male / tfr_fem,
         mac_diff  = mac_male - mac_fem) |> 
  rename(code = id) |> 
  select(year, starts_with("tfr"), code, starts_with("mac")) |> 
  mutate(country = case_when(code == "AUS" ~ "Australia",
                             code == "CAN" ~ "Canada",
                             code == "DEUTE" ~ "East Germany",
                             code == "DEUTNP" ~ "Germany",
                             code == "DEUTW" ~ "West Germanz",
                             code == "DNK" ~ "Denmark",
                             code == "ESP" ~ "Spain",
                             code == "EST" ~ "Estland",
                             code == "FIN" ~ "Finland",
                             code == "FRA" ~ "France",
                             code == "GBRTENW" ~ "Great Britain",
                             code == "HUN" ~ "Hungary",
                             code == "ITA" ~ "Italy",
                             code == "JPN" ~ "Japan",
                             code == "POL" ~ "Poland",
                             code == "PRT" ~ "Portugal",
                             code == "TWN" ~ "Taiwan",
                             code == "SWE" ~ "Sweden",
                             code == "USA" ~ "United States"))

# Load the Mexican fertility data
load("Data/tfr_national_mexico.Rda")

# Attach country name
tfr_nat <- tfr_nat |> 
  mutate(code = "MEX", country = "Mexico")

# Attach the data
nat <- rbindlist(list(nat, tfr_nat), fill = T)

### Plot the results ==============================

ggplot() +
  geom_histogram(data = nat, aes(x = tfr_ratio, fill = "national"), alpha = .3) +
  geom_histogram(data = fert, aes(x = tfr_ratio, fill = "regional"), alpha = .3) +
  geom_vline(xintercept = 1) +
  scale_x_continuous("TFR ratio", expand = c(0, 0), trans = "log", n.breaks = 10) +
  scale_y_continuous("Count", expand = c(0, 0)) +
  scale_fill_brewer(name = "TFR:", palette = "Set1") +
  theme(legend.position = c(0.2, 0.8))
ggsave(last_plot(), filename = "figures/hist_cnt_reg_comp.pdf")

ggplot() +
  geom_bar(data = nat, stat = "bin", aes(x = tfr_ratio, y = ..density.., fill = "national"), alpha = .3) +
  geom_bar(data = fert, stat = "bin", aes(x = tfr_ratio, y = ..density.., fill = "regional"), alpha = .3) +
  geom_vline(xintercept = 1) +
  scale_x_continuous("TFR ratio", expand = c(0, 0), trans = "log", n.breaks = 10) +
  scale_y_continuous("Share (%)", expand = c(0, 0)) +
  scale_fill_brewer(name = "TFR:", palette = "Set1") +
  theme(legend.position = c(0.2, 0.8))
ggsave(last_plot(), filename = "figures/bar_cnt_reg_comp.pdf")
     
ggplot() +
  geom_density(data = subset(nat, year > 1990), aes(x = tfr_ratio, y = ..density.., fill = "national"), alpha = .3) +
  geom_density(data = subset(fert, year > 1990), aes(x = tfr_ratio, y = ..density.., fill = "regional"), alpha = .3) +
  geom_vline(xintercept = 1) +
  scale_x_continuous("TFR ratio", expand = c(0, 0), trans = "log", n.breaks = 10) +
  scale_y_continuous("Share (%)", expand = c(0, 0)) +
  scale_fill_brewer(name = "TFR:", palette = "Set1") +
  theme(legend.position = c(0.2, 0.8))

### Make it comparable

countries <- unique(fert$country)

ggplot() +
  geom_density(data = subset(nat, year > 1990 & country %in% countries), aes(x = tfr_ratio, y = ..density.., fill = "national"), alpha = .3) +
  geom_density(data = subset(fert, year > 1990 & country %in% countries), aes(x = tfr_ratio, y = ..density.., fill = "regional"), alpha = .3) +
  geom_vline(xintercept = 1) +
  scale_x_continuous("TFR ratio", expand = c(0, 0), trans = "log", n.breaks = 10) +
  scale_y_continuous("Share (%)", expand = c(0, 0)) +
  scale_fill_brewer(name = "TFR:", palette = "Set1") +
  theme(legend.position = c(0.2, 0.8))


# Show the variation
ggplot() +
  geom_vline(data = subset(nat, year == 2004 & country %in% countries), aes(xintercept = tfr_ratio, colour = country), size = 2) +
  geom_density(data = subset(fert, year == 2004 & country %in% countries), aes(x = tfr_ratio, y = ..density.., fill = country)) +
  geom_vline(xintercept = 1) +
  scale_x_continuous("TFR ratio", expand = c(0, 0), trans = "log") +
  scale_y_continuous("Share (%)", expand = c(0, 0)) +
  scale_fill_brewer(name = "TFR:", palette = "Set1") +
  scale_colour_brewer(name = "TFR:", palette = "Set1") +
  facet_wrap(~ country) +
  guides(colour = "none", fill = "none") +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15))

ggsave(last_plot(), filename = "Figures/dist_cntr_reg_2004.pdf")

# Change in the level of fertility ----------------------------------

# Change in the age gap
fert %>% 
  group_by(region) %>% 
  mutate(last_year = max(year), first_year = min(year)) %>% 
  filter(year == last_year | year == first_year) %>% 
  arrange(region, year) %>% 
  summarise(change = tfr_ratio - lag(tfr_ratio), 
            percent_change = (tfr_ratio - lag(tfr_ratio)) / lag(tfr_ratio), 
            span = year - lag(year), 
            country, 
            .groups = "drop") %>% 
  filter(!is.na(change)) %>% 
  mutate(decline = ifelse(change < 0, 1, 0)) %>% 
  group_by(country) %>% 
  summarise(share_decline = mean(decline), 
            level_decline = mean(change), 
            percent_decline = mean(percent_change) * 100,
            cases = n(), 
            speed = mean(decline / span))


# Change in the timing of fertility ---------------------------------

# Change in the age gap
fert %>% 
  group_by(region) %>% 
  mutate(last_year = max(year), first_year = min(year)) %>% 
  filter(year == last_year | year == first_year) %>% 
  arrange(region, year) %>% 
  summarise(change = mac_diff - lag(mac_diff), span = year - lag(year),  .groups = "drop") %>% 
  filter(!is.na(change)) %>% 
  mutate(decline = ifelse(change < 0, 1, 0)) %>% 
  summarise(share_decline = mean(decline), 
            level_decline = mean(change), 
            cases = n(), 
            speed = mean(decline / span))
