########################################
# Purpose: Regression modelling        #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2024                     #
########################################

# Clean the environment
rm(list = ls()); gc(TRUE)

# Load the packages
library(data.table)
library(tidyverse)
library(janitor)
library(stargazer)
library(patchwork)
library(ggrepel)

options(scipen = 999)

# Load the functions
source("functions/graphics.R")

# Load the population data
load("data/asfr_deu.Rda")

# Load the bbirths
load("data/births_mother.Rda")

### Functions ----------------------------------------

# Estimate the tfr
calc_tfr <- function(births, exposure) sum(births/exposure)

# Save the figures
figs_sim <- function(plot, filename, height = 20, width = 25) {
  ggsave(plot, filename = paste0("figures/sim_", filename, ".pdf"),
         height = height, width = width, unit = "cm")
}

### Wrangling ---------------------------------------


# Change the parantheses
asfr_deu$age_group <- sub("\\[", "", as.character(asfr_deu$age_group))
asfr_deu$age_group <- sub("\\)", "", as.character(asfr_deu$age_group))
asfr_deu$age_group <- sub("\\]", "", as.character(asfr_deu$age_group))

# Rates
dta <- asfr_deu %>% 
  filter(year == 2018 & region == "Hamburg" & age_group != "50,55" & age_group != "55,60")


# Conversion factor
conv_fct <- mean(dta$exposure_female) / mean(dta$asfr_female)

# TFR ratio
tfr_ratio <- sum(dta$asfr_male) / sum(dta$asfr_male)

# Populations
ggplot(dta, aes(x = age_group, colour = sex, fill = sex)) + 
  geom_col(aes(y = exposure_male), position = "dodge", alpha = 0.3) +
  geom_line(aes(y = asfr * conv_fct, group = sex, linetype = sex), linewidth = 2) +
  scale_y_continuous("Population", labels = scales::unit_format(unit = "M", scale = 1e-6),
               sec.axis = sec_axis(~./conv_fct, name="Age-specific fertility rate"), expand = c(0, 0)) +
  scale_x_discrete("Age", expand = c(0, 0)) +
  scale_colour_brewer(palette = "Set1", name = "Gender") +
  scale_fill_brewer(palette = "Set1", name = "Gender") +
  scale_linetype(name = "Gender") +
  theme(legend.position = c(0.8, 0.8)) +
  labs(subtitle = paste0("Male TFR / Female TFR = ", round(tfr_ratio, 2)))


# Plot the graph
dta_long <-  dta %>% 
  pivot_longer(cols = c("birth", "pop", "asfr"), values_to = "value", names_to = "indicator") 

# Plot the rate differences
ggplot(subset(dta_long, indicator != "asfr"), aes(x = age_group, y = value, colour = sex, fill = sex)) + 
  geom_col(position = position_dodge(), alpha = 0.5) +
  geom_line(data = subset(dta_long, indicator == "asfr"), aes(y = value * conv_fct, group = sex), linewidth = 2) +
  scale_y_continuous("Births and exposures", labels = scales::unit_format(unit = "k", scale = 1e-3), expand = c(0, 0),
                     sec.axis = sec_axis(~./conv_fct, name="Age-specific fertility rate", breaks = seq(0, 0.1, by = 0.02))) +
  scale_x_discrete("Age", expand = c(0, 0)) +
  scale_colour_brewer(palette = "Set1", name = "Gender") +
  scale_fill_brewer(palette = "Set1", name = "Gender") +
  theme(legend.position = c(0.92, 0.8)) +
  labs(subtitle = paste0("Male TFR / Female TFR = ", round(tfr_ratio, 2)))
figs_sim(last_plot(), "obs")

### Population differences ---------------------------

# Estimate the tfr ratio while holding the births to the counterfactual of the women
tfr_ratio_popdiff <- calc_tfr(dta$birth[dta$sex == "female"], dta$pop[dta$sex == "male"]) / calc_tfr(dta$birth[dta$sex == "female"], dta$pop[dta$sex == "female"])


# Pivot widerr
dta_popdiff <- dta |> 
  pivot_wider(values_from = c("birth", "pop", "asfr", "exposure", "X"), names_from = "sex") |> 
  mutate(birth_male = birth_female,
         asfr_male = birth_male / exposure_male) |> 
  pivot_longer(cols = ends_with("ale"), names_pattern = "(.*)_(.*)", values_to = "value", names_to = c("indicator", "sex")) |> 
  filter(indicator %in% c("birth", "pop", "asfr"))


# Plot the rate differences
ggplot(subset(dta_popdiff, indicator != "asfr"), aes(x = age_group, y = value, colour = sex, fill = sex)) + 
  geom_col(position = position_dodge(), alpha = 0.5) +
  geom_line(data = subset(dta_popdiff, indicator == "asfr"), aes(y = value * conv_fct, group = sex), linewidth = 2) +
  scale_y_continuous("Births and exposures", labels = scales::unit_format(unit = "k", scale = 1e-3), expand = c(0, 0),
                     sec.axis = sec_axis(~./conv_fct, name="Age-specific fertility rate", breaks = seq(0, 0.1, by = 0.02))) +
  scale_x_discrete("Age", expand = c(0, 0)) +
  scale_colour_brewer(palette = "Set1", name = "Gender") +
  scale_fill_brewer(palette = "Set1", name = "Gender") +
  theme(legend.position = c(0.92, 0.8)) +
  labs(subtitle = paste0("Male TFR / Female TFR = ", round(tfr_ratio_popdiff, 2)))
figs_sim(last_plot(), "pop_diff")

### Rate difference ----------------------------------

#  Estimate the rate difference
tfr_ratio_ratediff <- calc_tfr(dta$birth[dta$sex == "male"], dta$exposure[dta$sex == "female"]) / calc_tfr(dta$birth[dta$sex == "female"], dta$exposure[dta$sex == "female"])

# Pivot widerr
dta_ratediff <- dta |> 
  pivot_wider(values_from = c("birth", "pop", "asfr", "exposure", "X"), names_from = "sex") |> 
  mutate(exposure_male = exposure_female,
         asfr_male = birth_male / exposure_male) |> 
  pivot_longer(cols = ends_with("ale"), names_pattern = "(.*)_(.*)", values_to = "value", names_to = c("indicator", "sex")) |> 
  filter(indicator %in% c("birth", "exposure", "asfr"))

# Plot the rate differences
ggplot(subset(dta_ratediff, indicator != "asfr"), aes(x = age_group, y = value, colour = sex, fill = sex)) + 
  geom_col(position = position_dodge(), alpha = 0.5) +
  geom_line(data = subset(dta_ratediff, indicator == "asfr"), aes(y = value * conv_fct, group = sex), linewidth = 2) +
  scale_y_continuous("Births and exposures", expand = c(0, 0), labels = scales::unit_format(unit = "k", scale = 1e-3),
                     sec.axis = sec_axis(~./conv_fct, name="Age-specific fertility rate", breaks = seq(0, 0.1, by = 0.02))) +
  scale_x_discrete("Age", expand = c(0, 0)) +
  scale_colour_brewer(palette = "Set1", name = "Gender") +
  scale_fill_brewer(palette = "Set1", name = "Gender") +
  theme(legend.position = c(0.92, 0.8)) +
  labs(subtitle = paste0("Male TFR / Female TFR = ", round(tfr_ratio_ratediff, 2)))
figs_sim(last_plot(), "births_diff")

### END ##############################################