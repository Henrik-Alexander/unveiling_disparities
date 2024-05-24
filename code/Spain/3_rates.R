#####################################
# Purpose: Estimate rates for Spain #
# Author: Henrik-Alexander Schubert #
# E-mail: schubert@demogr.mpg.de    #
# Date: 15.10.2023                  #
#####################################

rm(list = ls())

# Load the packages
library(tidyverse)
library(data.table)
library(splines)
library(modelr)

# Load the functions
source("functions/functions.R")
source("functions/graphics.R")

# Load the data
load("Data/esp_births.Rda")
load("Data/esp_pop.Rda")

# Keep pop for age groups
pop <- pop[!is.na(age_grouped), .(sex, year, Name, age_grouped, exposure, exposure_smooth)]

### Merge population with births --------------------------

# Sex selective
pop_m <- pop[sex == "hombres", ]
pop_f <- pop[sex == "mujeres", ]

# Create male and female births
births_f <- births[, .(births = sum(births)), .(age_mot_group, year, province)]
births_m <- births[, .(births = sum(births)), .(age_fat_group, year, province)]

# Join
asfr_m <- merge(births_m, pop_m, by.x = c("age_fat_group", "province", "year"), by.y = c("age_grouped", "Name", "year"), all.x = TRUE)
asfr_f <- merge(births_f, pop_f, by.x = c("age_mot_group", "province", "year"), by.y = c("age_grouped", "Name", "year"), all.x = TRUE)

### Recode spanish regions ---------------------------------------

#### Estiamte the rates ------------------------------------------


# Combine the data
asfr_esp <- merge(asfr_f, asfr_m,
                  by.x = c("province", "year", "age_mot_group"),
                  by.y = c("province", "year", "age_fat_group"), 
                  all.x = T, all.y = T, 
                  suffixes = c("_female", "_male"))

# Clean the data
asfr_esp <- asfr_esp[, .(age = age_mot_group), .(province, year, births_female, births_male, exposure_male, exposure_female, exposure_smooth_female, exposure_smooth_male)]

# Recode the regions
asfr_esp$region <- ""
asfr_esp[asfr_esp$province %in% c("Almeria", "Cadiz", "Cordoba", "Granada", "Huelva", "Jaen", "Malaga", "Sevilla"), ]$region <- "Andalucia"
asfr_esp[asfr_esp$province %in% c("Huasca", "Teruel"), ]$region <- "Aragon"
asfr_esp[asfr_esp$province %in% c("Canarias"), ]$region <- "Canarias"
asfr_esp[asfr_esp$province %in% c("Asturias"), ]$region <- "Cantabria"
asfr_esp[asfr_esp$province %in% c("Leon", "Valladolid", "Soria", "Avila", "Zamora", "Palencia", "Salamanca", "Burgos"), ]$region <- "Castilla y Leon"
asfr_esp[asfr_esp$province %in% c("Albacete", "Ciudad Real", "Guadalaljara", "Toledo", "Cuenca"), ]$region <- "Castilla-la Mancha"
asfr_esp[asfr_esp$province %in% c("Barcelona", "Girona", "Lleida", "Tarragona"), ]$region <- "Cataluna"
asfr_esp[asfr_esp$province %in% c("Ceuta"), ]$region <- "Ciudad Autonoma de Ceuta"
asfr_esp[asfr_esp$province %in% c("Melilla"), ]$region <- "Ciudad Autonoma de Melilla"
asfr_esp[asfr_esp$province %in% c("Madrid"), ]$region <- "Comunidad de Madrid"
asfr_esp[asfr_esp$province %in% c("Navarra"), ]$region <- "Comunidad Foral de Navarra"
asfr_esp[asfr_esp$province %in% c("Valencia", "Castellon de la Plana", "Alicante"), ]$region <- "Comunidad Valenciana"
asfr_esp[asfr_esp$province %in% c("Badajoz"), ]$region <- "Extremadura"
asfr_esp[asfr_esp$province %in% c("Lugo", "Orense", "Pontevedra"), ]$region <- "Galicia"
#asfr_esp[asfr_esp$province %in% c(""), ]$region <- "Illes Balears"
#asfr_esp[asfr_esp$province %in% c(""), ]$region <- "La Rioja"
asfr_esp[asfr_esp$province %in% c("Alava"), ]$region <- "Pais Vasco"
asfr_esp[asfr_esp$province %in% c("Avila"), ]$region <- "Principado de Asturias"
#asfr_esp[asfr_esp$province %in% c(), ]$region <- "Region de Murcia"


# Filter the births
asfr_esp <- asfr_esp[!is.na(exposure_male) & !is.na(births_male)]

# Group by provinces
asfr_esp <- asfr_esp[, .(asfr_female = sum(births_female, na.rm = T) / sum(exposure_female, na.rm = T),
                         asfr_male = sum(births_male, na.rm = T) / sum(exposure_male, na.rm = T),
                         asfr_female_smooth = sum(births_female, na.rm = T) / sum(exposure_smooth_female, na.rm = T),
                         asfr_male_smooth = sum(births_male, na.rm = T) / sum(exposure_smooth_male, na.rm = T)),
                     by = .(year, region, age)]

# Create middle age group
asfr_esp$middle <- as.numeric(str_sub(asfr_esp$age, 1, 2))

# Estimate TFRs
tfr_esp <- asfr_esp[, .(tfr_female = sum(5 * asfr_female, na.rm = T),
                        tfr_male   = sum(5 * asfr_male, na.rm = T),
                        tfr_female_smooth = sum(5 * asfr_female_smooth, na.rm = T),
                        tfr_male_smooth = sum(5 * asfr_male_smooth, na.rm = T),
                        mac_male   = sum(asfr_male * middle, na.rm = T) / sum(asfr_male, na.rm = T),
                        mac_female   = sum(asfr_female * middle, na.rm = T) / sum(asfr_female, na.rm = T),
                        mac_male_smooth   = sum(asfr_male_smooth * middle, na.rm = T) / sum(asfr_male_smooth, na.rm = T),
                        mac_female_smooth   = sum(asfr_female_smooth * middle, na.rm = T) / sum(asfr_female_smooth, na.rm = T)), .(region, year)]


# Remove Principado de Asturias
asfr_esp <- asfr_esp[region != "Principado de Asturias", ]
tfr_esp <- tfr_esp[region != "Principado de Asturias", ]

# Save the result
save(asfr_esp, file = "data/asfr_esp.Rda")
save(tfr_esp, file = "data/tfr_esp.Rda")

### Plot the births ----------------------------------------------

asfr_esp |> 
  filter(name %in% c("Barcelona", "Alava") & year %in% c(1998, 2020)) |> 
  ggplot(aes(x = age, group = year)) +
   geom_line(aes(y = asfr_female, colour = "Female", linetype = "Female"), linewidth = 1.8) +
   geom_line(aes(y = asfr_male, colour = "Male", linetype = "Male"), linewidth = 1.8) +
  facet_wrap(year ~ name, labeller = label_wrap_gen(multi_line=FALSE)) +
  scale_colour_manual(name = "TFR", values = c("firebrick", "navyblue")) +
  scale_linetype_manual(name = "TFR", values = c("solid", "dotdash")) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("TFR") + xlab("Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank())

ggsave(last_plot(), filename = "Figures/esp_asfr_reg.pdf")


### Export births for Jutta -------------------------------------

pop_jutta <- pop[name == reg[1] & year == 2016 & sex == "hombres", ]
pop_jutta$pop <- round(pop_jutta$pop)
pop_jutta <- pop_jutta[, .(name, agegroup = age, lower, upper, year, pop)]
pop_jutta <- as.data.frame(pop_jutta)
births_jutta <- births[name == reg[1] & year == 2016, ]
births_jutta$births <- round(births_jutta$births)
births_jutta <- births_jutta[, .(births = sum(births)),  .(name, year, age_fat)]
births_jutta <- as.data.frame(births_jutta)
save(births_jutta, file = "births/jutta_births.Rda")
save(pop_jutta, file = "births/jutta_pop.Rda")
