#####################################
# Purpose: Estimate population      #
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

# Load the births
path_esp <- "U:/data/esp/"
load(paste0(path_esp, "birth_register/births_imputed.Rda"))
pop <- fread(paste0(path_esp, "pop_counts/pop_esp_prov_1998-2022.csv"), encoding = "UTF-8")
pop <-  fread(paste0(path_esp, "pop_counts/pop_esp_community_1998-2022.csv"), encoding = "Latin-1")

# Dimensions
years   <- unique(births$year)
regions <- unique(births$Name)

#### Wrangle the pop -----------------------------------------

# Clean the names
oldnames <- c("Total Nacional", "Comunidades y Ciudades Autónomas", "Edad (grupos quinquenales)", "Españoles/Extranjeros",  "Sexo", "Año", "Total,")
newnames <- c("data", "community", "age", "spanish", "sex", "year", "pop")
setnames(pop, oldnames, newnames)

# Make small letters
vars <- c("community", "spanish", "sex")
pop[, (vars) := lapply(.SD, str_to_lower), .SDcols = vars ]

# Filter variables
pop <- pop[community != "total españa", ]
pop <- pop[age != "TOTAL EDADES", ]
pop <- pop[spanish == "total", ]
pop <- pop[sex != "ambos sexos", ]

# Clean the age column
pop$age <- str_remove(pop$age, " años")
ages <- str_split(pop$age, "-", simplify = TRUE)
colnames(ages) <- c("lower", "upper")
pop <- cbind(pop, ages)

# Clean the name column
pop[, community_nr := str_extract(community, "[0-9]+")]
pop[, community := str_remove(community, "[0-9]+ ")]


# Make population counts numeric
pop$pop <- as.numeric(str_remove(str_remove(pop$pop, "\\."), ","))

# Create the lower for sorting
pop$lower <- as.numeric(str_extract(pop$age, "[0-9]+"))

plot_pop <- function(year) {
plotting <- pop |> 
  filter(year == {{year}}) |> 
  ggplot(aes(x = lower, y = pop, colour = sex)) + 
  geom_line(linewidth = 2) +
  facet_wrap(~ community) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle(paste0("Population in year ", {{year}}))
return(plotting)
}

for(i in 1998:2022) {
  Sys.sleep(2)  
  plot_pop(i)
}
