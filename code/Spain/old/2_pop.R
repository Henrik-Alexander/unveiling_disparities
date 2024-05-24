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

# Dimensions
years   <- unique(births$year)
regions <- unique(births$Name)

#### Wrangle the pop -----------------------------------------

# Clean the names
oldnames <- c("Provincias", "Edad (grupos quinquenales)", "Españoles/Extranjeros",  "Sexo", "Año", "Total")
newnames <- c("province", "age", "spanish", "sex", "year", "pop")
setnames(pop, oldnames, newnames)

# Make small letters
vars <- c("province", "spanish", "sex")
pop[, (vars) := lapply(.SD, str_to_lower), .SDcols = vars ]

# Filter variables
pop <- pop[province != "total españa", ]
pop <- pop[age != "TOTAL EDADES", ]
pop <- pop[spanish == "total", ]
pop <- pop[sex != "ambos sexos", ]

# Clean the age column
pop$age <- str_remove(pop$age, " años")
ages <- str_split(pop$age, "-", simplify = TRUE)
colnames(ages) <- c("lower", "upper")
pop <- cbind(pop, ages)

# Clean the name column
pop[, province := str_remove(province, "[0-9]+ ")]

# Clean the names
pop[, Name := fcase(
  province == "albacete", "Albacete",
  province == "Albacete", "Alicante",
  province == "albacete", "Albacete",
  province == "alicante/alacant", "Alicante", 
  province == "almería", "Almeria",
  province == "araba/álava" , "Alava",
  province == "asturias", "Asturias",       
  province == "ávila", "Avila",      
  province == "badajoz", "Badajoz",
  province == "barcelona", "Barcelona",
  province == "bizkaia", "Bizkaia",
  province == "burgos", "Burgos",    
  province == "cáceres", "", # MISSING!
  province == "cádiz", "Cadiz",
  province == "cantabria", "Cantabria",        
  province == "castellón/castelló", "Castellon de la Plana",
  province == "ciudad real", "Ciudad Real",
  province == "córdoba", "Cordoba",
  province == "cuenca", "Cuenca", 
  province == "gipuzkoa", "", # MISSING!
  province == "girona", "Girona",
  province == "granada", "Granada",     
  province == "guadalajara", "Guadalaljara",
  province == "huelva", "Huelva",  
  province == "huesca", "Huasca",    
  province == "jaén", "Jaen",
  province == "león", "Leon",         
  province == "lleida", "Lleida",  
  province == "lugo", "Lugo",    
  province == "madrid", "Madrid",
  province == "málaga", "Malaga",
  province == "murcia", "Murcia",    
  province == "navarra", "Navarra",
  province == "ourense", "Orense",
  province == "palencia", "Palencia",
  province == "pontevedra", "Pontevedra",
  province == "salamanca", "Salamanca",
  province == "santa cruz de tenerife", "Santa Cruz de Tenerife",
  province == "segovia", "Segovia",
  province == "sevilla", "Sevilla", 
  province == "soria", "Soria", 
  province == "tarragona", "Tarragona",       
  province == "teruel", "Teruel",
  province == "toledo", "Toledo",
  province == "valencia/valència", "Valencia",    
  province == "valladolid", "Valladolid",
  province == "zamora", "Zamora",
  province == "zaragoza", "Zaragoza",
  province == "ceuta", "Ceuta",
  province == "melilla" , "Melilla"
  
)]

# Make population counts numeric
pop$pop <- as.numeric(str_remove(pop$pop, "\\."))

# Create the lower for sorting
pop$lower <- as.numeric(str_extract(pop$age, "[0-9]+"))

pop |> 
     filter(province == "lleida" & year == 2008) |> 
     ggplot(aes(x = lower, y = pop, colour = sex)) + 
     geom_line(linewidth = 2) +
     scale_colour_brewer(palette = "Set1")

### Smooth the births ----------------------------------------

# Set the knots
knots <- seq(from = 25, to = 75, by = 25)

# Take the logarithm of population counts
pop$log_pop <- ifelse(pop$pop == 0, 0, log(pop$pop))

# Specific births
pop_nest <- pop |> 
  group_by(year, Name, sex) |>
  nest()

# Splines
spline_model <- function(df) {
  glm(log_pop ~ bs(lower, knots = knots), data = df, family = "gaussian")
}

# Estimate the model
pop_nest <- pop_nest |>
  mutate(model = map(data, spline_model))

# Simulate the data
predictions <- expand.grid(year = unique(pop$year), Name = unique(pop$Name), sex = unique(pop$sex), lower = 0:105) |> 
  group_by(year, Name, sex) |>
  nest() |> 
  rename(prediction = data)

# Get the predictions
predictions <- left_join(pop_nest, predictions, by = c("year", "Name", "sex")) |> 
  mutate(pred = map2(prediction, model, add_predictions)) 

# Unnest predictions
pop_smoothed <- unnest(predictions, pred) |> 
  rename(age = lower) |> 
  select(sex, year, Name, age, pred) 

# Join with population data
pop_smoothed <- left_join(pop_smoothed, pop, by = c("year", "Name" = "Name", "sex", "age" = "lower"), suffix = c("", "_grouped"))

# Transform back into normal scale
pop_smoothed$pred <- exp(pop_smoothed$pred)

# Remove the last two age intervals
pop <- pop_smoothed |>
  filter(age != "95-99" & age != "100 y más") |> 
  select( -spanish, -log_pop) 

# Make a births.table
pop <- as.data.table(pop)

# Estimate the exposures
pop <- pop[order(Name, sex, age, year), ]
pop <- pop[!is.na(age_grouped) & Name != "", ]
pop <- pop[, .(exposure = (pop + shift(pop, n = 1, type = "lead")),
               exposure_smooth = (pred + shift(pred, n = 1, type = "lead")),
               year), .(Name, sex, age_grouped)]

# Save the data
save(pop, file = "Data/esp_pop.Rda")

### Plot the smoothed ------------------------------------------

# Plot the population births
pop_smoothed |> 
  filter(Name == "Madrid" & year %in% c(1998, 2022)) |> 
  ggplot(aes(group = interaction(year, sex))) +
  geom_line(aes(x = age, y = pred, linetype = "Smoothed")) +
  geom_line(data = subset(pop_smoothed, !is.na(province) & Name == "Madrid" & year %in% c(1998, 2022)), aes(x = age, y = pop, linetype = "Observed")) +
  scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 1e-3), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_linetype_manual(values = c("solid", "dashed"), name = "births:") +
  facet_grid(sex ~ year) +
  ylab("Population counts") + xlab("Age") +
  theme(panel.spacing.x = unit(0.5, "cm"))

# Save the result
ggsave(last_plot(), filename = "Figures/esp_spline_smooth_pop.pdf")

### END ##############################