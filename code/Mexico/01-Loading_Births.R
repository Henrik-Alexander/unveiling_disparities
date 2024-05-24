### Load Births #########################################
# Purpose: raw birth data                               #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: cleaned birth data and functions       #
#########################################################

### Settings  ----------------------------------------------------------------

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")

# Path:
# https://en.www.inegi.org.mx/contenidos/programas/natalidad/microdatos/2021/natalidad_base_datos_2021_dbf.zip

### Preparations -------------------------------------------------------------

# Set the path
zipfile <- "U:/data/mex/birth_registers/Mexico"

# Years
years <- 1990:2021

# Create folders
if (!all(file.exists(paste0("Raw/Mexico/", years)))) map(paste0("Raw/Mexico/", years), dir.create, showWarnings = FALSE)

### Get and clean the data ---------------------------------------------------
  
# Load the Data
if (!file.exists(paste0("Raw/Mexico", years[1], "_dbf.zip"))) {
map(years, load_data_MEX, zipfile)
}

# Unzip the data
data <- map(years, unzip_MEX)

# Clean the names
data <- lapply(data, clean_names_MEX) 

# Clean the data
data <- lapply(data, clean_data_MEX) 
  
# Save the data
names(data) <- years

# Select
data <- lapply(data, select, c(entity, age_mot, age_fat, year, parity))

# Make a list of regions and years
data <- do.call(rbind, data)

# Transform the data into a tibble
data <- tibble(data)

# Save the data
save(data, file = "Data/births_complete_MEX.Rda")

### Plot the data -----------------------------------------------------

# Missing
missing <- data  |>
  group_by(year) |> 
  summarise(age_mot = mean(is.na(age_mot)),
            age_fat = mean(is.na(age_fat)))


data |>
  group_by(year) |> 
  summarise(NAs = mean(is.na(entity)))

# Plot the missing values
missing_plot <- ggplot(missing, aes(year, age_mot)) +
  geom_line(aes(col = "Age of Mother"),
            linewidth = 1.4) +
  geom_line(aes(y = age_fat,
                col = "Age of Father"),
            linewidth = 1.4) +
  ylab("Share missing") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.2),
                     expand = c(0, 0)) +
  scale_colour_manual(values = c("Age of Mother" = MPIDRyellow,
                                 "Age of Father" = MPIDRgreen)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Share of missing values for 'Age of mother' and 'Age of mother'")

ggsave(missing_plot, filename = "Figures/share_missing_sex.pdf")


### END ######################################################################  