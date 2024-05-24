#################################################################################
#                        Revisiting the J-Shape                                 #
#                   Edit and Aggregate Birth Counts                             #
#################################################################################

rm(list = ls())

### Edit population data ######################################################

## Last edited: Jan 15 2022

## Data downloaded from:
## https://www.nber.org/data/seer_u.s._county_population_data.html
## https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-detail.html


### Packages & settings #######################################################

# Packages
library(data.table)
library(tidyverse)
library(janitor)

# load functions
source("Functions/Functions.R")

# Years to cover
years <- 1969:2004


# Output variable names
var_names <- c("year","state","sex","age","pop")

### Packages & settings #######################################################

# Set the path
path <- "https://seer.cancer.gov/popdata/yr1969_2020.singleages/us.1969_2020.singleages.adjusted.txt.gz"

# Temporary file direction
temp <- tempfile()

# Download the file
download.file(path, temp, quite = T)

# Load the data
pop <- read.table(gzfile(temp))

# Split the data
d <- pop$V1

# Create different vectors
year <- str_sub(d, start = 1, end = 4)
state <- str_sub(d, start = 5, end = 6)
fips <- str_sub(d, start = 7, end = 8)
space <- str_sub(d, start = 9, end = 11)
geo <- str_sub(d, start = 12, end = 13)
race <- str_sub(d, start = 14, end = 14)
origin <- str_sub(d, start = 15, end = 15)
sex <- str_sub(d, start = 16, end = 16)
age <- str_sub(d, start = 17, end = 18)
pop <- str_sub(d, start = 19, end = 27)

# Combine the data
pop <- tibble(year, state, race, origin, sex, age, pop)

# Clean the labels
pop <- pop |> mutate(across(c(year, origin, sex, age, pop), as.numeric))

### Read and edit data ########################################################

# Aggregate
pop <- aggregate(pop~year+state+sex+age,data=pop,FUN=sum)

# Edit state variable
pop$state <- State_abbr_to_nr(pop$state)

# Save
save(pop,file="Data/US_states_pop.rda")

###   END   ####################################################################  