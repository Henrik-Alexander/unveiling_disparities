#####################################
# Purpose: Clean births for Spain   #
# Author: Henrik-Alexander Schubert #
# E-mail: schubert@demogr.mpg.de    #
# Date: 15.10.2023                  #
#####################################


### Structure
# 1. Functions
# 2. Group births
# 3. Tranform the regional coding
# 4. Fill missing age of the father
# 5. Create age groups


rm(list = ls())

# Load the packages
library(tidyverse)
library(data.table)
library(splines)
library(modelr)
library(httr)
library(rvest)

# Load the functions
source("functions/functions.R")
source("functions/graphics.R")

# Load the births
path_esp <- "U:/data/esp/"
load(paste0(path_esp, "birth_register/births_imputed.Rda"))
dict <- readxl::read_xlsx("Raw/spain/prov_comm.xlsx")

# Dimensions
years   <- unique(births$year)
regions <- unique(births$Name)


### Functions ---------------------------------------------

span_eng <- function(vector = "íáúéóÁñ") {
  tmp <- vector
  tmp <- str_replace_all(tmp, "í", "i")
  tmp <- str_replace_all(tmp, "á", "a")
  tmp <- str_replace_all(tmp, "ú", "u")
  tmp <- str_replace_all(tmp, "é", "e")
  tmp <- str_replace_all(tmp, "ó", "o")
  tmp <- str_replace_all(tmp, "Á", "A")
  tmp <- str_replace_all(tmp, "ñ", "n")
  return(tmp)
}

### Group birth counts ------------------------------------

# Age range for men and women
age_m <- 15:59
age_f <- 15:55

# Age group labels
age_groups <- paste0(seq(15, 55, by = 5), "-", seq(19, 59, by = 5))

# Edit age variables: Restrict to age range (women)
births$age_mot[births$age_mot<min(age_f)] <- min(age_f)
births$age_mot[births$age_mot>max(age_f)] <- max(age_f)

# Create missings
births$age_fat[births$age_fat == 0] <- NA

# Edit age variables: Restrict to age range (men)
below <- paste(0:(min(age_m)-1))
above <- paste((max(age_m)+1):99)
births$age_fat[births$age_fat%in% below]  <- min(age_m)
births$age_fat[births$age_fat%in% above] <- max(age_m)

### Transform the provinces into communities -------------------------

# Load the data from wikipedia
path <- "https://en.wikipedia.org/wiki/Provinces_of_Spain"

# Reading in the table from Wikipedia
page <- read_html(path)

# Obtain the piece of the web page that corresponds to the "wikitable" node
dict <- html_node(page, ".wikitable")

# Convert the html table element into a data frame
dict <- html_table(dict, trim = T)

# Cleaning the variables
dict$community <- dict$`Autonomous community`
dict$province <- dict$`Province name`

# Subset the data
dict <- subset(dict, select = c(community, province))
dict <- as_tibble(dict)

# Clean the names
dict$province <- span_eng(dict$province)

# Split the name
prov <- str_split(dict$province, ";")
indicator <- ifelse(sapply(prov, length) > 1, T, F)

# Create the result
dict$final <- "" 
dict$final[!indicator] <- dict$province[!indicator]

# Crate the multi names
prov <- prov[indicator]
fill <- character(length = length(prov))
for (i in 1:length(prov)){ 
  tmp <- prov[[i]]
  spain <- str_detect(tmp, "(Spanish)")
  if (i == 13) spain <- c(T, F)
  res <- tmp[spain]
  res <- str_remove(res, "^ ")
  res <- str_remove(res, " \\(Spanish\\)")
  fill[i] <- res
}

# Fill the data
dict$final[indicator] <- fill
dict <- dict %>% select(final, community) %>% rename(province = final)

# Handle the exceptions
births$province[births$province == "Castellon de la Plana"] <- "Castellon"
births$province[births$province == "Coruna"] <- "La Coruna"
births$province[births$province == "Girona"] <- "Gerona"
births$province[births$province == "Palmas"] <- "Las Palmas"
births$province[births$province == "Baleares"] <- "Islas Baleares"
births$province[births$province == "Rioja"] <- "La Rioja"
births$province[births$province == "Lleida"] <- "Lerida"
births$province[births$province == "Huasca"] <- "Huesca"
births$province[births$province == "Guadalaljara"] <- "Guadalajara"

# Compare the dict with the births coding
prov_b <- unique(births$province)
prov_d <- unique(dict$province)
prov_b[prov_b %!in% prov_d]

# Merge births with provinces
births <- left_join(births, dict, by = c("province"))

# Fill in missing community with province label
births <- births %>% mutate(community = if_else(is.na(community), province, community))

# Create the community numbers
births <- births %>%
           mutate(community_nr = case_when(community == "Andalusia" ~ "01",
                                           community == "Aragon" ~ "02",
                                           community == "Asturias" ~ "03",
                                           community == "Balearic Islands" ~ "04",
                                           community == "Canary Islands" ~ "05",
                                           community == "Cantabria" ~ "06",
                                           community == "Castile and León" ~ "07",
                                           community == "Castilla-La Mancha" ~ "08",
                                           community == "Catalonia" ~ "09",
                                           community == "Valencian Community" ~ "10",
                                           community == "Extremadura" ~ "11",
                                           community == "Galicia" ~ "12",
                                           community == "Community of Madrid" ~ "13",
                                          # community == "" ~ "14",## Missing
                                           community == "Navarre" ~ "15",
                                           community == "Andalusia" ~ "16",
                                           community == "La Rioja" ~ "17",
                                           community == "Ceuta" ~ "18",
                                           community == "Melilla" ~ "19")) 

### Fill missing age of father ---------------------------------------

# Make a data.table
births <- as.data.table(births)

# Aggregate the data
births <- births[, .(births = sum(births)), by = .(year, community, age_mot, age_fat, community_nr)]

# Create births without missing values
b_nm <- births[!is.na(age_fat), ]
b_m  <- births[is.na(age_fat), ]

# How many missing values
print(paste("The share of missing values is", round(sum(b_m$births) / (sum(b_m$births, na.rm = T) + sum(b_nm$births, na.rm = T) ), 2), "%."))

# Estimate the distribution for non-missing values
b_nm[, total := sum(births), by = .(year, age_mot)]
dist <- b_nm[, .(share = sum(births) / unique(total)), by = .(year, age_mot, age_fat)]

# Plot the distribution
ggplot(dist, aes(x = age_fat, y = age_mot, fill = share) ) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ year)

# Remove the column for age of father in b_m
b_m[, age_fat := NULL]

# Combine the distribution with the missings
b_m <- merge(b_m, dist, on = c("age_mot", "year"), all = TRUE, allow.cartesian = TRUE)

# Estimate the new number of births
b_m[, births := births * share]

# Merge with the non-missing births
births <- merge(b_nm, b_m, by = c("age_mot", "age_fat", "year", "community", "community_nr"), suffixes = c("", "_missing"), all.x = TRUE, all.y = TRUE)

# Combine the birth counts
births[, births := ifelse(!is.na(births_missing), births + births_missing, births)]

# Remove helper columns
births[, `:=` (total = NULL, share = NULL, births_missing = NULL)]

# Replace NA with 0
births$births <- ifelse(is.na(births$births), 0, births$births)

# Filter non-missing regions
births <- births[!is.na(community), ]

### Create age groups -----------------------------------------------

# Births
births$age_mot_group <- cut(births$age_mot, breaks = seq(15, 55, by = 5), include.lowest = TRUE, left = TRUE, labels = age_groups[1:8])
births$age_fat_group <- cut(births$age_fat, breaks = seq(15, 60, by = 5), include.lowest = TRUE, left = TRUE, labels = age_groups[1:length(age_groups)]) 

# Assign group labels
births$age_fat_group <- age_groups[births$age_fat_group]
births$age_mot_group <- age_groups[births$age_mot_group]

# Cumulate the births for age groups
births <- births[, .(births = sum(births)), by = .(age_mot_group, age_fat_group, year, community, community_nr)]

# Save the births
save(births, file = "data/esp_births.Rda")

### END ##############################################################