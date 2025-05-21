########################################
# Project: Subnational birth squeezes #
# Purpose: Clean columbia data        #
# Author: Henrik-Alexander Schubert   #
# E-mail: schubert@demogr.mgp.de      #
# Date: 05.06.2024                    #
#######################################

# Load the packages
library(tidyverse)
library(data.table)
library(haven)

# Functions --------------------------

# Replace a number with missing
set_na <- function(variable, number = 99) {
  if(is.character(number)) number <- as.numeric(number)
  return(ifelse(variable == number, NA, variable))
}

# Make to two digit
make_2digit <- function(variable) {
  ifelse(nchar(variable) == 1, paste("0", variable), as.character(variable))
}

# Clean the age variable
clean_age <- function(age){
  labels <- paste(seq(10, 50, by = 5), seq(14, 54, by = 5), sep = "-")
  return(labels[age]) 
}

# Reset the department of birth
reset_code_dpto <- function (dpto) {
  dpto <- make_2digit(dpto)
  dpto[dpto == "05"] <- "Antioquia"
  dpto[dpto == "08"] <- "Atlántico"
  dpto[dpto == "11"] <- "Bogotá"
  dpto[dpto == "13"] <- "Bolivar"
  dpto[dpto == "15"] <- "Boyaca"
  dpto[dpto == "17"] <- "Caldas"
  dpto[dpto == "18"] <- "Caqueta"
  dpto[dpto == "19"] <- "Cauca"
  dpto[dpto == "20"] <- "Cesar"
  dpto[dpto == "23"] <- "Cordoba"
  dpto[dpto == "25"] <- "Cundinamarca"
  dpto[dpto == "27"] <- "Choco"
  dpto[dpto == "41"] <- "Huila"
  dpto[dpto == "44"] <- "La guajira"
  dpto[dpto == "47"] <- "Magdalena"
  dpto[dpto == "50"] <- "Meta"
  dpto[dpto == "52"] <- "Nariño"
  dpto[dpto == "54"] <- "Norte de Santander"
  dpto[dpto == "63"] <- "Quindio"
  dpto[dpto == "66"] <- "Risaralda"
  dpto[dpto == "68"] <- "Santander"
  dpto[dpto == "70"] <- "Sucre"
  dpto[dpto == "73"] <- "Tolima"
  dpto[dpto == "76"] <- "Valle del Cauca"
  dpto[dpto == "81"] <- "Arauca"
  dpto[dpto == "85"] <- "Casanare"
  dpto[dpto == "86"] <- "Putumayo"
  dpto[dpto == "88"] <- "Archipelago of San Andrés, Providencia and Santa Catalina"
  dpto[dpto == "91"] <- "Amazonas"
  dpto[dpto == "94"] <- "Guainía"
  dpto[dpto == "95"] <- "Guaviare"
  dpto[dpto == "97"] <- "Vaupés"
  dpto[dpto == "99"] <- "Vichada"
  return(dpto)
}

# Translate the header
translate_header <- function(header) {
  header <- gsub("ano", "year", header)
  header <- gsub("edad", "age", header)
  header <- gsub("padre", "father", header)
  header <- gsub("madre", "mother", header)
}

impute_father <- function (births) {
  
  # Altersverteilung des Vaters der Geburten (ohne Missings)
  births_tmp_f <- aggregate(birth ~ age_father, data = births, sum)
  
  # Alter des Mutters bei Alter des Vaters fehlend
  births_na <- births[is.na(births$age_father), ]
  
  # Imputiere Alter des Vaters, falls es fehlende Werte gibt
  if (nrow(births_na) > 0) {
    
    # Aggregiere die konditionale Verteilung des Alter der Mutter 
    missing <- aggregate(birth ~ age_mother, data = births_na, sum)
    missing_age <- unique(missing$age_mother)
    
    for (j in missing_age) {
      
      # Fallback option: if absolute no births for the age of the mother
      all_missing <- births$birth[births$age_mother == j & !is.na(births$age_father)]
      all_missing <- all(is.na(all_missing))
      
      if (all_missing) {
        fathers <- data.frame(age_father = j+3, birth = missing$birth[missing$age_mother == j]) 
      } else {
        # Aggregiere die Geburten nach dem Alter des Vaters
        fathers <- births %>% 
          filter(age_mother == j & !is.na(age_father)) %>% 
          group_by(age_father) %>% 
          summarise(birth = sum(birth), .groups = "drop")
      }
      
      # Berechne die konditionale Verteilung der Geburten nach dem Alter der Mutter: die Summe ist 1
      fathers$count <- fathers$birth / sum(fathers$birth) 
      
      # Erstelle einen Vektor für die Lokation der Ausprägungn
      matching_ages <- match(fathers$age_father, births_tmp_f$age_father)
      
      # Mutlipliziere die konditionale Verteilung mit den unbeobachteten Geburten
      fathers$births <- missing$birth[missing$age_mother == j] * fathers$count
      
      # Addiere die beobachteten und imputierten Geburten
      births_tmp_f$birth[matching_ages] <- births_tmp_f$birth[matching_ages] + fathers$births
      
    }
  }
  return(births_tmp_f)
}

# Aggregiere die Geburten nach dem Alter der Mutter
aggregate_mother <- function (births) {
  births_mother <- aggregate(birth ~ age_mother, data = births, sum)
  return(births_mother)
}

# Aggregat births
aggregate_births <- function(birth, age, minage = 10, maxage = 59) {
  age[age>maxage] <- maxage
  age[age<minage] <- minage
  labels <- paste(seq(minage, maxage, by = 5), seq(minage+4, maxage, by = 5), sep = "-")
  age_father <- cut(age, breaks = seq(minage, maxage, by = 5), include.lowest = T, right = F, labels = FALSE)
  age_father <- labels[age_father]
  aggregate(birth ~ age_father, FUN = sum)
}


# 1. Load and clean the data ---------------------

# Set the path to the data
files <- list.files("U:/data/col/births", pattern = ".sav$", full.names = T)

# Create a container for the results
births <- vector("list", length = length(files))

for (i in seq_along(files)) {
  
    cat("Iteration:", i, "\n")
  
  # Load the data
  df <- haven::read_sav(files[i])
  names(df) <- str_to_lower(names(df))
  df <- df[, c("ano", "edad_madre", "edad_padre", "cod_dpto")]
  
  # Clean the different variables
  df$edad_madre <- set_na(df$edad_madre)
  df$edad_padre <- set_na(df$edad_padre, number = max(df$edad_padre))
  #df$edad_madre <- clean_age(df$edad_madre)
  df$cod_dpto <- as.character(df$cod_dpto)
  df$region <- reset_code_dpto(df$cod_dpto)
  df$ano <- as.numeric(df$ano)
  
  # Translate the header
  names(df) <- translate_header(names(df))
  
  # Assign the result
  births[[i]] <- df
  
}

# Combine the data
#births <- bind_rows(births)

# 2. Impute the births -----------------------------


clean_birth_data <- function(birth) {

  # Get the year
  year <- unique(birth$year)
  
  # Split by region
  birth <- split(birth, birth$region)
  
  # Impute age of mother (randomly)
  impute_missing_maternal <- function(birth) {
    mother_ages <- unique(unique(birth$age_mother))
    mother_ages <- mother_ages[!is.na(mother_ages)]
    birth$age_mother[is.na(birth$age_mother)] <- sample(mother_ages, replace = T, size = sum(is.na(birth$age_mother)))
    return(birth)
  }
  birth <- lapply(birth, impute_missing_maternal)
  
  
  # Add a one for every birth
  birth <- lapply(birth, function(x) {x$birth <- 1; return(x)})
  
  # Aggregate births by maternal age
  birth_m <- lapply(birth, function(x) aggregate(birth ~ age_mother, FUN = sum, data = x))
  
  # Impute the father ages
  birth_f <- lapply(birth, impute_father)
  
  # Aggregate births by father's age
  birth_f <- lapply(birth_f, function(x) aggregate_births(x$birth, x$age))
  
  # Combine the results
  birth_f <- bind_rows(birth_f, .id = "region")
  birth_f <- rename(birth_f, age_group = age_father)
  birth_m <- bind_rows(birth_m, .id = "region")
  birth_m$age_group <- clean_age(birth_m$age_mother)
  
  
  # Combine the data
  birth <- inner_join(birth_f, birth_m, by = c("region", "age_group"), suffix = c("_father", "_mother"))
  birth$year <- year
  
  return(birth)
}

# Clean the birth data
births <- lapply(births, clean_birth_data)

# Combine the data
births <- rbindlist(births)

# Save the births
save(births, file = "data/births_columbia.Rda")

### END ####################################################