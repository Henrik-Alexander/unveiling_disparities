######################################
# Purpose: Load population data      #
# Author: Henrik-Alexander Schubert  #
# E-Mail: schubert@demogr.mpg.de     #
# Date: 10.10.2023                   #
######################################

### Preperations -------------------------------------------------------------

# Load the packages
library(httr)
library(rvest)
library(foreign)

# Load the functions
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")

# Data was downloaded from:
# download at : https://www.ined.fr/fichier/s_rubrique/159/estim.pop.nreg.sexe.gca.1975.2022.fr.xls

### Load the births -------------------------------------------

# Specify the path
path_nais <- "U:/data/fra/birth_registers/nais"

# Specify the years
years <- 1998:2013

# Create a container
births <- vector("list", length = length(years))

# Load the data
for (i in seq_along(years)) {
  
  cat("Year:", years[i], "\n")
  
  # Load the file
  file <- paste0(path_nais, years[i], "/", "NAIS", years[i], ".dbf")
  tmp  <- read.dbf(file) 
  
  # Basic cleaning of variable names
  tmp <- clean_names(tmp)
  
  # Get Age father, mother, residence, and year
  tmp <- subset(tmp, select = c(agexactm, agexactp, depdom, anais))
  
  births[[i]] <- tmp
}

# Bind the results together
births <- bind_rows(births)

# Change variable names
names(births) <- c("age_mother", "age_father", "department", "year")

### Create region names -----------------------------------

# Load the value expressions
varlist <- read.dbf(paste0(path_nais, years[i], "/", "varmod_naissances.dbf"))
varlist <- subset(varlist, subset = VARIABLE == "DEPDOM")
varlist$MODLIBELLE <- as.character(varlist$MODLIBELLE)
varlist <- clean_names(varlist)
varlist <- subset(varlist, select = c(modalite, modlibelle))

# Create a missing varlue for region
varlist$region <- NA

# Filter matches
vars <- varlist$modlibelle
vars[str_detect(vars, pattern = "Ari")]

# Create the Ile de France
varlist[varlist$modlibelle == "Paris", ]$region <- "Ile-de-France"
varlist[varlist$modlibelle == "Seine-et-Marne", ]$region <- "Ile-de-France"
varlist[varlist$modlibelle == "Yvelines", ]$region <- "Ile-de-France"
varlist[varlist$modlibelle == "Essonne", ]$region <- "Ile-de-France"
varlist[varlist$modlibelle == "Hauts-de-Seine", ]$region <- "Ile-de-France"
varlist[varlist$modlibelle == "Seine-Saint-Denis", ]$region <- "Ile-de-France"
varlist[varlist$modlibelle == "Val-de-Marne", ]$region <- "Ile-de-France"
varlist[varlist$modlibelle == "Val-d'Oise", ]$region <- "Ile-de-France"

# Create Auvergne
varlist[varlist$modlibelle == "Allier", ]$region <- "Auvergne"
varlist[varlist$modlibelle == "Cantal", ]$region <- "Auvergne"
varlist[varlist$modlibelle == "Haute-Loire", ]$region <- "Auvergne"
varlist[varlist$modlibelle == "Puy-de-D“me", ]$region <- "Auvergne"

# Create Rhone de alpes
varlist[varlist$modlibelle == "Ain", ]$region <- "Rhône-Alpes"
varlist[varlist$modlibelle == "ArdŠche", ]$region <- "Rhône-Alpes"
varlist[varlist$modlibelle == "Dr“me", ]$region <- "Rhône-Alpes"
varlist[varlist$modlibelle == "IsŠre", ]$region <- "Rhône-Alpes"
varlist[varlist$modlibelle == "Loire", ]$region <- "Rhône-Alpes"
varlist[varlist$modlibelle == "Rh“ne", ]$region <- "Rhône-Alpes"
varlist[varlist$modlibelle == "Savoie", ]$region <- "Rhône-Alpes"
varlist[varlist$modlibelle == "Haute-Savoie", ]$region <- "Rhône-Alpes"

# Create Aquitaniebn
varlist[varlist$modlibelle == "Dordogne", ]$region <- "Aquitanien"
varlist[varlist$modlibelle == "Gironde", ]$region <- "Aquitanien"
varlist[varlist$modlibelle == "Landes", ]$region <- "Aquitanien"
varlist[varlist$modlibelle == "Lot-et-Garonne", ]$region <- "Aquitanien"
varlist[varlist$modlibelle == "Pyr‚n‚es-Atlantiques", ]$region <- "Aquitanien"

# Create Poitou-Charentes
varlist[varlist$modlibelle == "Charente", ]$region <- "Poitou-Charentes"
varlist[varlist$modlibelle == "Charente-Maritime", ]$region <- "Poitou-Charentes"
varlist[varlist$modlibelle == "Deux-SŠvres", ]$region <- "Poitou-Charentes"
varlist[varlist$modlibelle == "Vienne", ]$region <- "Poitou-Charentes"

# Create Limousin
varlist[varlist$modlibelle == "CorrŠze", ]$region <- "Limousin"
varlist[varlist$modlibelle == "Creuse", ]$region <- "Limousin"
varlist[varlist$modlibelle == "Haute-Vienne", ]$region <- "Limousin"

# Create Nord-Pas-De-Calais
varlist[varlist$modlibelle == "Nord", ]$region <- "Nord-Pas-de-Calais"
varlist[varlist$modlibelle == "Pas-de-Calais", ]$region <- "Nord-Pas-de-Calais"

# Create Picardie
varlist[varlist$modlibelle == "Aisne", ]$region <- "Picardi"
varlist[varlist$modlibelle == "Oise", ]$region <- "Picardi"
varlist[varlist$modlibelle == "Somme", ]$region <- "Picardi"

# Create Provence-Alpes-Cote d'Azur
varlist[varlist$modlibelle == "Alpes-de-Haute-Provence", ]$region <- "Provence-Alpes-Côte d'Azur"
varlist[varlist$modlibelle == "Hautes-Alpes", ]$region <- "Provence-Alpes-Côte d'Azur"
varlist[varlist$modlibelle == "Alpes-Maritimes", ]$region <- "Provence-Alpes-Côte d'Azur"
varlist[varlist$modlibelle == "Bouches-du-Rh“ne", ]$region <- "Provence-Alpes-Côte d'Azur"
varlist[varlist$modlibelle == "Var", ]$region <- "Provence-Alpes-Côte d'Azur"
varlist[varlist$modlibelle == "Vaucluse", ]$region <- "Provence-Alpes-Côte d'Azur"

# Create Bretagne
varlist[varlist$modlibelle == "C“tes-d'Armor", ]$region <- "Bretagne"
varlist[varlist$modlibelle == "FinistŠre", ]$region <- "Bretagne"
varlist[varlist$modlibelle == "Ille-et-Vilaine", ]$region <- "Bretagne"
varlist[varlist$modlibelle == "Morbihan", ]$region <- "Bretagne"

# Create Centre-Val de Loirse
varlist[varlist$modlibelle == "Cher", ]$region <- "Centre-Val de Loire"
varlist[varlist$modlibelle == "Eure-et-Loir", ]$region <- "Centre-Val de Loire"
varlist[varlist$modlibelle == "Indre", ]$region <- "Centre-Val de Loire"
varlist[varlist$modlibelle == "Indre-et-Loire", ]$region <- "Centre-Val de Loire"
varlist[varlist$modlibelle == "Loir-et-Cher", ]$region <- "Centre-Val de Loire"
varlist[varlist$modlibelle == "Loiret", ]$region <- "Centre-Val de Loire"

# Create Pays de la Loire
varlist[varlist$modlibelle == "Loire-Atlantique", ]$region <- "Pays de la Loire"
varlist[varlist$modlibelle == "Maine-et-Loire", ]$region <- "Pays de la Loire"
varlist[varlist$modlibelle == "Mayenne", ]$region <- "Pays de la Loire"
varlist[varlist$modlibelle == "Sarthe", ]$region <- "Pays de la Loire"
varlist[varlist$modlibelle == "Vend‚e", ]$region <- "Pays de la Loire"

# Create Elsas
varlist[varlist$modlibelle == "Bas-Rhin", ]$region <- "Elsass"
varlist[varlist$modlibelle == "Haut-Rhin", ]$region <- "Elsass"

# Create Champagne-Ardennes
varlist[varlist$modlibelle == "Ardennes", ]$region <- "Champagne-Ardennes"
varlist[varlist$modlibelle == "Aube", ]$region <- "Champagne-Ardennes"
varlist[varlist$modlibelle == "Marne", ]$region <- "Champagne-Ardennes"
varlist[varlist$modlibelle == "Haute-Marne", ]$region <- "Champagne-Ardennes"

# Crate Lothringen
varlist[varlist$modlibelle == "Meurthe-et-Moselle", ]$region <- "Lothringen"
varlist[varlist$modlibelle == "Meuse", ]$region <- "Lothringen"
varlist[varlist$modlibelle == "Moselle", ]$region <- "Lothringen"
varlist[varlist$modlibelle == "Vosges", ]$region <- "Lothringen"

# Create Haute-Normandie
varlist[varlist$modlibelle == "Eure", ]$region <- "Haute-Normandie"
varlist[varlist$modlibelle == "Seine-Maritime", ]$region <- "Haute-Normandie"

# Create Basse-Normandie
varlist[varlist$modlibelle == "Calvados", ]$region <- "Basse-Normandie"
varlist[varlist$modlibelle == "Manche", ]$region <- "Basse-Normandie"
varlist[varlist$modlibelle == "Orne", ]$region <- "Basse-Normandie"

# Create Burgund
varlist[varlist$modlibelle == "C“te-d'Or", ]$region <- "Burgund"
varlist[varlist$modlibelle == "NiŠvre", ]$region <- "Burgund"
varlist[varlist$modlibelle == "Sa“ne-et-Loire", ]$region <- "Burgund"
varlist[varlist$modlibelle == "Yonne", ]$region <- "Burgund"

# Create France-Comté
varlist[varlist$modlibelle == "Doubs", ]$region <- "France-Comté"
varlist[varlist$modlibelle == "Jura", ]$region <- "France-Comté"
varlist[varlist$modlibelle == "Haute-Sa“ne", ]$region <- "France-Comté"
varlist[varlist$modlibelle == "Territoire de Belfort", ]$region <- "France-Comté"

# Create Languedoc-Roussilon
varlist[varlist$modlibelle == "Aude", ]$region <- "Languedoc-Roussillon"
varlist[varlist$modlibelle == "Gard", ]$region <- "Languedoc-Roussillon"
varlist[varlist$modlibelle == "H‚rault", ]$region <- "Languedoc-Roussillon"
varlist[varlist$modlibelle == "LozŠre", ]$region <- "Languedoc-Roussillon"
varlist[varlist$modlibelle == "Pyr‚n‚es-Orientales", ]$region <- "Languedoc-Roussillon"

# Create Midi-Pyrénées
varlist[varlist$modlibelle == "AriŠge", ]$region <- "Midi-Pyrénées"
varlist[varlist$modlibelle == "Aveyron", ]$region <- "Midi-Pyrénées"
varlist[varlist$modlibelle == "Haute-Garonne", ]$region <- "Midi-Pyrénées"
varlist[varlist$modlibelle == "Gers", ]$region <- "Midi-Pyrénées"
varlist[varlist$modlibelle == "Lot", ]$region <- "Midi-Pyrénées"
varlist[varlist$modlibelle == "Hautes-Pyr‚n‚es", ]$region <- "Midi-Pyrénées"
varlist[varlist$modlibelle == "Tarn", ]$region <- "Midi-Pyrénées"
varlist[varlist$modlibelle == "Tarn-et-Garonne", ]$region <- "Midi-Pyrénées"

# Create Korsika
varlist[varlist$modlibelle == "Corse-du-Sud", ]$region <- "Corse"
varlist[varlist$modlibelle == "Haute-Corse", ]$region <- "Corse"

# Extra territoriale regions
varlist[varlist$modlibelle == "Guadeloupe", ]$region <- "Guadeloupe"
varlist[varlist$modlibelle == "La R‚union", ]$region <- "La Réunion"
varlist[varlist$modlibelle == "Guyane", ]$region <- "Guyane"
varlist[varlist$modlibelle == "Mayotte (DOM depuis avril 2011)", ]$region <- "Mayotte"
varlist[varlist$modlibelle == "DOM sans pr‚cision", ]$region <- "DOM"
varlist[varlist$modlibelle == "Martinique", ]$region <- "Martinique"
varlist[varlist$modlibelle == "Saint-Pierre-et-Miquelon", ]$region <- "Martinique"
varlist[varlist$modlibelle == "Saint-Barth‚lemy", ]$region <- "Guadeloupe"
varlist[varlist$modlibelle == "Saint-Martin", ]$region <- "Guadeloupe"
varlist[varlist$modlibelle == "Terres australes et antarctiques fran‡aises", ]$region <- NA
varlist[varlist$modlibelle == "Wallis et Futuna", ]$region <- NA
varlist[varlist$modlibelle == "Polyn‚sie fran‡aise", ]$region <- NA
varlist[varlist$modlibelle == "Nouvelle-Cal‚donie", ]$region <- NA
varlist[varlist$modlibelle == "Etranger", ]$region <- NA
varlist[varlist$modlibelle == "COM sans pr‚cision", ]$region <- NA

# Aggregate the regions
varlist$region_new <- NA

varlist[varlist$region %in% c("Auvergne", "Rhône-Alpes"), ]$region_new <- "Auvergne-Rhône-Alpes" 
varlist[varlist$region %in% c("Burgund", "France-Comté"), ]$region_new <- "Bourgogne-Franche-Comté" 
varlist[varlist$region %in% c("Bretagne"), ]$region_new <- "Bretagne" 
varlist[varlist$region %in% c("Centre-Val de Loire"), ]$region_new <- "Centre-Val-de-Loire" 
varlist[varlist$region %in% c("Corse"), ]$region_new <- "Corse" 
varlist[varlist$region %in% c("Elsass", "Champagne-Ardennes", "Lothringen"), ]$region_new <- "Grand Est" 
varlist[varlist$region %in% c("Nord-Pas-de-Calais", "Picardie"), ]$region_new <- "Hauts-de-France" 
varlist[varlist$region %in% c("Ile-de-France"), ]$region_new <- "Île-de-France" 
varlist[varlist$region %in% c("Languedoc-Roussillon", "Midi-Pyrénées"), ]$region_new <- "Occitanie" 
varlist[varlist$region %in% c("Pays de la Loire"), ]$region_new <- "Pays de la Loire" 
varlist[varlist$region %in% c("Aquitaine", "Poitou-Charentes", "Limousin"), ]$region_new <- "Nouvelle-Aquitaine" 
varlist[varlist$region %in% c("Haute-Normandie", "Basse-Normandie"), ]$region_new <- "Normandie" 
varlist[varlist$region %in% c("Provence-Alpes-Côte d'Azur"), ]$region_new <- "Provence-Alpes-Côte d'Azur" 
varlist[varlist$region %in% c("Guadeloupe"), ]$region_new <- "Guadeloupe" 
varlist[varlist$region %in% c("Martinique"), ]$region_new <- "Martinique" 
varlist[varlist$region %in% c("Guyane"), ]$region_new <- "Guyane" 
varlist[varlist$region %in% c("DOM"), ]$region_new <- "DOM" 
varlist[varlist$region %in% c("La Réunion"), ]$region_new <- "La Réunion"
varlist[varlist$region %in% c("Mayotte"), ]$region_new <- "Mayotte" 

### Clean the data ----------------------------------

# Merge with the birth data
births <- left_join(births, varlist, by = c("department" = "modalite"))

# Make variables numeric
births <- births |> mutate(across(c(age_mother, age_father, year), ~ as.numeric(as.character(.x))))

# Make na if age is 45 for fathers mothers
births$age_father[births$age_father == 46] <- NA


### Aggregate the data ------------------------------

# Make it to data.table
births <- as.data.table(births)

# Aggregate the data
births <- births[, .(births = .N), by = .(age_mother, age_father, year, region_new)]

### Impute missing values -------------------------------------------

# Create data without missing values
b_nm <- births[!is.na(age_father), ]
b_m  <- births[is.na(age_father), ]

# How many missing values
print(paste("The share of missing values is", round(sum(b_m$births) / (sum(b_m$births) + sum(b_nm$births) ), 2)))

# Estimate the distribution for missing values
b_nm[, total := sum(births), by = .(year, age_mother)]
dist <- b_nm[, .(share = sum(births) / unique(total)), by = .(year, age_mother, age_father)]

# Plot the distribution
ggplot(dist, aes(x = age_father, y = age_mother, fill = share) ) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ year)

# Remove the column for age of father in b_m
b_m[, age_father := NULL]

# Combine the distribution with the missings
b_m <- merge(b_m, dist, on = c("age_mother", "year"), all.y = TRUE, allow.cartesian = TRUE)

# Estimate the new number of births
b_m[, births := births * share]

# Merge with the non-missing data
births <- merge(b_nm, b_m, by = c("age_mother", "age_father", "year", "region_new"), suffixes = c("", "_missing"), all.x = TRUE, all.y = TRUE)

# Combine the birth counts
births[, births := ifelse(!is.na(births_missing), births + births_missing, births)]

# Remove helper columns
births[, `:=` (total = NULL, share = NULL, births_missing = NULL)]

### Load the population data ---------------------------------

# Data was donwloaded from :
# https://www.insee.fr/en/outil-interactif/6799284/pyramide.htm?#!v=2&l=en&t=2&c=52

# Adapt the meta data
regions <- c("Auvergne-Rhône-Alpes", "Bourgogne-Franche-Comté", "Bretagne",
             "Centre-Val-de-Loire", "Centre-Val-de-Loire", "Corse", "Grand Est",
             "Guadeloupe", "Hauts-de-France", "Île-de-France", "La Réunion",
             "Martinique", "Mayotte", "Normandie", "Nouvelle-Aquitaine", "Occitanie",
             "Pays de la Loire", "Bretagne", "Provence-Alpes-Côte d'Azur")

# Create meta data
regions <- data.frame(regions, insee_code = NA)

# Set the path
regions[regions$regions == "Auvergne-Rhône-Alpes", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/84/donnees_pyramide_act.csv"
regions[regions$regions == "Bourgogne-Franche-Comté", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/27/donnees_pyramide_act.csv"
regions[regions$regions == "Bretagne", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/53/donnees_pyramide_act.csv"
regions[regions$regions == "Centre-Val-de-Loire", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/24/donnees_pyramide_act.csv"
regions[regions$regions == "Corse", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/94/donnees_pyramide_act.csv"
regions[regions$regions == "Grand Est", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/44/donnees_pyramide_act.csv"
regions[regions$regions == "Guadeloupe", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/971/donnees_pyramide_act.csv"
regions[regions$regions == "Hauts-de-France", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/32/donnees_pyramide_act.csv"
regions[regions$regions == "Île-de-France", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/11/donnees_pyramide_act.csv"
regions[regions$regions == "La Réunion", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/974/donnees_pyramide_act.csv"
regions[regions$regions == "Martinique", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/972/donnees_pyramide_act.csv"
regions[regions$regions == "Mayotte", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/976/donnees_pyramide_act.csv"
regions[regions$regions == "Normandie", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/28/donnees_pyramide_act.csv"
regions[regions$regions == "Nouvelle-Aquitaine", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/75/donnees_pyramide_act.csv"
regions[regions$regions == "Occitanie", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/76/donnees_pyramide_act.csv"
regions[regions$regions == "Pays de la Loire", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/52/donnees_pyramide_act.csv"
regions[regions$regions == "Provence-Alpes-Côte d'Azur", ]$insee_code <- "https://www.insee.fr/en/outil-interactif/6799284/data/Reg/93/donnees_pyramide_act.csv"

# Create a container
pop <- vector("list", length = nrow(regions))

# For different regions
for (i in 1:nrow(regions)) {
  
  cat("Working on:", regions$regions[i], "\n")
  
  # Set the path
  data <- fread(regions$insee_code[i], sep = ";")
  
  # Create new names
  setnames(data, c("ANNEE", "AGE", "SEXE", "POP"),  c("year", "age", "sex", "pop"))
  
  # Attach the region name
  data[, region := regions$regions[i]]
  
  # Filter the years below 2018
  data <- data[year <= 2018, ]
  
  # Assign the result
  pop[[i]] <- data
}

# Combine the data
pop <- rbindlist(pop)

# Remove duplicates
pop <- unique(pop, by = c("year", "age", "region", "sex"))

### Estimate exposures ------------------------------------

# sort the data
pop <- pop[order(sex, region, age, year), ]

# Estimate the exposures
pop <- pop[, .(year, exposure = (pop + shift(pop, n = 1, type = "lead"))/2), by = .(age, sex, region)]

### Estimate rates for men ------------------------------------------

# Filter both datasets for men
b_m <- births[, .(births = sum(births, na.rm = T)), by = .(age_father, year, region_new)]

# Filter men exposures
e_m <- pop[sex == "M", ]

# Combine the data
b_m <- merge(b_m, e_m, by.x = c("year", "age_father", "region_new"), by.y = c("year", "age", "region"), all.x = TRUE)

# Filter the data
b_m <- b_m[!is.na(births) & !is.na(exposure), ]

# Rename columns
setnames(b_m, c("age_father", "region_new"), c("age", "region"))

# Estimate age_specific fertility rates
asfr_men <- b_m[, asfr_male := births / exposure]

# Estimate the tfr
tfr_men <- b_m[, .(tfr_male = sum(asfr_male)), by = .(year, region)]

### Estimate the rates for women --------------------------------

# Filter both datasets for men
b_f <- births[, .(births = sum(births, na.rm = T)), by = .(age_mother, year, region_new)]

# Filter men exposures
e_f <- pop[sex == "F", ]

# Combine the data
b_f <- merge(b_f, e_f, by.x = c("year", "age_mother", "region_new"), by.y = c("year", "age", "region"), all.x = TRUE)

# Filter the data
b_f <- b_f[!is.na(births) & !is.na(exposure), ]

# Rename age column
setnames(b_f, c("age_mother", "region_new"), c("age", "region"))

# Estimate age_specific fertility rates
asfr_female <- b_f[, asfr_female := births / exposure]

# Estimate the tfr
tfr_female <- b_f[, .(tfr_female = sum(asfr_female)), by = .(year, region)]

# Combine the data sets ----------------------------------

# Combine the data
asfr_fra <- merge(asfr_female, asfr_men, by = c("year", "age", "region"), suffixes = c("_female", "_male"), all = TRUE)
tfr_fra <- merge(tfr_female, tfr_men, by = c("year", "region"), suffixes = c("_female", "_male"), all = TRUE)

# Filter the data
asfr_fra[, total := .N, by = year]
asfr_fra <- asfr_fra[total <= mean(asfr_fra$total), ]
tfr_fra[, total := .N, by = year]
tfr_fra <- tfr_fra[total <= mean(tfr_fra$total), ]

# Remove sex columns
asfr_fra <- asfr_fra[, names(asfr_fra)[!str_detect(names(asfr_fra), "sex")]]

# Save the data
save(asfr_fra, file = "data/asfr_fra.Rda")
save(tfr_fra, file = "data/tfr_fra.Rda")

### Basic plotting ----------------------------------------

# Plot
ggplot(asfr_fra, aes(x = age, y = asfr_female, colour = year, group = year)) +
  geom_line() +
  facet_wrap(~ region) +
  ylab("ASFR (female)") +
  xlab("Age (Mother)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.25))


# Plot the trend
ggplot(tfr_fra, aes(x = year)) +
  geom_line(aes(y = tfr_male, colour = "Male")) +
  geom_line(aes(y = tfr_female, colour = "Female")) +
  facet_wrap(~ region) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
  scale_colour_manual(name = "TFR", values = c("firebrick", "darkblue"))

# Plot the fertility data
ggplot(tfr_men, aes(x = year, y = tfr_male, group = region_new, colour = region_new)) +
  geom_line()

# Plot the age-specific fertility rates for men
ggplot(asfr_men, aes(x = age_father, y = asfr_male, colour = year, group = year)) +
  geom_line() +
  facet_wrap(~ region_new) +
  ylab("ASFR (men)") +
  xlab("Age (Father)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.25))

# Plot the fertility data 
ggplot(tfr_female, aes(x = year, y = tfr_female, group = region_new, colour = region_new)) +
  geom_line()

# Plot the age-specific fertility rates for men
ggplot(asfr_fra, aes(x = age, alpha = year, group = year)) +
  geom_line(aes(y = asfr_female, colour = "Female")) +
  geom_line(aes(y = asfr_male, colour = "Male")) +
  facet_wrap(~ region_new) +
  ylab("ASFR") +
  xlab("Age") +
  guides(alpha = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.25)) +
  scale_colour_manual(name = "TFR: ", values = c("firebrick", "navyblue")) +
  theme(strip.background = element_blank()) 
  
# Save the plot
ggsave(last_plot(), filename = "Figures/fra_asfr_reg.pdf", height = 25, width = 30, unit = "cm")

### END #####################################################