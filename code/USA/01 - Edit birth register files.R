#################################################################################
#                        Revisiting the J-Shape                                 #
#                     Edit the Birth Register Files                             #
#################################################################################


### Edit birth register files: Consistent names etc. ##########################

## Last edited: 16. 01. 2023

## Data downloaded from:
## https://www.nber.org/data/vital-statistics-natality-data.html

## Notes:
## Depending on the year the data files look differently
## Because of this they are edited in several chunks of
## code


### Packages & settings #######################################################

rm(list = ls())

# Set the working directory
path_births <- "U:/data/usa/birth_registers/"

# load packages
library(data.table)
library(tidyverse)

source("Functions/Functions.R")

### Loading the data ####################################################
# Years to cover
years <- 1969:2021

# Group of years 1: 1969-1977
years_1    <- 1969:1977
oldnames_1 <- c("dmage","dfage","dlivord","dlegit", "stateres") # statenat
newnames_1 <- c("age_of_mother","age_of_father", "birth_order",
              "marital_status","state")
keep_1     <- c(newnames_1,"year","count")

# Group of years 2: 1978-2002
years_2    <- 1978:2002
oldnames_2 <- c("dmage","dfage","dlivord","dmar","stateres", "recwt" ) # statenat
newnames_2 <- c("age_of_mother","age_of_father", "birth_order",
                "marital_status","state", "count")
keep_2     <- c(newnames_2,"year")

# Group of years 3: 2003-2004
years_3    <- 2003:2004
oldnames_3 <- c("mager","ufagecomb","lbo_rec","mar","mrstate","recwt") # ostate
newnames_3 <- newnames_2
keep_3     <- c(newnames_3,"year")


# Aggregation formula
agg_formula <- as.formula("count~age_of_mother+age_of_father+state+birth_order+year+marital_status")



### First group of years ######################################################

for(year in years_1) {

  # Load data
  file <- paste0("natl", year, ".csv")
  dat  <- fread(file = paste0(path_births, file))
  
  # Generate variables
  dat$year  <- year
  
  # Handle DC
   tochange <- dat$statenat==9 & dat$stateres!=9
   tochange[is.na(tochange)] <- FALSE
   dat$statenat[tochange]  <- dat$stateres[tochange]

  # Rename variables
  setnames(dat,
           old=oldnames_1,
           new=newnames_1)
  
  # Weight variable
  if(year < 1972) {
    dat$count <- 2} else {
    setnames(dat,old="recwt",new="count")
    }
  
  # Subset
  dat <- subset(dat,
                subset=restatus != 4,
                select=keep_1)
  
  # Aggregate
  dat <- aggregate(agg_formula,FUN=sum,data=dat)
  
  # Save
  file <- paste0("US_states_",year,".Rda")
  save(dat, file=paste0("Data/", file))

}



  
### Second group of years ------------------------------------------------

for(year in years_2) {
  
  # Load data
  file <- paste0("natl",year,".csv")
  dat  <- fread(file = paste0(path_births, file))
  
  # Generate variables
  dat$year  <- year
  
  # Handle DC
  # tochange <- dat$statenat==9 & dat$stateres!=9
  # dat$statenat[tochange]  <- dat$stateres[tochange]
  
  # Rename variables
  setnames(dat,
           old=oldnames_2,
           new=newnames_2)

  # Subset
  dat <- subset(dat,
                subset=restatus != 4,
                select=keep_2)
  
  # Aggregate
  dat <- aggregate(agg_formula,FUN=sum,data=dat)
  
  # Save
  file <- paste0("US_states_",year,".Rda")
  save(dat,file= paste0("Data/", file))
  
}  


### Third group of years -----------------------------------------


for(year in years_3) {
  
  # Load data
  file <- paste0("natl",year,".csv")
  dat  <- fread(file = paste0(path_births, file))
  
  # Generate variables
  dat$year  <- year
  if(year==2003) dat$mager <- dat$mager41+13
  
  # Handle DC (different code as earlier years)
  # tochange <- dat$ostate=="DC" & dat$mrstate!="DC"
  # dat$ostate[tochange]  <- dat$mrstate[tochange]
  
  # Rename variables
  setnames(dat,
           old = oldnames_3,
           new = newnames_3)
  
  # Subset
  dat <- subset(dat,
                subset=restatus != 4,
                select=keep_3)
  
  # Aggregate
  dat <- aggregate(agg_formula,FUN=sum,data=dat)
  
  # Recode states
  dat$state <- State_abbr_to_nr(dat$state)
              
  # Save
  file <- paste0("US_states_",year,".Rda")
  save(dat,file= paste0("Data/", file))
  
} 

####    END    #############################################################  