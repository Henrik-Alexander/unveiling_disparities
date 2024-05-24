#################################################################################
#                        Revisiting the J-Shape                                 #
#                      Impute and calculate ASFRs                               #
#################################################################################

### Settings ##################################################################

## Last edited: 06.12.2021

rm(list = ls())

# load packages
library(reshape2)
library(tidyverse)
library(data.table)
library(usdata)

# load functions
source("Functions/Functions_USA.R")
source("Functions/Functions.R")

### Settings ##################################################################

# Age range for men and women
age_m <- 15:59
age_f <- 15:49

### Read data #################################################################

# Read birth data
load("Data/US_states_births.rda")

# Population counts
load("Data/US_states_pop.rda")
pop     <- subset(pop, subset = age > 14 & age <= 99)

# Find year range
years    <- 1972:2004

### Edit age variable #########################################################

# Edit Texas: In 1989 missing values coded with '89' instead of '99'
texas89 <- births$age_of_father%in%89 & births$state==44 & births$year==1989
births$age_of_father[texas89] <- "Unknown"

# Edit age variables: Restrict to age range (women)
births$age_of_mother[births$age_of_mother<min(age_f)] <- min(age_f)
births$age_of_mother[births$age_of_mother>max(age_f)] <- max(age_f)

# Edit age variables: Restrict to age range (men)
below <- paste(0:(min(age_m)-1))
above <- paste((max(age_m)+1):99)
births$age_of_father[births$age_of_father%in% below]  <- min(age_m)
births$age_of_father[births$age_of_father%in% above] <- max(age_m)

# Aggregate
agg_formula <- as.formula("count~age_of_mother+age_of_father+year+state")
births <- aggregate(agg_formula,data=births,FUN=sum)

# Edit age of father
births$age_of_father <- as.numeric(births$age_of_father)

# Remove missing variables from population data
pop <- na.omit(pop)

### Objects for results #######################################################

# Create male template objects
f_male             <- matrix(data=0,ncol=length(years),nrow=length(age_m))
rownames(f_male)   <- paste(age_m)
colnames(f_male)   <- paste(years)

# Create female template
f_female           <- matrix(data=0,ncol=length(years),nrow=length(age_f))
rownames(f_female) <- paste(age_f)
colnames(f_female) <- paste(years)

b_male             <- f_male
b_female           <- f_female

exposure_f    <- vector("list", length = length(years))
exposure_m    <- vector("list", length = length(years))
names(exposure_f) <- names(exposure_m) <- years


# List of states
states <- unique(births$state)

# Create a container for the results
result <- vector("list", length = length(states))
names(result) <- states


### Get rates #################################################################

# Cycle over states
for(s in states)  {
  
  cat("STATE: ",s,"\n")
  
  # Cycle over years
  for(i in years) {
    
    # Mid-year population count men
    pop1_m <- pop[pop$year==i&pop$age%in%age_m&pop$state==s&pop$sex==1,c("age","pop")]
    pop2_m <- pop[pop$year==i+1&pop$age%in%age_m&pop$state==s&pop$sex==1,c("age","pop")]
    pop_m  <- (pop1_m+pop2_m)/2
    
    # Mid-year population count women
    pop1_f <- pop[pop$year==i&pop$age%in%age_m&pop$state==s&pop$sex==2,c("age","pop")]
    pop2_f <- pop[pop$year==i+1&pop$age%in%age_m&pop$state==s&pop$sex==2,c("age","pop")]
    pop_f  <- (pop1_f+pop2_f)/2
    
    # Birth counts: Marginals
    birth_year   <- births[births$year==i&births$state==s,]
    births_tmp_m <- aggregate(count~age_of_father,data=birth_year,sum)
    births_tmp_f <- aggregate(count~age_of_mother,data=birth_year,sum)
    
    # Births with missing age of father
    birth_mis      <- births[births$year==i&is.na(births$age_of_father)&births$state==s,]
    births_tmp_fna <- aggregate(count~age_of_mother,data=birth_mis,sum)
    
    # Ages of mother with births with missing age of father
    missing_age <- unique(births_tmp_fna$age_of_mother)
    
    # Distribute missing values: Loop over ages of mother
    for(j in missing_age) {
      
      # Fall-back option: If no births with age of father
      # then impute age of father as age of mother plus 3
      all_missing <- birth_year[birth_year$age_of_mother==j,"age_of_father"]
      all_missing <- all(is.na(all_missing))
      
      # If all missing: Create artificial counts
      if(all_missing) {
        
        fathers <- data.frame(count=births_tmp_fna[births_tmp_fna$age_of_mother==j,"count"],age_of_father=j+3)
        
        # If not all missing: Get counts          
      } else {
        
        fathers <- aggregate(count~age_of_father,data=birth_year[birth_year$age_of_mother==j,],sum)
        
      }
      
      # Calculate distribution for imputation
      fathers$count                       <- fathers$count/sum(fathers$count)
      # Where to add missing births in current distribution
      matching_ages                       <- match(fathers$age_of_father,births_tmp_m$age_of_father)
      # How many to add in current distribution
      distr_tmp                           <- births_tmp_fna[births_tmp_fna$age_of_mother==j,"count"]*fathers$count
      # Add to current distribution
      births_tmp_m[matching_ages,"count"] <- births_tmp_m[matching_ages,"count"]+distr_tmp
      
    }
    
    # Match ages of population counts and births
    popmatch_m <- !is.na(match(pop_m$age,births_tmp_m$age_of_father))
    popmatch_f <- !is.na(match(pop_f$age,births_tmp_f$age_of_mother))
    
    # Birth counts
    b_female[paste(age_f[popmatch_f]),paste(i)] <- births_tmp_f$count
    b_male[paste(age_m[popmatch_m]),paste(i)]   <- births_tmp_m$count
    
    # ASFRs
    f_female[paste(age_f[popmatch_f]),paste(i)] <- births_tmp_f$count/pop_f[popmatch_f,"pop"]
    f_male[paste(age_m[popmatch_m]),paste(i)]   <- births_tmp_m$count/pop_m[popmatch_m,"pop"]
    
    # Exposures
    exposure_f[[paste(i)]] <- pop_f
    exposure_m[[paste(i)]] <- pop_m
    
    
  }
  
  # Assign results
  mal <- melt(f_male)
  fem <- melt(f_female)
  names(mal) <- names(fem) <- c("age", "year", "asfr")
  asfr_com <- inner_join(mal, fem, by = c("year", "age"), suffix = c("_male", "_female"))
  
  # Combine the birth results
  mal <- melt(b_male)
  fem <- melt(b_female)
  names(mal) <- names(fem) <- c("age", "year", "births")
  births_com <- inner_join(mal, fem, by = c("year", "age"), suffix = c("_male", "_female"))
  
  # Combine the exopsure
  exp <- full_join(bind_rows(exposure_f, .id = "year"),
                   bind_rows(exposure_m, .id = "year"),
                   suffix = c("_female", "_male"),
                   by = c("age", "year"))
  
  exp$year <- as.numeric(exp$year)
  
  # Combine the results
  tmp <- inner_join(asfr_com, births_com)
  tmp <- inner_join(tmp, exp)
  tmp$state <- s
  
  # Assign the results
  result[[paste(s)]] <- tmp
  
}


# Combine the results
asfr_us <- bind_rows(result)

# Create state names
asfr_us$state <- abbr2state(State_nr_to_abbre(asfr_us$state))

# Estimate the TFR
tfr_us <- asfr_us |> 
  group_by(year, state) |> 
  summarise(tfr_male = sum(asfr_male),  tfr_female = sum(asfr_female), .groups = "drop")

# Save the results
save(asfr_us, file = "data/asfr_us.Rda")
save(tfr_us, file = "data/tfr_us.Rda")

# Remove the temporary files
sapply(list.files("data", pattern = "^US_states_\\d{4}.Rda", full.names = T), file.remove)

### END ########################################################################  