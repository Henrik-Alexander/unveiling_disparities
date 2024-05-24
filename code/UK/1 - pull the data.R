##############################################################
#   Max-Planck Institute for Demographic Research            #
#             United - Kingdom - Birth Squeezes              #
#             Henrik-Alexander Schubert                      #
##############################################################


  # edited by: Henrik-Alexander Schubert
  # edited on: 11. May 2022

  # Population data from: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
  # Birth data from: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=207
  # Births by father and mother: https://www.nomisweb.co.uk/query/construct/summary.asp?menuopt=200&subcomp=

#####     Preperations      ################################
  

  rm(list = ls())
  
  # load the functions
  source("Functions/Functions.R")

  # load the packages
  library(tidyverse)
  library(readxl)
  library(HMDHFDplus)

  ## account information for human mortality database
  myHMDusername <- "schubert@demogr.mpg.de"
  myHMDpassword <- "1624547717"

  # years vector
  years <- 2013:2020

#####     Load the Data       ################################

  # load the birth data --------------------
  for(i in 1:length(years)){
  
    # load the data
  births_tmp <- read_excel("Rawdata/england_regional_births.xlsx", sheet = i, skip = 5)
  
  # add a year variable
  births_tmp$Year <- years[i]
  
  # write to an object
  assign(paste0("births_", years[i]), births_tmp)
  
  }
  
  
  # combine the data
  births <- mget(ls(pattern = "births_")) %>% do.call(rbind, .) %>% as.data.frame()
  
  # remove old files
  rm(list = ls(pattern = "births_"))
  
  # load the pop data ---------------------
  pop <- read.csv("Rawdata/pop_2011_2020.csv")
  
  # pivot longer
  pop <- pop %>% pivot_longer(cols = starts_with("population_"), names_prefix = "population_", names_to = "Year", values_to = "Population") %>% 
    mutate(Year = as.numeric(Year))
  
  # load the father distribution ----------
  
  # load the birth data --------------------
  for(i in 1:length(years)){
    
    # load the data
    father_tmp <- read_excel("Rawdata/father_mother_birth.xlsx", sheet = i, skip = 8)
    
    # add a year variable
    father_tmp$Year <- years[i]
    
    # write to an object
    assign(paste0("father_", years[i]), father_tmp)
    
  }
  
  # combine the data
  father <- mget(ls(pattern = "father_")) %>% do.call(rbind, .) %>% as.data.frame() %>% filter(`Age of father` != "Column Total")

  # remove old files
  rm(list = ls(pattern = "father_"))
  
  # reshape longer
  father <- father %>% pivot_longer(cols = 2:9, names_to = "Mother_Age", values_to = "Births")
  
  
  ### Create numeric age vector -----------------------------
  # create numeric age vector
  a <- father$`Age of father` %>%  str_remove_all(pattern = "[A-Z*][a-z]* [a-z]* ?[a-z]* ") %>% str_extract("[0-9]*") %>% as.numeric()
  a[a == 20] <- 15
  a[father$`Age of father` == "Father aged 20-24"] <- 20
  # attach the vector to the dataset
  father$Father <- a
  
  # same for mothers
  a <- father$Mother_Age %>%  str_remove_all(pattern = "[A-Z*][a-z]* [a-z]* ?[a-z]* ") %>% str_extract("[0-9]*") %>% as.numeric()
  a[a == 20] <- 15
  a[father$Mother_Age == "Mother aged 20-24"] <- 20
  
  # attach the vector to the dataset
  father$Mother <- a; rm(a)
  
  
  ### distribute the missing births conditional on mother's age (Dudel 2020) ---
  
  # calculate the conditional distribution
  cond_distr <- father %>% group_by(Year, Mother) %>% filter(!is.na(Father)) %>%
    mutate(Proportion = Births / sum(Births)) %>% select(Year, Father, Proportion, Mother)

  #
  father <- left_join(father, cond_distr, by = c("Year", "Father", "Mother"))
  
  # add the number of missing father's age
  father <- father %>% filter(is.na(Father)) %>% mutate(missing = Births) %>% 
    select(Year, Mother, missing) %>% inner_join(father, ., by = c("Year", "Mother"))

  
  # redistribute the number of missing births
  father <- father %>% mutate(Births_adj = round(Births + Proportion * missing, 0))
  
  # the difference between original and new birth numbers
  ggplot(father, aes(x = Mother, y = Father, fill = Births_adj - Births)) +
    geom_tile() + facet_wrap(~ Year) + guides(fill = "none" ) +       
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_fill_viridis_c(option = "H") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  

  
###  Calculate the age-sex preferences ##################################
  
  # load the deaths
  exp <- readHMDweb(item =  "Exposures_5x1",CNTRY = "GBRTENW", username = myHMDusername,
                       password = myHMDpassword)
  
  # male exposure
  father <- exp %>% select(Year, Age, Male) %>% left_join(father, ., by = c("Year", "Father" = "Age"))
  
  # female exposure
  father <- exp %>% select(Year, Age, Female) %>% left_join(father, ., by = c("Year", "Mother" = "Age"))
  
  # calculate the harmonic mean preference
  father <- father %>% mutate(Preference = Births_adj / ((Male * Female)/(Male+Female)), na.rm = T)

  # plot the preferences
  ggplot(father, aes(x = Mother, y = Father, fill = Preference)) +
    geom_tile() + facet_wrap(~ Year) + guides(fill = "none" ) +       
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_fill_viridis_c(option = "H") +
    geom_abline(intercept = 0, slope = 1, colour = "white", size = 1.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) 
  
###      END        ##########################################