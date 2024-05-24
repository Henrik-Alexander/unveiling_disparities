#################################################################################
#                        Revisiting the J-Shape                                 #
#                  Combine the Birth Register Files                             #
#################################################################################

### Packages & settings #######################################################

  
  # Packages
  library(data.table)

  # Years to cover
  years <- 1969:2004
  
  # Variable names
  var_names <- c("age_of_mother","age_of_father", "birth_order",
                 "marital_status","state","count","year")
  
### Combine files #############################################################
  
  # Create empty data frame
  empty_table <- data.table(matrix(nrow = 0, ncol = length(var_names)))
  births <- setNames(empty_table, var_names)
  
  # Loop over years
  for(year in years) {
    
    file <- paste0("US_states_",year,".rda")
    load(paste0("Data/", file))
    
    births <- rbind(births,dat)
    rm(dat)
    
  }
  
  # Edit age of father
  births$age_of_father <- as.character(births$age_of_father) 
  births$age_of_father[births$age_of_father=="99"] <- "Unknown"
  
  # Save
  file <- paste0("US_states_births.rda")
  save(births,file= paste0("Data/", file))
  
####    END    #############################################################  