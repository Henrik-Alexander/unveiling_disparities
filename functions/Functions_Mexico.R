###########################################
#  Functions to load, clean and analyze the data for Mexico
#
#
#
###########################################


#### Load data for mexico -----------------------------------

load_data_MEX <- function(year, zipfile){
  
  # Set the path
  path <- "https://en.www.inegi.org.mx/contenidos/programas/natalidad/microdatos/"
  mid <- "/natalidad_base_datos_"
  end <- "_dbf.zip"
  
  # Set the path
  pathtmp <- paste0(path, year, mid, year, end)
  
  # Set the zipfile
  ziptmp <- paste0(zipfile,year, end )
  
  # Download the data
  GET(pathtmp, write_disk(ziptmp, overwrite = FALSE), progress())
  

}

### Unzip the file -------------------------------------------

unzip_MEX <- function(year){

    # Set the path
    path <- "https://en.www.inegi.org.mx/contenidos/programas/natalidad/microdatos/"
    mid <- "/natalidad_base_datos_"
    end <- "_dbf.zip"
    
    # Set the zipfile
    ziptmp <- paste0(zipfile,year, end )
  
    # Unzip the data
    utils::unzip(zipfile = ziptmp, exdir =  paste0("Raw/Mexico/", year))
    
    # Year shortcut
    y <- str_sub(year, 3, 4)
    
    # Load the data
    tmp <- foreign::read.dbf(paste0("Raw/Mexico/",year, "/NACIM", y , ".dbf"))
    
    # Create a year column
    tmp$year <- year
    
    # Return the result
    return(tmp)

}

### Clean the names ------------------------------------------


clean_names_MEX <- function(...){ 
  
  ### Assign 
  dat <- (...)
  
  ## List of variables; number of waves in parantheses
  # Entity of registration (32), municiplaity of registration (32), 
  # age of the mother (32), age of the father (32), year of registration (32), year of occurence, parity(32), live births (32),
  # marital status (32), education of mother (32), education of father (32), activity mother (32), activity father (32)
  
  # Make lower case
  dat <- clean_names(dat)
  
  # Filter
  dat <- subset(dat, select = c(ent_regis, mun_resid, edad_madn, edad_padn, ano_reg, # ano_nac,
                                orden_part, hijos_vivo, edociv_mad, escol_mad, escol_pad, act_mad, act_pad, year))
  
  # Rename 
  dat <- rename(dat, 
                entity = ent_regis,
                mun_res = mun_resid , 
                age_mot = edad_madn,
                age_fat = edad_padn, 
                year_reg= ano_reg, 
                #year_bir = ano_nac,
                parity = orden_part, 
                live_births = hijos_vivo,
                mar_mot = edociv_mad, 
                edu_mot = escol_mad, 
                edu_fat = escol_pad, 
                act_mot = act_mad,
                act_fat = act_pad,
                year = year)
  

  
  # Return the resul
  return(dat)
}


####


clean_data_MEX <- function(...){
  
  ### Assign 
  dat <- (...)
  
  ### Change the variable format -------------------------------------
  
  # Make numeric
  dat <- mutate(dat, across(c(age_fat, age_mot, parity), as.integer))
  
  
  #### Recode age and parity variables ----------------------------------
  
  # Recode age as missing
  vars <- c("age_fat", "age_mot", "parity", "live_births", "mun_res")
  dat[, vars] <- sapply(dat[ ,vars], function(x) ifelse(x == 99, NA, x))

  
  #### Recode education, marriage and activity variables ----------------
  
  
  # Recode education and activity
  vars <- c("edu_fat", "edu_mot", "act_fat", "act_mot", "mar_mot")
  dat[, vars] <- sapply(dat[, vars], function(x) ifelse(x == 9, NA, x))
  
  
  # Recode
  dat$live_births <- ifelse(dat$live_births == 999, NA, dat$live_births)
  
  # Make factor
  dat <- mutate(dat, across(c(mar_mot, edu_mot, edu_fat, act_mot, act_fat), as.factor))
}
