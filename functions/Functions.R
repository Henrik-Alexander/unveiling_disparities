## Functions

#### Create not-in function -----------------------


'%!in%' <- Negate('%in%')

# A saving functions for pdf files
figs <- function(plot_name, file_name, height = 15, width = 25, unit = "cm") ggsave(plot_name, filename = paste0("figures/", file_name, ".pdf"), height = height, width = width, unit = unit)


##  Function to impute variable based on another variable
impute_variable <- function(data = d, outcome = age_mot, predictor = age_fat){
  
  # Filter the non-missing data
  nmiss     <- data %>% filter(!is.na({{outcome}}))
  
  # Filter the missing data
  miss      <- data %>% filter(is.na({{outcome}}) & !is.na({{predictor}})) %>% 
    group_by({{predictor}}) %>% count()
  
  # Estimate the conditional distribution
  cond_dist <- nmiss %>% 
    filter(!is.na({{predictor}})) %>%
    group_by({{predictor}}) %>% 
    mutate(Total = n()) %>% 
    group_by({{predictor}}, {{outcome}}) %>% 
    summarise(prob = n() / unique(Total), .groups = "drop") %>% 
    complete({{predictor}}, {{outcome}}, fill = list(prob = 0))
  
  # Join the cond_dist and miss
  tmp <- left_join(miss, cond_dist)
  
  # Estimate the imputed births
  tmp <- tmp %>% mutate(births = round(n * prob)) %>% 
    select({{predictor}}, {{outcome}},births)
  
  return(tmp)
}

### Clean age group -------------------------------------------------

clean_age_group <- function(age_group) {
  age_group <- gsub(",", "-", age_group)
  str_extract(age_group, "[0-9]+-[0-9]+")
}



#### Impute uncoditional --------------------------------------------

impute_unconditional <- function(data = d, outcome = age_mot){
  
  if(nrow(data) > 0){
    
    # Filter the non-missing data
    cond_dist <- data %>%
      filter(!is.na({{outcome}})) %>% 
      mutate(Total = n()) %>% 
      group_by({{outcome}}) %>% 
      summarise(prob = n() / unique(Total), .groups = "drop")
    
    # Filter the missing data
    miss      <- data %>% filter(is.na({{outcome}})) %>% count() %>% pull(n)
    
    # Estimate the imputed births
    cond_dist <- cond_dist %>% mutate(births = round(miss * prob))
    
    # Return 
    return(cond_dist)
    
  }else{
    
    cat("NO data! \n")
    
  }
}


#### Population Share ----------------------------------------

pop_share <- function(population){
  share <- population / sum(population, na.rm = TRUE)
  return(share)
}

#### Estimate the difference --------------------------------

difference <- function(x, y){
  diff <- y - x
  return(diff)
}

#### Average ------------------------------------------------

averaging <- function(x, y){
  m <- (y + x) / 2
  return(m)
}


#### Genesis API -----------------------------------

# Load data from genesis
genesis_api <- function(task = "results", request) {
  
  # select the task
  tmp <- request %>% filter(names == task) 
  
  #  select the task
  task <- tmp[1, 2]
  
  # select the end of the inquire
  ending <- tmp[1, 3]
  
  # create url
  url <- paste0("https://www-genesis.destatis.de/genesisWS/rest/2020/", task, login, ending)
  
  # scrape the data from the website
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
}


#### Load the data from germany -----------------------------


# Function to call the data
load_data_DE <- function(data_nr = "12411KJ0018",start = 2015, end = 2015) {
  
  #create the url
  url <- paste0("https://www-genesis.destatis.de/genesisWS/rest/2020/data/timeseries?username=", IHRE_KENNUNG ,"&password=", IHR_PASSWORT, "&name=", data_nr,
                "&area=all&compress=false&transpose=false&contents=BEVSTD&startyear=", start, "&endyear=", end, "&timeslices=&regionalvariable=0018&regionalkey=&regionalkeycode=&classifyingvariable1=Geschlecht&classifyingkey1=weiblich&classifyingkeycode1=GESW&classifyingvariable2=&classifyingkey2=&classifyingkeycode2=&classifyingvariable3=&classifyingkey3=KREISE&classifyingkeycode3=&job=false&stand=&language=de")
  
  resp <- GET(url)
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
}


### Load tables Germany -------------------------------------

# Function to call the data
load_table_DE <- function(data_nr = "12411KJ0018",start = 2015, end = 2015) {
  
  #create the url
  url <- paste0("https://www-genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=", IHRE_KENNUNG ,"&password=", IHR_PASSWORT, "&name=", data_nr,
                "&area=all&compress=false&transpose=false&startyear=", start, "&endyear=", end, "&timeslices=&regionalvariable=0018&regionalkey=&regionalkeycode=&classifyingvariable1=GES&classifyingkey1=&classifyingkeycode1=&classifyingvariable2=&classifyingkey2=&classifyingkeycode2=&classifyingvariable3=&classifyingkey3=KREISE&classifyingkeycode3=&format=csv&job=false&stand=&language=de")
  
  resp <- GET(url)
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
}


#### Negate in function -------------------------------------

#`%!in%` <- negate(`%in%`)


#### Tabulate function --------------------------------------


tab <- function(...){
  
  tmp <- table(..., useNA = "always")
  return(tmp)
  
}


#### Load country specific functions ------------------------

source("Functions/Functions_Mexico.R")


### Recode the state names USA ------------------------------

State_nr_to_abbre <- function(var){
  
  tmp <- dplyr::recode(var,
                       `2`= "AK",
                       `1`= "AL",
                       `4`= "AR",
                       `3`= "AZ",
                       `5`= "CA",
                       `6`= "CO",
                       `7`= "CT",
                       `9`= "DC",
                       `8`= "DE",
                       `10`= "FL",
                       `11`= "GA",
                       `12`= "HI",
                       `16`= "IA",
                       `13`= "ID",
                       `14`= "IL",
                       `15`= "IN",
                       `17`= "KS",
                       `18`= "KY",
                       `19`= "LA",
                       `22`= "MA",
                       `21`= "MD",
                       `20`= "ME",
                       `23`= "MI",
                       `24`= "MN",
                       `26`= "MO",
                       `25`= "MS",
                       `27`= "MT",
                       `34`= "NC",
                       `35`= "ND",
                       `28`= "NE",
                       `30`= "NH",
                       `31`= "NJ",
                       `32`= "NM",
                       `29`= "NV",
                       `33`= "NY",
                       `36`= "OH",
                       `37`= "OK",
                       `38`= "OR",
                       `39`= "PA",
                       `40`= "RI",
                       `41`= "SC",
                       `42`= "SD",
                       `43`= "TN",
                       `44`= "TX",
                       `45`= "UT",
                       `47`= "VA",
                       `46`= "VT",
                       `48`= "WA",
                       `50`= "WI",
                       `49`= "WV",
                       `51`= "WY")
  return(tmp)
}


###


State_abbr_to_nr <- function(var){
  
  tmp <- dplyr::recode(var,
                       "AK" = 2,
                       "AL" = 1,
                       "AR" = 4,
                       "AZ" = 3,
                       "CA" = 5,
                       "CO" = 6,
                       "CT" = 7,
                       "DC" = 9,
                       "DE" = 8,
                       "FL" = 10,
                       "GA" = 11,
                       "HI" = 12,
                       "IA" = 16,
                       "ID" = 13,
                       "IL" = 14,
                       "IN" = 15,
                       "KS" = 17,
                       "KY" = 18,
                       "LA" = 19,
                       "MA" = 22,
                       "MD" = 21,
                       "ME" = 20,
                       "MI" = 23,
                       "MN" = 24,
                       "MO" = 26,
                       "MS" = 25,
                       "MT" = 27,
                       "NC" = 34,
                       "ND" = 35,
                       "NE" = 28,
                       "NH" = 30,
                       "NJ" = 31,
                       "NM" = 32,
                       "NV" = 29,
                       "NY" = 33,
                       "OH" = 36,
                       "OK" = 37,
                       "OR" = 38,
                       "PA" = 39,
                       "RI" = 40,
                       "SC" = 41,
                       "SD" = 42,
                       "TN" = 43,
                       "TX" = 44,
                       "UT" = 45,
                       "VA" = 47,
                       "VT" = 46,
                       "WA" = 48,
                       "WI" = 50,
                       "WV" = 49,
                       "WY" = 51)
  return(tmp)
}

### Transform years -----------------------------

trans_year <- function (year) {
  (year / 2021)^40
}


#####               END               ########################
