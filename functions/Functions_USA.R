
## 5. Recode state numbers ----------------------------

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

#=============================================================================

## 6. Recode state level abbrevations -------------------------------

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

#===============================================================================

## 7. create census divisions ----------------------------

divisioning <- function(variable){
  
  x <- variable
  
  x[x %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")] <-  "New England"
  x[x %in% c("New Jersey", "New York", "Pennsylvania")] <- "Middle Atlantic"
  x[x %in% c("Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin")] <-   "East North Central"
  x[x %in% c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")] <- "West North Central"
  x[x %in% c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia")] <- "South Atlantic" 
  x[x %in% c("Alabama", "Kentucky", "Mississippi", "Tennessee")] <- "East South Central"
  x[x %in% c("Arkansas", "Louisiana", "Oklahoma", "Texas")] <- "West South Central"
  x[x %in% c("Arizona", "Colorado", "Idaho", "New Mexico", "Montana", "Utah", "Wyoming", "Nevada")] <- "Mountain"
  x[x %in% c("Alaska", "California", "Hawaii", "Oregon", "Washington")] <- "Pacific"
  
  census_division <- x
  
  return(x)
}


#==============================================================================

## 8. Abbreaviation to state ----------------------------
abbr2state <- function (abbr) 
{
  ab <- tolower(c("AL", "AK", "AZ", "KS", 
                  "UT", "CO", "CT", "DE", "FL", 
                  "GA", "HI", "ID", "IL", "IN", 
                  "IA", "AR", "KY", "LA", "ME", 
                  "MD", "MA", "MI", "MN", "MS", 
                  "MO", "MT", "NE", "NV", "NH", 
                  "NJ", "NM", "NY", "NC", "ND", 
                  "OH", "OK", "OR", "PA", "RI", 
                  "SC", "SD", "TN", "TX", "CA", 
                  "VT", "VA", "WA", "WV", "WI", 
                  "WY", "DC"))
  st <- c("Alabama", "Alaska", "Arizona", 
          "Kansas", "Utah", "Colorado", "Connecticut", 
          "Delaware", "Florida", "Georgia", "Hawaii", 
          "Idaho", "Illinois", "Indiana", "Iowa", 
          "Arkansas", "Kentucky", "Louisiana", 
          "Maine", "Maryland", "Massachusetts", 
          "Michigan", "Minnesota", "Mississippi", 
          "Missouri", "Montana", "Nebraska", 
          "Nevada", "New Hampshire", "New Jersey", 
          "New Mexico", "New York", "North Carolina", 
          "North Dakota", "Ohio", "Oklahoma", 
          "Oregon", "Pennsylvania", "Rhode Island", 
          "South Carolina", "South Dakota", "Tennessee", 
          "Texas", "California", "Vermont", "Virginia", 
          "Washington", "West Virginia", "Wisconsin", 
          "Wyoming", "District of Columbia")
  st[match(tolower(abbr), ab)]
}

#==============================================================================


## create state number from abbreviation ----------------------------


state_nr <- function(state_abbrev){
  
  tmp <- dplyr::recode({state_abbrev}, 
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



# Combine the results
combine_tables <- function(name = "f", state = 1, outcome = "asfr", sex = "female") {
  tmp <- get(paste0(name, "_", sex, "_", state))
  tmp <- melt(tmp)
  names(tmp) <- c("age", "year", paste0(outcome, "_", sex))
  tmp$state <- s
  return(tmp)
}
