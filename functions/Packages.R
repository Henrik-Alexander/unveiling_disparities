## This file loads the required packages for 
## reproducing the results of the paper "Regional Birth Squeezes: A cross-country comparison"

# Do you want to install the required packages: set install <- TRUE
install <- FALSE
if(install) {
  packages <- c("tidyverse", "data.table", "usmap")
}

# Load the packages
library(tidyverse)
library(data.table)
library(dtplyr)
library(reshape2)
library(naniar)
library(survey)
library(haven)
library(scales)
library(httr)
library(rvest)
library(jsonlite)
library(readxl)
library(pxweb)
library(httr)
library(janitor)
library(usmap)

# web screping


