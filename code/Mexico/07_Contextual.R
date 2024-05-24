### Load Context variables ##############################
# Purpose: raw birth data                               #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: cleaned birth data and functions       #
#########################################################

### Settings  ----------------------------------------------------------------

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")


# https://en.www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/
#[IdIndicador]/
#[Idioma]/
#[Área Geográfica]/
#[Recientes]/
#[Fuente de datos]/
#[Versión]
#[Token]
#?type=[Formato]

# Base path
base <- "https://en.www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR"
end <- "?type=xml"
meta <- "https://en.www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_INDICATOR"

# Indicator
IdIndicador <- 6200093975

# Idioma = Language
idioma <- "en"

# Area geografica, 99 = federal entity
entities <- 1:32
entities <- ifelse(entities <= 9, paste0("0", entities), entities)
geo <- paste0("070000", entities)

# Last data
last <- "false"

# Data source
source <- "BISE"

# Version 
version <- "2.0"

# Save the token
token <- "2a68ebe0-bb04-aa52-e76c-a307ab85dc33"

### Catalogue -----------------------------------

# Economically active population
query <- paste(base, IdIndicador, idioma, geo, last, source, version, paste0(token, end), sep = "/")

# Load the data
result <- read_html(query[[1]])

# Extract the data
body <- html_nodes(result, xpath = "//*") |> html_text()

# Get the meta data
query <- paste(meta, IdIndicador, idioma, geo, last, source, version, paste0(token, end), sep = "/")


