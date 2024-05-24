### Subnational birth sqeezes


# edited on: 10.05.2023


## Link for the population counts
## https://seer.cancer.gov/popdata/yr1969_2020.singleages/us.1969_2020.singleages.exe
## https://seer.cancer.gov/popdata/popdic.html



### Preparations -------------------------------------------------------------



# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")


###   Load the data ---------------------------------------------------


# Set the path
path <- "https://seer.cancer.gov/popdata/yr1969_2020.singleages/us.1969_2020.singleages.adjusted.txt.gz"

# Temporary file direction
temp <- tempfile()

# Download the file
download.file(path, temp, quite = T)

# Load the data
pop <- read.table(gzfile(temp))

# Split the data
pop <- pop %>% mutate(Year = str_sub(V1, start = 1, end = 4),
                      State = as.factor(str_sub(V1, 5, 6)),
                      fips = as.numeric(str_sub(V1, 7, 8)),
                      Year = as.numeric(str_sub(V1, 9, 11)),
                      Geo = as.numeric(str_sub(V1, 12, 13)),
                      Race = as.numeric(str_sub(V1, 14, 14)),
                      Origin = as.numeric(str_sub(V1, 15, 15)),
                      Sex = as.numeric(str_sub(V1, 16, 16)),
                      Age = as.numeric(str_sub(V1, 17, 18)),
                      Pop = as.numeric(str_sub(V1, 19, 27)))


###   Load the births data -------------------------------------------
