### Load metadata ----------------------------------------------

# Set the path to the wikipedia site
path_wiki <- "https://simple.wikipedia.org/wiki/Departments_of_France"

# Read the table from wikipedia
page <- read_html(path_wiki)

# Obtain the poiece of the web page that corresponds to the "wikitable"
meta <- html_node(page, ".wikitable")

# Convert the html code into a data frame
meta <- html_table(meta, fill = TRUE)

# Clean the data
meta <- clean_names(meta)

# Remove unimportant columns
meta <- subset(meta, select = c(insee_code, department, prefecture, region))

# Filter Metropolitan lyon
meta <- subset(meta, subset = department != "Metropolitan Lyon 18")

# Clean the department names
meta$department <- str_replace(meta$department, " [0-9]*$", "")


### Load data --------------------------------------------------

# Set the baseline path
path_pop <- "https://www.insee.fr/en/outil-interactif/6799284/data/Dep/"
path_end <- "/donnees_pyramide_act.csv"

# Create a container
pop <- vector("list", length = length(meta))

# For different regions
for (i in 1:nrow(meta)) {
  
  cat("Working on:", meta$department[i], "\n")
  
  # Set the path
  path <- paste0(path_pop, meta$insee_code[i], path_end)
  data <- fread(path, sep = ";")
  
  # Create new names
  setnames(data, c("ANNEE", "AGE", "SEXE", "POP"),  c("year", "age", "sex", "pop"))
  
  # Attach the region name
  data[, department := meta$department[i]]
  
  # Filter the years below 2018
  data <- data[year <= 2018, ]
  
  # Assign the result
  pop[[i]] <- data
}

# Combine the data
pop <- rbindlist(pop)

# Save the data for the departements
save(pop, file = "U:/data/fra/population/pop_age_sex_dep_2013-2070.Rda")
