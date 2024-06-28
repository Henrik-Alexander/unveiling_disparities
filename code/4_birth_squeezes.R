########################################
# Purpose: Regression modelling        #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################


# Load the packages
library(brms)
library(tidyverse)
library(xlsx)
library(HMDHFDplus)

# Load the graphics
source("functions/Graphics.R")

# Load the fertility data
load("data/fert_data_subnational.Rda")

### Load the Schoumaker and Dudel data -----------------

# Load the schoumaker data
# Data accessed from: https://perso.uclouvain.be/bruno.schoumaker/data/
path_global <- "U:/data/global/"
sd <- read.xlsx(paste0(path_global, "schoumaker_male/A. Estimates of male and female fertility.xlsx"), sheetIndex = 1)
sd <- sd[, c("Country.name", "Female.TFR..shown.on.Figures.", "Male.TFR..shown.on.Figures.")]
names(sd) <- c("country", "tfr_female", "tfr_male")
sd$data <- "Schoumaker's fertility estimates"
sd[, c("tfr_male", "tfr_female")] <- apply(sd[, c("tfr_male", "tfr_female")], 2, as.numeric)

# Load the male-fertility database
# Data accessed from: https://www.fertilitydata.org/Home/Index
hfc_files <- list.files(paste0(path_global, "male_fertility_database"), full.names = T)
hfc <- lapply(hfc_files, read.delim, sep = ",")
hfc <- bind_rows(hfc)
hfc <- aggregate(ASFR ~ Country + Year1, data = hfc, FUN = sum)
hfc$CNTRY <- gsub(" +", "", hfc$Country)
hfc <- hfc[ c("CNTRY", "Year1", "ASFR")]

# Load the female fertility data from the human fertility database
hfd <- read.xlsx(paste0(path_global, "/human_fertility_database/TFR.xlsx"), sheetIndex = 2, startRow = 3)
hfd <- pivot_longer(hfd, cols = !PERIOD, names_to = "CNTRY", values_to = "tfr_female")

# Get the meta information on countries and country codes in HFD
meta_hfd <- HMDHFDplus::getHFDcountries()
meta_hfd <- meta_hfd[, c("Country", "CNTRY")]

# Combine the data
hfc <- full_join(hfc, hfd, by = c("Year1" = "PERIOD", "CNTRY"))
names(hfc) <- c("CNTRY", "year", "tfr_male", "tfr_female")
hfc <- inner_join(hfc, meta_hfd, by = "CNTRY")
names(hfc) <- tolower(names(hfc))
hfc$data <- "human fertiltiy collection"

# Combine the Schoumaker's and Christian's data
ccd <- bind_rows(hfc, sd)

### 1. Naive approach ----------------------------------

# Set the parameters
percent_difference <- 8.5

# Execute the naive approach
naive_approach <- function(tfr_ratio, percent_difference = percent_difference) {
  ifelse(tfr_ratio > 1 + 8.5 / 100 | tfr_ratio < 1 - 8.5 / 100, 1, 0)
}



### 2. Empirical approach -------------------------------

# Plot the data
ggplot(ccd, aes(x = tfr_female, y = tfr_male, colour = data)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  scale_x_continuous("TFR female", limits = c(0.5, 10)) +
  scale_y_continuous("TFR male", limits = c(0.5, 10)) +
  theme(legend.position = c(0.6, 0.2))

# Estimate the outcome: TFR ratio
ccd$tfr_ratio <- ccd$tfr_male / ccd$tfr_female

# Filter non-missing values
ccd <- ccd[!is.na(ccd$tfr_ratio), ]

# Define the priors
priors <- c(set_prior("lognormal(0, 10)", class = "b"),
            set_prior("normal(0, 10)", class = "Intercept"),
            set_prior("lognormal(0, 10)", class = "sigma"))

# Estimate the bayesian model
bayesian_model <- brm(tfr_ratio ~ 1 + tfr_female,
    family = gaussian(),
    prior = priors, 
    data = ccd)

# Get the mean prediction
bayesian_mean_predict <- function(tfr_female = 2, model = bayesian_model){
  result <- summary(model)
  result$fixed$Estimate[1] + result$fixed$Estimate[2] * tfr_female
} 

# Get the confidence intervals
bayesian_confidence_interval <- function(tfr_female = 2, model = bayesian_model) {
  mu <- bayesian_mean_predict(tfr_female = tfr_female, model = model)
  res <- summary(model)
  sigma <- res$spec_pars[[1]]
  lower <- qnorm(p = 0.05, mean = mu, sd = sigma)
  upper <- qnorm(p = 0.95, mean = mu, sd = sigma)
  return(c("lower" = lower, "upper" = upper))
}

# Apply the empiric approach
empiric_approach <- function(tfr_ratio = 1, tfr_female = 2.1, model = bayesian_model) {
  ci_empiric <- bayesian_confidence_interval(tfr_female = tfr_female, model = model)
  ifelse(tfr_ratio < ci_empiric["lower"] | tfr_ratio > ci_empiric["upper"], 1, 0 )
}

### 3. Stable population approach ----------------------


### END ################################################

