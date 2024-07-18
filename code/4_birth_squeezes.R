########################################
# Purpose: Regression modelling        #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

# Load the packages
library(brms)
library(tidyverse)
library(readxl)
library(HMDHFDplus)
library(patchwork)

# Load the graphics
source("functions/Graphics.R")
source("functions/stable_population.R")

# Load the fertility data
load("data/fert_data_subnational.Rda")

# Remove observations without male or female TFR
fert <- fert[!is.na(fert$tfr_female) & !is.na(fert$tfr_male), ]

### Functions ------------------------------------------

# Funciton to estimate the mean age at childbearing (MAC), for father (MAF)
estimate_mac <- function(age, fx) {
  sum(age * fx) / sum(fx)
}

# Function to estimate the generation length
# Estimation follows Keyfitz/Caswell 2005: Applied mathematical demography, p. 265
estimate_generation_length <- function(age, px, fx) {
  sum(age * px * fx) / sum(px * fx)
}

# Function to load life tables
load_life_table <- function(path) {
  tmp <- list.files(path, full.names = T)
  tmp <- lapply(tmp, read.table, skip = 2, header = T)
  tmp <- lapply(tmp, function(x) mutate(x, across(everything(), as.numeric)))
  names(tmp) <- str_extract(list.files(path), "^[A-Z]+(_[A-Z]+)?")
  tmp <- bind_rows(tmp, .id = "country")
  return(tmp)
}

# Write the Schoumaker approximation
tfr_stable_pop <- function(SRB, p_mac, p_maf, r, T_m, T_f) {
  1 / SRB * (p_mac / p_maf) * exp(r*(T_m - T_f)) 
}

# Function to harmonize the SRB and HMD country names
harmonize_country_srb <- function(country_names) {
  country_names[country_names == "United States of America"] <- "U.S.A"
  country_names[country_names == "United Kingdom"] <- "England and Wales"
  return(country_names)
}

# Plot the impact of one determinant on tfr approximation
plot_tfr_ratio <- function(det, label, data = stable_pop) {
  p <- ggplot(data, aes_string(x=det, y="tfr_ratio_stable_pop", colour="Country")) + 
    geom_hline(yintercept = 1) +
    geom_point() +
    scale_y_continuous("TFR ratio", expand = c(0, 0)) +
    scale_colour_viridis_d(name = "")
  if (is.numeric(data[[det]])){
    p <- p +scale_x_continuous(label, expand = c(0, 0))
  } else {
    p <- p + scale_x_discrete(label) +
      coord_flip()
  }
  return(print(p))
}

### Load the Schoumaker and Dudel data -----------------

# Load the schoumaker data
# Data accessed from: https://perso.uclouvain.be/bruno.schoumaker/data/
path_global <- "U:/data/global/"
sd <- read_xlsx(paste0(path_global, "schoumaker_male/A. Estimates of male and female fertility.xlsx"), sheet = 1)
sd <- janitor::clean_names(sd)
sd <- sd[, c("country_name", "female_tfr_shown_on_figures", "male_tfr_shown_on_figures")]
names(sd) <- c("country", "tfr_female", "tfr_male")
sd$data <- "Schoumaker's fertility estimates"
sd[, c("tfr_male", "tfr_female")] <- apply(sd[, c("tfr_male", "tfr_female")], 2, as.numeric)
sd <- sd[!is.na(sd$tfr_female) & !is.na(sd$tfr_male), ]

# Load the male-fertility database
# Data accessed from: https://www.fertilitydata.org/Home/Index
hfc_files <- list.files(paste0(path_global, "male_fertility_database"), full.names = T)
hfc <- lapply(hfc_files, read.delim, sep = ",")
hfc <- bind_rows(hfc)
hfc$CNTRY <- gsub(" +", "", hfc$Country)
asfr_m <- hfc
maf <- hfc |> group_by(CNTRY, Year1) |> summarise(maf = estimate_mac(Age, ASFR), .groups = "drop")
hfc <- aggregate(ASFR ~ CNTRY + Year1, data = hfc, FUN = sum)
names(hfc) <- c("CNTRY", "Year", "tfr_male") 

# Load the female fertility data from the human fertility database
hfd <- read_xlsx(paste0(path_global, "/human_fertility_database/TFR.xlsx"), sheet = 2, skip = 2)
hfd <- pivot_longer(hfd, cols = !PERIOD, names_to = "CNTRY", values_to = "tfr_female")

# Get the meta information on countries and country codes in HFD
meta_hfd <- HMDHFDplus::getHFDcountries()
meta_hfd <- meta_hfd[, c("Country", "CNTRY")]

# Combine the data
hfc <- full_join(hfc, hfd, by = c("Year" = "PERIOD", "CNTRY"))
names(hfc) <- c("CNTRY", "year", "tfr_male", "tfr_female")
hfc <- inner_join(hfc, meta_hfd, by = "CNTRY")
names(hfc) <- tolower(names(hfc))
hfc$data <- "Human Fertility Collection"
hfc <- hfc[!is.na(hfc$tfr_female) & !is.na(hfc$tfr_male), ]

### Load the Schoen, 1985 data
rsd <- read.csv("data/schoen_birthsqueeze_1985.csv")
rsd$country <- gsub("^[0-I]+. ", "", rsd$country)
rsd$tfr_ratio <- rsd$tfr_male / rsd$tfr_female
rsd$data <- "Robert Schoen, 1985" 

# Combine the Schoumaker's and Christian's data
ccd <- bind_rows(hfc, sd, rsd)

# Estimate the outcome: TFR ratio
ccd$tfr_ratio <- ccd$tfr_male / ccd$tfr_female

### Assess normal births ------------------------------

# Load the mean age of childbearing
mac <- read.table("U:/data/global/human_fertility_database/mabRR.txt", skip = 2, header = T)

# Clean the sex ratio at births
srb <- read_xlsx("U:/data/global/sex_ratios/pnas.1812593116.sd02.xlsx", range="A4:F43252")
srb <- srb[srb$Quantile == "Median", c("Country.Name", "Reference.Year", "Model.Estimate")]
names(srb) <- c("country", "year", "srb")
srb$country <- harmonize_country_srb(srb$country)

# Merge mean age of fatherhood
timing <- inner_join(mac, maf, by = c("Code"="CNTRY", "Year"="Year1"))
timing <- inner_join(timing, meta_hfd, by = c("Code"="CNTRY"))
timing <- timing |> select(-MAB40) |> rename(mac = MAB)

# Load the population growth rates log(p1 / p0)
growth_rates <- read.csv(list.files("U:/data/global/world_population_prospects", full.names=T))
growth_rates <- growth_rates[, c("Time", "Location", "Value")]
names(growth_rates) <- c("year", "country", "r")
growth_rates$country <- harmonize_country_srb(growth_rates$country)

### Estimate the generation length --------------------------------

# For Men
lt_m <- load_life_table(path = "U:/data/global/human_mortality_database/lt_male/mltper_1x1")
t_m <- left_join(lt_m, asfr_m, by = c("country"="CNTRY", "Year"="Year1","Age"="Age"))
t_m <- t_m |> group_by(country, Year) |> mutate(na = sum(!is.na(ASFR))) |> filter(na > 0)


# Estimate the stable population growth rate
leslies <- t_f |> group_by(country, Year) |> mutate(na = sum(!is.na(ASFR))) |> filter(na >0) |> select(-na)
leslies <- split(leslies, list(leslies$country, leslies$Year), drop = T)
leslies <- lapply(leslies, create_leslie)
growth_rates_f <- map_dbl(leslies, stable_pop_growth)
stable_age_population <- map(leslies, stable_pop_age)

# Estimate the generation length
t_m <- t_m |> filter(Age < 55) |> group_by(country) |> mutate(px = cumprod(1-qx))
t_m$ASFR[is.na(t_m$ASFR)] <- 0
t_m <- t_m |> 
  group_by(Year, country) |>
  summarise(t_m = estimate_generation_length(age=Age, px=px, fx=ASFR), .groups = "drop")

# For women
lt_f <- load_life_table(path = "U:/data/global/human_mortality_database/lt_female/fltper_1x1")
asfr_f <- read.table("U:/data/global/human_fertility_database/asfrRR.txt", skip = 2, header=T)
asfr_f$Age <- as.numeric(asfr_f$Age)
t_f <- left_join(lt_f, asfr_f, by = c("country"="Code", "Year"="Year","Age"))
t_f$ASFR[is.na(t_f$ASFR)] <- 0


# Estimate the stable population growth rate
leslies <- t_f |> group_by(country, Year) |> mutate(na = sum(!is.na(ASFR))) |> filter(na >0) |> select(-na)
leslies <- split(leslies, list(leslies$country, leslies$Year), drop = T)
leslies <- lapply(leslies, create_leslie)
growth_rates_f <- map_dbl(leslies, stable_pop_growth)
stable_age_population <- map(leslies, stable_pop_age)

# Estimate the mean gernation length
t_f <- t_f |> group_by(country, Year) |> mutate(na = sum(!is.na(ASFR))) |> filter(na > 0)
t_f <- t_f |> filter(Age < 55) |> group_by(country) |> mutate(px = cumprod(1-qx))
t_f <- t_f |>
  group_by(Year, country) |>
  summarise(t_f = estimate_generation_length(age=Age, px=px, fx=ASFR), .groups = "drop")

# Combine the generation lengths
generation_length <- left_join(t_m, t_f, by = join_by(Year, country))

### Merge timing with life tables
timing$age_mother <- round(timing$mac)
timing$age_father <- round(timing$maf)
timing <- left_join(timing, lt_f[, c("Age", "Year", "country", "lx")], by = c("Year", "age_mother" = "Age", "Code"="country"))
timing <- left_join(timing, lt_m[, c("Age", "Year", "country", "lx")],
                    by = c("Year", "age_father" = "Age", "Code"="country"),
                    suffix = c("_mother", "_father"))

# Divide by the life table radix
timing$px_mac <- timing$lx_mother / 100000
timing$px_maf <- timing$lx_father / 100000
timing <- timing[, c("Code", "Year", "Country", "maf", "mac", "px_mac", "px_maf")]


### Combine the data
stable_pop <- left_join(generation_length, timing, c("Year", "country"="Code"))
stable_pop <- left_join(stable_pop, srb, by = c("Country"="country", "Year"="year"))
stable_pop <- left_join(stable_pop, growth_rates, by = c("Year"="year", "Country"="country"))

# Drop Australia because it is not in the HFD
stable_pop <- na.omit(stable_pop)
stable_pop <- rename(stable_pop, country_code = country)

# Estimate the TFR ratio approximation
stable_pop$tfr_ratio_stable_pop <- with(stable_pop, 1/(srb*100) * px_mac/px_maf * exp(r*(t_m-t_f)))

# Plot the data
variable_labels <- c("Year", "Country", "Generation length (men)", "Generation length (women)",
                    "Mean age at fatherhood", "Mean age at childbearing", "Probability to survive to MAC",
                    "Probability to survive to MAF", "Sex ratio at birth", "Growth rate")
variable_names <- c("Year", "Country", "t_m", "t_f", "maf", "mac", "px_mac", "px_maf", "srb", "r")

plots <- map2(.x = variable_names,.y = variable_labels, .f = plot_tfr_ratio)
wrap_plots(plots, nrow = 5, ncol = 2) + plot_layout(guides = "collect")

# Check the different components
hist(1/stable_pop$srb)

# Ratio of survival probabilites
hist(with(stable_pop, px_mac / px_maf))

# Difference in the generation length
hist(with(stable_pop, exp(r * (t_m - t_f))))


## Combine the data -------------------------


# Add a data column
fert$data <- "Subnational Fertility Data"

# Combine the data
fert <- bind_rows(fert, ccd)

# Filter missing data
fert <- fert[!is.na(fert$tfr_ratio), ]

### 1. Naive approach ----------------------------------

# Set the parameters
percent_difference <- 8.5

# Execute the naive approach
naive_approach <- function(tfr_ratio, percent_difference = percent_difference) {
  factor(ifelse(tfr_ratio > 1 + 8.5 / 100 | tfr_ratio < 1 - 8.5 / 100, 1, 0),
  labels = c("no squeeze", "birth squeeze"))
}

# Apply the naive approach
fert$naive_approach <- naive_approach(fert$tfr_ratio)

### 2. Empirical approach -------------------------------

# Define the priors
priors <- c(set_prior("inv_gamma(1, 1)", class = "b"),
            set_prior("uniform(-10, 10)", class = "Intercept"),
            set_prior("inv_gamma(1, 1)", class = "sigma"))

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
  ifelse(tfr_ratio < ci_empiric["lower"] | tfr_ratio > ci_empiric["upper"], 1, 0)
}

# Apply the empiric approach
fert$empiric_approach <- NA
for (i in 1:nrow(fert)) {
  cat("Row:", i, "\n")
  fert$empiric_approach[i] <- empiric_approach(tfr_ratio=fert$tfr_ratio[i], tfr_female=fert$tfr_female[i], model=bayesian_model)
}
fert$empiric_approach <- factor(fert$empiric_approach, labels = c("birth squeeze", "no squeeze"))

# Predict
pp <- posterior_predict(bayesian_model, newdata = fert)
pp <- apply(pp, 2, quantile, probs = c(0.05, 0.95))
pp <- t(rbind(pp, fert$tfr_ratio))

# Make the same as before
fert$posterior_predict <- factor(ifelse(fert$tfr_ratio < pp[, 1] | fert$tfr_ratio > pp[, 2], 1, 0),
                                 labels = c("no squeeze", "birth squeeze"))

### 3. Stable population approach ----------------------


# Estimate the population squeeze for one region-year observation
estimate_population_squeeze <- function(data) {
  sex <- c("female", "male")
  measures <- c("births", "exposure")
  variables <- as.vector(outer(measures, sex, paste, sep="_"))
  if(!all(variables %in% names(data))) stop("Does not have the right columns!")
  if(any(duplicated(data$age_group))) stop("Not unique row observations!")
  sum(data$births_female/data$exposure_male, na.rm = T) / sum(data$births_female/data$exposure_female, na.rm = T)
}

# Estimate the fertility squeeze for one region-year observation
estimate_fertility_squeeze <- function(data) {
  sex <- c("female", "male")
  measures <- c("births", "exposure")
  variables <- as.vector(outer(measures, sex, paste, sep="_"))
  if(!all(variables %in% names(data))) stop("Does not have the right columns!")
  if(any(duplicated(data$age_group))) stop("Not unique row observations!")
  sum(data$births_male/data$exposure_female, na.rm = T) / sum(data$births_female/data$exposure_female, na.rm = T)
}

# Load files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

# Load the fertility data
files <- list.files("data", full.names = T)
files <- files[str_detect(files, "asfr")]
files <- files[!str_detect(files, "nat")]
asfrs <- lapply(files, rda2list)



### Plot the results -----------------------------------

# Plot the predictions
plot_birth_squeeze <- function(data = fert, approach = naive_approach, label = "Naive approach") {
  p <- ggplot(data = subset(data, data == "subnational fertility data"),
              aes(x = tfr_female, y = tfr_male, colour = {{approach}}, shape = {{approach}})) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(alpha = .5) +
    ggtitle(label) +
    scale_colour_viridis_d(option = "D", name = "") +
    scale_shape_discrete(name = "", solid = T) +
    scale_x_continuous("TFR female", expand = c(0, 0), limits = c(0, 8)) +
    scale_y_continuous("TFR male", expand = c(0, 0), limits = c(0, 8)) + 
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) 
    
  return(print(p))
  }

###
naive <- plot_birth_squeeze()
empiric <- plot_birth_squeeze(approach = posterior_predict, label = "Empiric approach")

# Assembel the plot
naive + empiric + 
  plot_layout(guides = "collect")
ggsave(last_plot(), filename = "figures/birth_squeezes_panel.pdf", height = 15, width = 20, unit = "cm")


naive <- fert |> 
  filter(data == "subnational fertility data") |>
  group_by(country) |> 
  mutate(total = n()) |> 
  group_by(country, naive_approach) |> 
  summarise(share = 100 * n()/unique(total), .groups = "drop") |> 
  arrange(share) |> 
  ggplot(aes(x = country, y = share, fill = naive_approach)) + 
  geom_col() +
  coord_flip() +
  scale_y_continuous("% Share", expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_viridis_d(name = "") + 
  ggtitle("Naive approach")


empiric <- fert |> 
  filter(data == "subnational fertility data") |>
  group_by(country) |> 
  mutate(total = n()) |> 
  group_by(country, posterior_predict) |> 
  summarise(share = 100 * n()/unique(total), .groups = "drop") |> 
  arrange(share) |> 
  ggplot(aes(x = country, y = share, fill = posterior_predict)) + 
  geom_col() +
  coord_flip() +
  scale_y_continuous("% Share", expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_viridis_d(name = "") + 
  ggtitle("Empiric approach")

naive + empiric + 
  plot_layout(guides = "collect")


# Plot the data
ggplot(fert, aes(x = tfr_female, y = tfr_male, colour = data, shape = data)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = .3) +
  scale_x_continuous("TFR female", limits = c(0, 10), n.breaks = 10, expand = c(0, 0)) +
  scale_y_continuous("TFR male", limits = c(0, 10), n.breaks = 10, expand = c(0, 0)) +
  theme(legend.position = c(0.65, 0.2)) +
  scale_colour_viridis_d(name = "Data source:") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)))

ggsave(last_plot(), filename = "figures/data_sources.pdf", height = 15, width = 15, unit = "cm")

### END ################################################