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
library(stargazer)

# Load the graphics
source("functions/Graphics.R")

# Load the fertility data
load("data/fert_data_subnational.Rda")

# Remove observations without male or female TFR
fert <- fert[!is.na(fert$tfr_female) & !is.na(fert$tfr_male), ]

### Functions ------------------------------------------

### Load the Schoumaker and Dudel data -----------------

# Load the schoumaker data:
# Data accessed from: https://perso.uclouvain.be/bruno.schoumaker/data/
path_global <- "U:/data/global/"
sd <- read_xlsx(paste0(path_global, "schoumaker_male/A. Estimates of male and female fertility.xlsx"),
                sheet = 1)
sd <- janitor::clean_names(sd)
sd <- sd[, c("country_name", "female_tfr_shown_on_figures", "male_tfr_shown_on_figures")]
names(sd) <- c("country", "tfr_female", "tfr_male")
sd$data <- "Schoumaker's fertility estimates"
sd[, c("tfr_male", "tfr_female")] <- apply(sd[, c("tfr_male", "tfr_female")], 2, as.numeric)
sd <- sd[!is.na(sd$tfr_female) & !is.na(sd$tfr_male), ]

# Load the male-fertility database:
# Data accessed from: https://www.fertilitydata.org/Home/Index
hfc_files <- list.files(paste0(path_global, "male_fertility_database"), full.names = T)
hfc <- lapply(hfc_files, read.delim, sep = ",")
hfc <- bind_rows(hfc)
hfc$CNTRY <- gsub(" +", "", hfc$Country)
save(hfc, file = "data/asfr_male_dudel21.Rda")
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

### Load the Schoen(1985) data:
rsd <- read.csv("data/schoen_birthsqueeze_1985.csv")
rsd$country <- gsub("^[0-I]+. ", "", rsd$ï..country)
rsd$tfr_ratio <- rsd$tfr_male / rsd$tfr_female
rsd$data <- "Robert Schoen, 1985" 

# Combine the Schoumaker's and Christian's data
ccd <- bind_rows(hfc, sd, rsd)

# Estimate the outcome: TFR ratio
ccd$tfr_ratio <- ccd$tfr_male / ccd$tfr_female

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

### 3. Review approach ----------------------

# Compare the different datas-ource
distr_tfr_ratio <- tapply(fert$tfr_ratio, fert$data,
                          function(x) quantile(x, probs = c(0.1, 0.5,  0.9)), simplify = F)
distr_tfr_ratio <- array2DF(distr_tfr_ratio)
distr_tfr_ratio$N <- as.numeric(tapply(fert$tfr_ratio, fert$data, length))
distr_tfr_ratio <- distr_tfr_ratio[, c("Var1", "N", "10%", "50%", "90%")]
stargazer(distr_tfr_ratio, summary = F)

# Estimate the diciles for the country-level data
country_fert <- fert[fert$data != "Subnational Fertility Data", ]
review_threshold <- round(quantile(country_fert$tfr_ratio, probs = c(0.1, 0.9)), 3)
fert$review_approach <- factor(ifelse(fert$tfr_ratio < review_threshold[1] | fert$tfr_ratio > review_threshold[2], 1, 0), 
labels = c("no squeeze", "birth squeeze"))


### 4. Stable population approach -----------

# Set the growth rate
r <- 1

# Create data for the normal range
stable_pop_normal <-  expand.grid(age_diff = seq(2, 4, by = 0.2),
                                  pop_diff = seq(0.915, 1.085, by = 0.005),
                                  srb = seq(1.03, 1.08, by = 0.01),
                                  r = seq(-0.035, 0.01, by = 0.05))
stable_pop_normal$tfr_ratio <- with(stable_pop_normal, 1/srb * pop_diff * exp(r*(age_diff)))
hist(stable_pop_normal$tfr_ratio)

# Create data for the extreme range
stable_pop_normal <-  expand.grid(age_diff = seq(2, 4, by = 0.2),
                                  pop_diff = seq(0.915, 1.085, by = 0.005),
                                  srb = seq(min(srb$srb), max(srb$srb), by = 0.01),
                                  r = seq(-0.05, 0.05, by = 0.05))
stable_pop_normal$tfr_ratio <- with(stable_pop_df, 1/srb * pop_diff * exp(r*(age_diff)))


# Create the birth squeeze variable
fert$stable_pop_approach <- factor(ifelse(fert$tfr_ratio < stable_pop_th[1] | fert$tfr_ratio > stable_pop_th[2], 1, 0), 
                               labels = c("no squeeze", "birth squeeze"))


# Function to obatin the range of TFR values
obtain_range <- function(variable = "srb", data = stable_pop_df){
  tmp_range <- range(data[[variable]])
  tmp <- data[data[[variable]] == tmp_range[1] | data[[variable]] == tmp_range[2], ]
  tapply(tmp$tfr_ratio, tmp[[variable]], function(x) round(range(x), 2))
}


# Create the range
map(c("srb", "pop_diff", "age_diff", "r"), obtain_range)

### 5. Impact approach -----------------

# Set the thresholds
impact_threshold_m <- c("lower"=0.552, "upper"=1.102)
impact_threshold_f <- c("lower"=0.924, "upper"=0.995)

# Identify the birth squeezes
fert$impact_approach_m <- factor(ifelse(fert$tfr_ratio < impact_threshold_m["lower"] | fert$tfr_ratio > impact_threshold_m["upper"], 1, 0), 
                               labels = c("no squeeze", "birth squeeze"))
fert$impact_approach_f <- factor(ifelse(fert$tfr_ratio < impact_threshold_f["lower"] | fert$tfr_ratio > impact_threshold_f["upper"], 1, 0), 
                                 labels = c("no squeeze", "birth squeeze"))


# Create the formal
impact_approach <- function(mu=0.05, alpha=-1.1363211, beta=0.3472586, r=1) {
  if (abs(mu) > 1) warning("Childlessness cannot be lower than 0!")
  # Subfunctions
  estimate_l <- function(alpha, beta, r) {
    exp(alpha + beta * r)
  }
  # Estimate parameters
  l <- estimate_l(alpha, beta, r)
  # Estimate the result
  den <- log((-l-mu*l-mu)/(mu-1+mu*l)) - alpha - beta * r
  return(den / beta)
}

# Estimate the result
sapply(c(0.01, 0.05), impact_approach)


#### Include the results in a plot --------

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
  p <- ggplot(data = subset(data, data == "Subnational Fertility Data"),
              aes(x = tfr_female, y = tfr_male, colour = {{approach}}, shape = {{approach}})) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(alpha = .5) +
    ggtitle(label) +
    scale_colour_viridis_d(option = "D", name = "") +
    scale_shape_discrete(name = "", solid = T) +
    scale_x_continuous("TFR female", expand = c(0, 0), limits = c(0, 8), breaks = 1:8)  +
    scale_y_continuous("TFR male", expand = c(0, 0), limits = c(0, 8), breaks = 1:8) + 
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) 
    
  return(print(p))
  }

# Create the different plots
naive <- plot_birth_squeeze()
empiric <- plot_birth_squeeze(approach = posterior_predict, label = "Empiric approach")
review <- plot_birth_squeeze(approach = review_approach, label = "Review approach")
stable <- plot_birth_squeeze(approach = stable_pop_approach, label = "Stable population approach")
impact_f <- plot_birth_squeeze(approach = impact_approach_m, label = "Impact approach (male)")
impact_m <- plot_birth_squeeze(approach = impact_approach_f, label = "Impact approach (female)")

# Assemble the plot
(naive + review) / (empiric + stable) / (impact_f + impact_m) +
  plot_layout(guides = "collect")
ggsave(last_plot(), filename = "figures/birth_squeezes_panel.pdf", height = 20, width = 15, unit = "cm")


naive <- fert |> 
  filter(data == "Subnational Fertility Data") |>
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
  filter(data == "Subnational Fertility Data") |>
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
ggsave(last_plot(), filename = "figures/share_birth_squeeze.pdf")

#  Plot the distribution of the different TFR ratios 
fert |> 
  select(data, tfr_ratio) |> 
  ggplot(aes(x = tfr_ratio, fill = data)) +
  geom_histogram(colour = "white") +
  geom_vline(xintercept = 1, colour = "red", linetype = "dashed", linewidth = 1.2) +
  facet_wrap(~ data, scales = "free_y") +
  scale_y_continuous("Number of region-year observations", expand = c(0, 0)) +
  scale_x_continuous("TFR ratio", expand = c(0, 0), n.breaks = 10, trans = "log") +
  scale_fill_viridis_d() +
  guides(fill = "none") +
  theme(
    strip.text = element_text(size = 10)
  )
ggsave(last_plot(), filename = "figures/distribution_tfr_datasets.pdf", height = 15, width = 20, unit = "cm")

# Plot the data
ggplot(fert, aes(x = tfr_female, y = tfr_male, colour = data, shape = data)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(alpha = .5) +
  scale_x_continuous("TFR female", limits = c(0, 10), n.breaks = 10, expand = c(0, 0)) +
  scale_y_continuous("TFR male", limits = c(0, 10), n.breaks = 10, expand = c(0, 0)) +
  theme(legend.position = c(0.65, 0.2)) +
  scale_colour_viridis_d(name = "Data source:") + 
  scale_shape(name = "Data source:") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)))

ggsave(last_plot(), filename = "figures/data_sources.pdf", height = 15, width = 15, unit = "cm")

### END ################################################