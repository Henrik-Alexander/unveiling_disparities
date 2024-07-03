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
library(patchwork)

# Load the graphics
source("functions/Graphics.R")

# Load the fertility data
load("data/fert_data_subnational.Rda")

# Remove observations without male or female TFR
fert <- fert[!is.na(fert$tfr_female) & !is.na(fert$tfr_male), ]

### Load the Schoumaker and Dudel data -----------------

# Load the schoumaker data
# Data accessed from: https://perso.uclouvain.be/bruno.schoumaker/data/
path_global <- "U:/data/global/"
sd <- read.xlsx(paste0(path_global, "schoumaker_male/A. Estimates of male and female fertility.xlsx"), sheetIndex = 1)
sd <- sd[, c("Country.name", "Female.TFR..shown.on.Figures.", "Male.TFR..shown.on.Figures.")]
names(sd) <- c("country", "tfr_female", "tfr_male")
sd$data <- "Schoumaker's fertility estimates"
sd[, c("tfr_male", "tfr_female")] <- apply(sd[, c("tfr_male", "tfr_female")], 2, as.numeric)
sd <- sd[!is.na(sd$tfr_female) & !is.na(sd$tfr_male), ]

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
hfc$data <- "human fertility collection"
hfc <- hfc[!is.na(hfc$tfr_female) & !is.na(hfc$tfr_male), ]


# Combine the Schoumaker's and Christian's data
ccd <- bind_rows(hfc, sd)

# Estimate the outcome: TFR ratio
ccd$tfr_ratio <- ccd$tfr_male / ccd$tfr_female

### Combine the data ----------------------------------

# Add a data column
fert$data <- "subnational fertility data"

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
  scale_colour_viridis_d() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)))

ggsave(last_plot(), filename = "figures/data_sources.pdf", height = 15, width = 15, unit = "cm")

### END ################################################