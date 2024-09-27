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

# Load the national fertiltiy data
load("data/male_national_fertility.Rda")

# Remove observations without male or female TFR
fert <- fert[!is.na(fert$tfr_female) & !is.na(fert$tfr_male), ]

## Combine the data -------------------------

# Add a data column
fert$data <- "Subnational Fertility Data"

# Combine the data
fert <- bind_rows(fert, ccd)

# Filter missing data
fert <- fert[!is.na(fert$tfr_ratio), ]

# Estimate the Schoen index
fert$U <- with(fert, (tfr_male-tfr_female)/(0.5*tfr_male+0.5*tfr_female))

# 1. Expert-based approach
source("code/thresholds/expert_based_approach.R")

# 2. Data-based approach
source("code/thresholds/data_based_approach.R")

# 3. Stable population approach
source("code/thresholds/stable_population_approach.R")

### 4. Impact approach
source("code/thresholds/impact_approach.R")


#### Include the results in a plot --------

# Estimate the population squeeze for one region-year observation
estimate_population_squeeze <- function(data) {
  sex <- c("female", "male")
  measures <- c("births", "exposure")
  variables <- as.vector(outer(measures, sex, paste, sep="_"))
  if(!all(variables %in% names(data))){
    stop("Does not have the right columns!")
  }
  if(any(duplicated(data$age_group))){
    stop("Not unique row observations!")
  }
  sum(data$births_female/data$exposure_male, na.rm = T) / sum(data$births_female/data$exposure_female, na.rm = T)
}

# Estimate the fertility squeeze for one region-year observation
estimate_fertility_squeeze <- function(data) {
  sex <- c("female", "male")
  measures <- c("births", "exposure")
  variables <- as.vector(outer(measures, sex, paste, sep="_"))
  if(!all(variables %in% names(data))) {
    stop("Does not have the right columns!")
    }
  if(any(duplicated(data$age_group))){
    stop("Not unique row observations!")
  }
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
plot_birth_squeeze <- function(data = fert, approach = expert_based_approach, label = "Expert-based approach") {
  p <- ggplot(data = subset(data, data == "Subnational Fertility Data"),
              aes(x = tfr_female, y = tfr_male, colour = {{approach}}, shape = {{approach}})) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(alpha = .5) +
    ggtitle(label) +
    scale_colour_viridis_d(option = "D", name = "") +
    scale_shape_discrete(name = "", solid = T) +
    ylab(NULL) +
    scale_x_continuous("TFR female", expand = c(0, 0), limits = c(0, 8), breaks = 1:8)  +
    scale_y_continuous("TFR male", expand = c(0, 0), limits = c(0, 8), breaks = 1:8) + 
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) 
    
  return(print(p))
  }

# Create the different plots
expert <- plot_birth_squeeze()
data_based <- plot_birth_squeeze(approach = data_based_approach, label = "Data-based approach")
stable <- plot_birth_squeeze(approach = stable_pop_approach, label = "Stable population approach")
impact <- plot_birth_squeeze(approach = outcome_based_approach, label = "Outcome-based approach")

# Assemble the plot
(expert + data_based) / (stable + impact) +
  plot_layout(guides = "collect")
ggsave(last_plot(), filename = "figures/birth_squeezes_panel.pdf", height = 20, width = 15, unit = "cm")

# Function plot by category
plot_country_shares <- function(variable = expert_based_approach, label = "Expert-based approach", data = fert) {
  
  fert |> 
    filter(data == "Subnational Fertility Data") |>
    group_by(country) |> 
    mutate(total = n()) |> 
    group_by(country, {{variable}}) |> 
    summarise(share = 100 * n()/unique(total), .groups = "drop") |> 
    arrange(share) |> 
    ggplot(aes(x = country, y = share, fill = {{variable}})) + 
    geom_col() +
    coord_flip() +
    scale_y_continuous("% Share", expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_fill_viridis_d(name = "") + 
    ggtitle(label) +
    theme(
      axis.title.y = element_blank()
    )
  
}

# Create plots
e_country_shares <- plot_country_shares()
d_country_shares <- plot_country_shares(data_based_approach, "Data-based approach")
s_country_shares <- plot_country_shares(stable_pop_approach, "Stable-population approach")
o_country_shares <- plot_country_shares(outcome_based_approach, "Outcome-based approach")
e_country_shares + d_country_shares + s_country_shares + o_country_shares +
  plot_layout(guides = "collect", ncol = 2)
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