########################################
# Purpose: Decompose all data          #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

rm(list = ls())

# Load the decomposition module
library(remotes)
install_github("timriffe/DemoDecomp")

# Load packages
library(tidyverse)
library(DemoDecomp)
library(data.table)

# Load funciton
source("functions/functions.R")
source("functions/graphics.R")

# Functions -------------------------------------

# Function to estimate the tfr
calc_mac <- function(pars){
  
  # step 1: convert from vec into something we can use
  ages <- 15:50
  N        <- length(pars)
  dim(pars)<- c(N / 2, 2) # rows, columns
  
  # step 2: calculate the result
  rates <- (pars[, 1] / pars[, 2])
  return(sum(rates*ages) / sum(rates))
}

# Function to estimate the tfr
calc_tfr <- function(pars) {
  # Step 1: convert from vec into something we can use
  N <- length(pars)
  dim(pars) <- c(N / 2, 2)
  # Step2: calculate the result
  rates <- (pars[, 1] / pars[, 2])
  return(sum(rates * 5, na.rm = T))
}


# Wrangling --------------------------------

# Countries 
countries <- c("us", "mex", "deu", "fra", "aus")

# Load the data
for(country in countries) load(paste0("data/asfr_", country, ".Rda"))

# Harmonize the US data
asfr_us <- rename(asfr_us, region = state)
names(asfr_us) <- gsub("pop", "exposure", names(asfr_us))

# Remove missing region informaiton in Germany
asfr_deu <- asfr_deu[!is.na(asfr_deu$region), ]

## Collect the data
asfr <- mget(ls(pattern = "asfr_"))

### Decomposition -----------------------------------------

# Funciton to clean 5-year age groups
clean_age_group <- function(age_group){
  lower <- str_sub(age_group, 2, 3)
  upper <- as.numeric(str_sub(age_group, 5, 6))-1
  return(paste0(lower, "-", upper))
}

# Create five year age groups
aggregate_to5 <- function(df = asfr_mex) {
if (any(str_detect(names(df), "age$"))) {
  df$age_group <- cut(df$age, breaks = seq(15, 55, by = 5), include.lowest = T, right = F)
  df$age_group <- clean_age_group(df$age_group)
  df <- df |> 
    group_by(region, year, age_group) |> 
    summarise(across(ends_with("male"),  ~ mean(.x))) |> 
    ungroup()
} 
  return(df)
}

# Aggregate age-groups
asfr <- map(asfr, aggregate_to5)

#  Make the counterfactual estimation
tfr_decomposition <- function(dataset, years = unique(dataset$year), regions = unique(dataset$region), measure = "TFR") {
    
  tmp <- split(dataset, list(dataset$region, dataset$year))
  
      # Prepare the data
      prep_data <- function(tmp) {
  
      sex <- c("female", "male")
      variables <- c("births", "exposure")
      vars <- as.vector(outer(variables, sex, paste, sep = "_"))
      tmp <- tmp[, c(vars, "age_group")]

      # Make the horiushi decomposition
      tmp <- tmp |> 
        pivot_longer(cols = ends_with("male"), names_to = c("measure", "sex"), names_sep = "_") |> 
        pivot_wider(names_from = "sex") |> 
        arrange(measure, age_group)
      return(tmp)
      }
      
      # Make the decomposition
      if (measure == "TFR") {
      decomp_data <- function(tmp){
        tmp$contribution <- horiuchi(calc_tfr, tmp$male, tmp$female, N = 20)
        tmp$difference <- calc_tfr(tmp$male) - calc_tfr(tmp$female)
        return(tmp)
      }
      } else {
        decomp_data <- function(tmp){
          tmp$contribution <- horiuchi(calc_mac, tmp$male, tmp$female, N = 20)
          tmp$difference <- calc_mac(tmp$male) - calc_mac(tmp$female)
          return(tmp)
        }
      }
    
      tmp <- lapply(tmp, prep_data)
      tmp <- tmp[unlist(lapply(tmp, nrow)) > 0]
      tmp <- lapply(tmp, decomp_data)
      tmp <- rbindlist(tmp, idcol = "label")
      tmp$region <- str_split(tmp$label, pattern = "\\.", simplify = T)[, 1]
      tmp$year <- as.numeric(str_split(tmp$label, pattern = "\\.", simplify = T)[, 2])
      
  return(tmp)
}

# Decompose the different asfr
decomp_res <- map(asfr, tfr_decomposition)
decomp_res <- rbindlist(decomp_res, idcol = "country")
decomp_res$country <- gsub("asfr_", "", decomp_res$country)
decomp_res$age_group <- ifelse(decomp_res$country == "deu", 
                               paste0(str_sub(decomp_res$age_group, 1, 2), "-", as.numeric(str_sub(decomp_res$age_group, 4, 5))), 
                                      decomp_res$age_group)
decomp_res$group <- ifelse(decomp_res$difference >= 0, "Higher male TFR", "Lower male TFR")


# Plot the result
ggplot(subset(decomp_res, age_group %in% unique(decomp_res$age_group)[1:8]), aes(x = age_group, y = contribution * difference, group = interaction(region, year), colour = year)) +
  geom_hline(yintercept = 0) +
  geom_line(alpha = .3) +
  facet_grid(group ~ measure) +
  scale_x_discrete("Age", expand = c(0, 0)) +
  scale_y_continuous("Contribution") +
  scale_colour_viridis_c(option = "D") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key.width = unit(2, "cm"),
    panel.spacing.x = unit(0.4, "cm")
  ) 
figs(last_plot(), "decomp_tfr_all")
####
    
    ggplot(tmp, aes(x = age_group, y = contribution * difference, fill = measure)) +
      geom_col() + 
      #geom_text(x = 35, y = 0.08, aes(label = paste("male - female TFR =", round(difference, 2)))) + 
      geom_hline(yintercept = 0) +
      #facet_wrap( vars(year, state), strip.position = "top", labeller = label_wrap_gen(multi_line=FALSE)) +
      scale_fill_brewer(name = "Component:", palette = "Set1") +
      theme(strip.background = element_blank(), strip.placement = "outside") +
      xlab("Age") + ylab("Contribution")
    
    

### END ####################################