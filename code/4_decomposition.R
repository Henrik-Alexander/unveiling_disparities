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

# Remove missing region informaiton in Germany
asfr_deu <- asfr_deu[!is.na(asfr_deu$region), ]

## Collect the data
asfr <- mget(ls(pattern = "asfr_"))

### Decomposition -----------------------------------------

# Create five year age groups
aggregate_to5 <- function(df = asfr_mex) {
if (any(str_detect(names(df), "age$"))) {
  df$age_group <- cut(df$age, breaks = seq(15, 55, by = 5), include.lowest = T, right = F)
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
dataset <- asfr_deu

tfr_decomposition <- function(dataset, years = unique(dataset$year), regions = unique(dataset$region)) {
    
  results <- vector("list", length = length(years) * length(regions))
  names(results) <- as.vector(outer(years, regions, paste, sep = "_"))
  
  for (y in years) {
    for (r in regions) {
  
      # Prepare the data
      sex <- c("female", "male")
      variables <- c("births", "exposure")
      vars <- as.vector(outer(variables, sex, paste, sep = "_"))
      tmp <- dataset[dataset$region == r & dataset$year == y, c("age_group", vars)]
      #tmp <- tmp |> mutate(across(everything(), ~ replace_na(.x, 0)))
      
      # Make the horiushi decomposition
      tmp <- tmp |> 
        pivot_longer(cols = ends_with("male"), names_to = c("measure", "sex"), names_sep = "_") |> 
        pivot_wider(names_from = "sex") |> 
        arrange(measure, age_group)
      
      # Make the decomposition
      tmp$contribution <- horiuchi(calc_tfr, tmp$male, tmp$female, N = 20)
      tmp$difference <- calc_tfr(tmp$male) - calc_tfr(tmp$female)
      
      # Assign the result 
      results[[paste(y, r, sep = "_")]] <- tmp
    }} 
  return(results)
}

# Make the decompositions across countries
for (country in countries){
  cat("Country:", county)
  tfr_decomposition(asfr[[county]])
}

decomp_res <- map(asfr, tfr_decomposition)



####
    
    ggplot(tmp, aes(x = age_group, y = contribution * difference, fill = measure)) +
      geom_col() + 
      #geom_text(x = 35, y = 0.08, aes(label = paste("male - female TFR =", round(difference, 2)))) + 
      geom_hline(yintercept = 0) +
      #facet_wrap( vars(year, state), strip.position = "top", labeller = label_wrap_gen(multi_line=FALSE)) +
      scale_fill_brewer(name = "Component:", palette = "Set1") +
      theme(strip.background = element_blank(), strip.placement = "outside") +
      xlab("Age") + ylab("Contribution")
    
    
####
    
    # Estimate the direct and indirect differences between male and female fertility
    tmp$pop_share_female <- pop_share(tmp$exposure_female)
    tmp$pop_share_male <- pop_share(tmp$exposure_male)
    tmp$delta_pop = difference(tmp$pop_share_female, tmp$pop_share_male)
    tmp$delta_rate <- difference(tmp$asfr_female, tmp$asfr_male)
    
    # Combine the results
    result <- tmp[, "age_group"]
    result$change_pop <- tmp$asfr_female * tmp$delta_pop
    result$change_rate <- tmp$exposure_female * tmp$delta_rate
    result$change_inter <- tmp$delta_pop * tmp$delta_rate
    
    # Make a long table
    result |> pivot_longer(cols = starts_with("change"), names_to = "component", values_to = "contribution")
    


# Make the decomposition
decomposition <- comp_afr_mex |>
  dplyr::select(year, age, change_rate, 
                change_pop, change_inter, region) |>
  pivot_longer(cols = starts_with("change"),
               names_to = "component",
               values_to = "contribution") 


### USA ---------------------------------------------------

# Estimate the TFR
females <- asfr_us |>
  rename(exposure = pop_female, births = births_female) |> 
  select(age, year, region, births, exposure) |> 
  pivot_longer(cols = c("exposure", "births"), names_to = "measure", values_to = "female")

# Estimate the TFR
males <- asfr_us |>
  rename(exposure = pop_male, births = births_male) |> 
  select(age, year, region, births, exposure) |> 
  pivot_longer(cols = c("exposure", "births"), names_to = "measure", values_to = "male") 

# Combine the data
births <- inner_join(females, males)

# Decompose the difference
decomposition <- births |> 
  arrange(year, region, measure, age) |> 
  group_by(year, region) |>
  mutate(contribution = horiuchi(calc_mac, male, female, N = 20 ),
         difference = calc_mac(male) - calc_mac(female))

# Plot the decomposition
ungroup(decomposition) |>
  mutate(max = max(difference), min = min(difference)) |>
  filter(difference %in% c(max, min)) |>
  ggplot(aes(x = age, y = contribution, fill = measure)) +
  geom_col() + 
  geom_text(x = 35, y = 0.08, aes(label = paste("male - female TFR =",round(difference, 2)))) + 
  geom_hline(yintercept = 0) +
  facet_wrap( vars(year, state), strip.position = "top", labeller = label_wrap_gen(multi_line=FALSE)) +
  scale_fill_brewer(name = "Component:", palette = "Set1") +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  xlab("Age") + ylab("Contribution")
ggsave(last_plot(), filename = "Figures/decomp_us.pdf")

# Arrange the data
births |> 
  arrange(year, state, measure, age) |> 
  group_by(year, state) |> 
  summarise(tfr_male = calc_mac(male),
            tfr_female = calc_mac(female))

### Mexico ------------------------------------------------

# Estimate the TFR
females <- asfr_reg |>
  arrange(year, entity, age) |> 
  rename(exposure = population_f, births = births_f) |> 
  select(age, year, entity_name, births, exposure) |> 
  pivot_longer(cols = c("exposure", "births"), names_to = "measure", values_to = "female")

# Estimate the TFR
males <- asfr_reg |>
  arrange(year, entity, age) |> 
  rename(exposure = population_m, births = births_m) |> 
  select(age, year, entity_name, births, exposure) |> 
  pivot_longer(cols = c("exposure", "births"), names_to = "measure", values_to = "male") 

# Combine the data
births <- inner_join(females, males)

# Decompose the difference
decomposition <- births |> 
  arrange(year, entity_name, measure, age) |> 
  group_by(year, entity_name) |>
  mutate(contribution = horiuchi(calc_mac, male, female, N = 20),
         difference = calc_mac(male) - calc_mac(female))

### END ####################################