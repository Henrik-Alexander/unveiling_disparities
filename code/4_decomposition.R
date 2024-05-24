########################################
# Purpose: Decompose all data          #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

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


# Wrangling --------------------------------

# Countries 
countries <- c("us", "mex", "deu", "fra", "aus")

# Load the data
for(country in countries) load(paste0("data/asfr_", country, ".Rda"))

# Harmonize the US data
asfr_us <- rename(asfr_us, region = state)

## Collect the data
asfr <- mget(ls(pattern = "asfr_"))

### Decomposition -----------------------------------------

df <- asfr_mex

# Create five year age groups
if (any(str_detect(names(df), "age$"))) {
  print("Not grouped!")
  df$age_group <- cut(df$age, breaks = seq(15, 55, by = 5), include.lowest = T, right = F)
  df |> 
    group_by(region, year, age_group) |> 
    summarise(across(ends_with("male"),  ~ mean(.x)))
} else {
  print("Is grouped!")
}

# 

### USA ---------------------------------------------------

# Estimate the TFR
females <- asfr_us |>
  rename(exposure = pop_female, births = births_female) |> 
  select(age, year, state, births, exposure) |> 
  pivot_longer(cols = c("exposure", "births"), names_to = "measure", values_to = "female")

# Estimate the TFR
males <- asfr_us |>
  rename(exposure = pop_male, births = births_male) |> 
  select(age, year, state, births, exposure) |> 
  pivot_longer(cols = c("exposure", "births"), names_to = "measure", values_to = "male") 

# Combine the data
births <- inner_join(females, males)

# Decompose the difference
decomposition <- births |> 
  arrange(year, state, measure, age) |> 
  group_by(year, state) |>
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