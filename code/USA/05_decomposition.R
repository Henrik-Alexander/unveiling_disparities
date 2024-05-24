########################################
# Purpose: Decompose USA data          #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

# Load the decomposition module
library(remotes)
install_github("timriffe/DemoDecomp")

# Load the data
load("data/asfr_us.Rda")

# Function to estimate the tfr
calc_tfr <- function(pars){
  
  # step 1: convert from vec into something we can use
  N        <- length(pars)
  dim(pars)<- c(N / 2, 2) # rows, columns
  
  # step 2: calculate the result
  return(sum(pars[, 1] / pars[, 2]))
}

# Test the function
exp <- asfr_us[asfr_us$year == 2001 & asfr_us$state == "Alabama",]$pop_female
bir <- asfr_us[asfr_us$year == 2001 & asfr_us$state == "Alabama",]$births_female
pars <- c(bir, exp)
calc_tfr(pars)

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
  mutate(contribution = horiuchi(calc_tfr, male, female, N = 20 ),
         difference = calc_tfr(male) - calc_tfr(female))

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
  summarise(tfr_male = calc_tfr(male),
            tfr_female = calc_tfr(female))

### END ####################################