### Analysis  ###########################################
# Purpose: Analyse the male and female fertility rates  #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 30th May 2023                                   #
# Prerequisites: functions                              #
#########################################################

### Settings -----------------------------------------------------------------

rm(list = ls())

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")

# Load the data
load("Data/births_father.Rda")
load("Data/births_mother.Rda")
load("Data/exposure_females.Rda")
load("Data/exposure_males.Rda")


### Estimate the rates -------------------------------------------------------

# Merge 
males <- inner_join(births_fat, pop_m,
                    by = c("year", "age_fat" = "age", "entity" = "geo_code"),
                    suffix = c("", "_name"))

# Estimate the ASFR
asfr_male <- males |> mutate(asfr_male = births / mid_year_pop)

# Estimate the TFR
tfr_male <- asfr_male |> summarise(tfr_male = sum(asfr_male), .by = c(year, entity))

## Do the same for females --------------------------------------------------

# Merge 
females <- inner_join(births_mot, pop_f,
                      by = c("year", "age_mot" = "age", "entity" = "geo_code"),
                      suffix = c("", "_name"))

# Estimate the ASFR
asfr_female <- females |> mutate(asfr_female = births / mid_year_pop)

# Estimate the TFR
tfr_female <- asfr_female |> summarise(tfr_female = sum(asfr_female), .by = c(year, entity))


### JOin the data ---------------------------------------------------------

### Compare
asfr_mex <- left_join(asfr_female, asfr_male,
                              by = c("age_mot" = "age_fat", "year", "entity", "entity_name"),
                              suffix = c("_female", "_male")) |> 
                    rename(age = age_mot, region = entity_name)


# Join male and female TFR+
tfr_mex <- inner_join(tfr_male, tfr_female)


### Estimate the TFR at the national level ---------------------------------

# National level TFR
asfr_nat <- asfr_mex |>
                  group_by(year, age) |>
                  summarise(exposure_female = sum(mid_year_pop_female),
                            births_female   = sum(births_female),
                            asfr_female     = births_female / exposure_female,
                            exposure_male = sum(mid_year_pop_male),
                            births_male   = sum(births_male),
                            asfr_male     = births_male / exposure_male,
                            .groups = "drop")



# Estimate the TFR at the national leve
tfr_nat <- asfr_nat |> 
                    group_by(year) |> 
                    summarise(tfr_femaleem = sum(asfr_female),
                              tfr_maleale = sum(asfr_male),
                              tfr_ratio = tfr_maleale / tfr_femaleem,
                              mac_femaleem = sum(age * asfr_male) / sum(asfr_male),
                              mac_maleale = sum(age * asfr_female) / sum(asfr_female),
                              mac_diff = mac_maleale - mac_femaleem,
                              .groups = "drop")

### Decomposition national ----------------------------------------------------

# Plot the crude birth rate for men and women
cbr_nat <- asfr_nat |>
  group_by(year) |> 
  summarise(cbr_female = 1000 * sum(births_female) / sum(exposure_female),
            cbr_male = 1000 * sum(births_male) / sum(exposure_male))

# Estimate the decomposition
comp_asfr <- asfr_nat |> 
  group_by(year) |> 
  mutate(across(starts_with("asfr"), ~ .x * 1000)) |> 
  mutate(pop_share_female = pop_share(exposure_female),
         pop_share_male = pop_share(exposure_male),
         mean_pop    = averaging(pop_share_female, pop_share_male),
         mean_rate   = averaging(asfr_female, asfr_male),
         delta_rate  = (asfr_female - asfr_male) * mean_pop,
         delta_pop   = (pop_share_female - pop_share_male) * mean_rate,
         .groups     = "drop")

# Make the decomposition
decomp_nat <- comp_asfr %>%
  dplyr::select(year, age, delta_rate, delta_pop) %>%
  pivot_longer(cols = starts_with("delta"),
               names_to = "component",
               values_to = "contribution") |> 
  inner_join( cbr_nat, by = "year") |> 
  mutate(contribution = contribution / (cbr_female - cbr_male))


### Decomposition regional ----------------------------------------------------

# Estimate the decomposition
comp_afr_mex <- asfr_mex |>
  group_by(year, region) |>
  mutate(pop_share_female = pop_share(mid_year_pop_female),
         pop_share_male = pop_share(mid_year_pop_male),
         delta_pop = difference(pop_share_female, pop_share_male),
         delta_rate = difference(asfr_female, asfr_male),
         change_rate = pop_share_female * delta_rate,
         change_pop  = asfr_female * delta_pop,
         change_inter = delta_pop * delta_rate)

# Make the decomposition
decomposition <- comp_afr_mex |>
  dplyr::select(year, age, change_rate, 
         change_pop, change_inter, region) |>
  pivot_longer(cols = starts_with("change"),
               names_to = "component",
               values_to = "contribution") 

### Save the data -------------------------------------------------------------

save(asfr_nat, file = "Data/asfr_national_maleexico.Rda")
save(tfr_nat, file = "Data/tfr_national_maleexico.Rda")
save(asfr_mex, file = "Data/asfr_maleex.Rda")
save(tfr_male, file = "Data/tfr_maleexional_maleexico.Rda")


### Plot the national rates ---------------------------------------------------

# Plot the TFR trend
ggplot(tfr_nat, aes(year)) +
  geom_line(aes(y = tfr_female, colour = "females"), linewidth = 1.4)  +
  geom_line(aes(y = tfr_male, colour = "males"), linewidth = 1.4) +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  scale_colour_maleanual(name = "TFR:", values = c(MPIDRred, MPIDRblue)) +
  xlab("Year") + ylab("Total fertility rate") + 
  theme(legend.position = c(0.8, 0.9))

# Plot the age shift
ggplot(asfr_nat, aes(age, asfr_female, group = year, colour = year)) +
  geom_line() +
  scale_colour_gradient(low = MPIDRgreen, high = MPIDRyellow) +
  xlab("Age of the mother") +
  ylab("Age-specific fertility rate")


### Plot the male-female difference for several years -------------------------

# Plot the ASFR
afr_maleex |> 
  filter(entity == 9 & year %in% c(1990, 2000, 2010, 2020)) |> 
  ggplot(aes(x = age)) +
  geom_line(aes(y = asfr_female, colour = "females"), linewidth = 2) +
  geom_line(aes(y = asfr_male, colour = "males"), linewidth = 2) +
  facet_wrap(~ year) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_maleanual(values = c( MPIDRred, MPIDRblue), name = "Sex:") +
  ylab("Age-specific fertility rate") +
  xlab("Age")


### Plot the distribution over time
tfr_maleex |> 
  pivot_longer(cols = starts_with("tfr"), names_prefix = "tfr_", values_to = "tfr", names_to = "sex") |> 
  filter(sex != "TFR_ratio") |> 
  ggplot(aes(as.factor(year), tfr, fill = sex, colour = sex)) +
  scale_femaleill_maleanual(values = c( MPIDRred, MPIDRblue), name = "Sex:") +
  scale_colour_maleanual(values = c( MPIDRred, MPIDRblue), name = "Sex:") +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.9)) +
  ylab("Total fertility rate") +
  xlab("Year")


### Plot the decompositions ---------------------------------------------------

# Plot the decomposition
decomp_nat %>% 
  filter(year %in% c(1990, 2000, 2010, 2020)) %>% 
  ggplot(aes(age, contribution, fill = component)) +
  geom_col(colour = "white", linewidth = 0.01) +
  geom_hline(yintercept = 0, colour = "black") +
  facet_wrap(~ year) +
  scale_femaleill_maleanual(values = c(MPIDRred,  MPIDRblue),
                    labels = c(expression(paste(Delta, "Population")),
                               expression(paste(Delta, "Rate"))),
                    name = "Component: ") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("% Contribution") + xlab("Age") +
  theme(legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.2, "cm"))


# Plot the time trend for population estimates
decomp_nat |> 
  filter(age %in% c(15, 25, 35, 45)) |> 
  ggplot(aes(x = year, contribution, colour = component, group = component)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, colour = "black") +
  scale_colour_maleanual(values = c(MPIDRred, MPIDRorange, MPIDRblue),
                      labels = c(expression(paste(Delta, "Population and rate")),
                                 expression(paste(Delta, "Population")),
                                 expression(paste(Delta, "Rate"))),
                      name = "Component: ") +
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap( ~ age)


# Plot the decomposition
decomp_nat |> 
  filter(year %in% c(1990, 2020) &
           entity_name %in% c("Baja California Sur",
                              "Guerrero")) |> 
  ggplot(aes(age, contribution, fill = component)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "black") +
  facet_grid(entity_name~ year) +
  scale_femaleill_maleanual(values = c(MPIDRred, MPIDRorange, MPIDRblue),
                    labels = c(expression(paste(Delta, "Population and rate")),
                               expression(paste(Delta, "Population")),
                               expression(paste(Delta, "Rate"))),
                    name = "Component: ") +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Contribution") + xlab("Age")

### END ########################################################################  
