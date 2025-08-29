########################################
# Purpose: Regression modelling        #
# Author: Henrik-Alexander Schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.05.2025                     #
########################################

library(tidyverse)
library(latex2exp)
library(ggnewscale)
library(ggrepel)
library(stargazer)

source("functions/Graphics.R")

# Load the data
load("data/birth_squeezes.Rda")

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


# Subset the data to subnational level
fert <- fert[fert$data=="Subnational Fertility Data", ]

# Set the number of digits for the rounding
rounding <- 3

# Capitals
capitals <- c("Berlin", "Uusimaa", "District of Columbia", "Île-de-France", 
              "Comunidad de Madrid", "Ciudad de México", "Australian Capital Territory")

# East Germany
east_germany <- c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen")


# Functions ---------------------------

geometric_mean <- function(x) {
  exp(mean(log(x)))
}

# Report results for threshold approach
report_birth_squeezes <- function(x, country) {
  
  # Total number of observations
  cat("Total count:", table(x)[2], "\n")
  cat("Percentage share:", round(100*prop.table(table(x))[2], 2), "\n")
  
  # Country-specific total counts
  cat("Total counts per country:\n")
  print(table("country"=country, "Expert-based"=x))
  
  # Estimate the proportions in countries
  cat("Share per country:\n")
  print(round(100 * prop.table(table("country"=country, "Birth squeeze"=x), margin = 1), 2))
  
  
}

data_based_approach <- function(tfr_female) {
  lower <- exp(-0.161+0.304*log(tfr_female)) - 0.12
  upper <- exp(-0.161+0.304*log(tfr_female)) + 0.12
  list(lower=lower, upper=upper)
}
# Create the numbers -------------------

fert <- fert[!is.na(fert$tfr_ratio), ]

# N: Case numbers
N <- length(fert)

## Overall distribution

# Estimate the geometric mean
round(geometric_mean(fert$tfr_ratio), rounding)

# Average TFR ratio in high fertility regions
round(geometric_mean(fert$tfr_ratio[fert$tfr_female>3]), rounding)

# Average TFR ratio in low fertility regions
round(geometric_mean(fert$tfr_ratio[fert$tfr_female<2]), rounding)


## Capitals -------------------------

# Capitals
fert %>% 
  filter(region %in% capitals) %>% 
  group_by(region) %>% 
  mutate(maxyear = max(year)) %>% 
  filter(year == maxyear)

# City states
fert %>% 
  filter(region %in% c("Hamburg", "Massachusetts")) %>% 
  group_by(region) %>% 
  mutate(maxyear = max(year)) %>% 
  filter(year == maxyear)

# East Germany
round(max(fert$tfr_ratio[fert$region %in% east_germany]), 2)

# Regression with capital
fert$capital <- fert$region %in% capitals
summary(lm(tfr_ratio ~ tfr_female + capital, data=fert))

# Estimate the birth squeezes ========

# Expert-based approach
report_birth_squeezes(fert$expert_based_approach, fert$country)

# Data based approach
report_birth_squeezes(fert$data_based_approach, fert$country)

# Stable population approach
report_birth_squeezes(fert$stable_pop_approach, fert$country)

# Outcome based approach
report_birth_squeezes(fert$outcome_based_approach, fert$country)
sum(fert$outcome_based_approach == "no squeeze")
round(mean(fert$outcome_based_approach == "no squeeze")*100, 2)

# Stable population table --------------

load("data/stable_population.Rda")

# Create a vector for input ranges
input_values <- expand.grid("srb" = c(1.04, 1.06),
                     "pop_ratio" = c(0.915, 1.085),
                     "gen_length" = c(3, 4),
                     "r" = c(-0.01, 0.01))

# Estimate the ratio
input_values$tfr_ratio <- with(input_values, 1/srb * pop_ratio * exp(r*(gen_length)))

# Estimate the range
input_values <- pivot_longer(input_values, cols=!tfr_ratio)

# Create the table for the stable population approach
stable_pop_table <- input_values %>% 
  group_by(name) %>% 
  mutate(min_value = min(value), max_value = max(value)) %>% 
  group_by(value) %>% 
  summarise(component = unique(name), 
            min=unique(min_value), 
            max=unique(max_value), 
            U = paste(round(R_to_U(min(tfr_ratio)), 2), "to", round(R_to_U(max(tfr_ratio)), 2)),
            tfr_ratio = paste(round(min(tfr_ratio), 2), "to",  round(max(tfr_ratio), 2))
           ) %>%
  select(component, value, tfr_ratio, U) 


# Create total
total_pop_stable <- input_values %>% 
  summarise(component = "Total", 
            U = paste(round(R_to_U(min(tfr_ratio)), 2), "to", round(R_to_U(max(tfr_ratio)), 2)),
            tfr_ratio = paste(round(min(tfr_ratio), 2), "to",  round(max(tfr_ratio), 2)))

# Create the table
stable_pop_table <- bind_rows(stable_pop_table, total_pop_stable)

# Order the table
stable_pop_table %>% 
  mutate(component = factor(component,
                            levels = c("srb", "pop_ratio", "gen_length", "r", "Total"), 
                            labels = c("Sex ratio at birth", "Population ratio", "Generation length", "Growth rate", "Total"))) %>% 
  arrange(component, value) %>% 
  stargazer(summary=F)

## Plot the birth squeezes ------------

load("results/tfr_ratio_draws.Rda")

# Estimate the parameters of the regression
alpha <- median(draws[, , "alpha"])
beta <- median(draws[, , "beta[1]"])
sigma <- median(draws[, , "sigma"])

# Count the number of birth squeezes
fert$squeeze_count <- apply(fert %>% select(ends_with("approach")), 1, FUN = function(x) sum(x != "no squeeze"))

# Plot the distribution
ggplot(subset(fert, data=="Subnational Fertility Data"), aes(x=tfr_female, y=tfr_ratio)) +
  #geom_hline(yintercept = 1, linewidth=1.5) +
  scale_y_continuous(TeX("$\\frac{TFR_{men}}{TFR_{women}}$"), trans="log10", n.breaks=10, limits = c(0.7, 1.4), expand = c(0, 0)) +
  scale_x_continuous(TeX("$TFR_{women}$"), trans="log10", n.breaks=10) + 
  #guides(colour="none") +
  # Add the boundaries for the approaches
  geom_function(fun=function(x) exp(alpha + beta * log(x)) + qnorm(0.1, sd = sigma), aes(colour="Data-based"), linewidth=1.5) +
  geom_function(fun=function(x) exp(alpha + beta * log(x)) + qnorm(0.9, sd = sigma), aes(colour="Data-based"), linewidth=1.5) +
  # Add the expert-based approach
  geom_hline(aes(yintercept = 0.9, colour="Expert-based"), linewidth=1.5) +
  geom_hline(aes(yintercept = 1.1, colour="Expert-based"), linewidth=1.5) +
  # Add the expert-based approach
  geom_hline( aes(yintercept = 0.78, colour="Stable-population"), linewidth=1.5) +
  geom_hline( aes(yintercept = 1.15, colour="Stable-population"), linewidth=1.5) +
  # Add the expert-based approach
  geom_hline( aes(yintercept = 0.86, colour="Outcome-based"), linewidth=1.5) +
  geom_hline( aes(yintercept = 1.13, colour="Outcome-based"), linewidth=1.5) +
  # Add points
  geom_text_repel(data=subset(fert, data=="Subnational Fertility Data" & region == "Brandenburg" & year == 1995), aes(label=paste0(region, " (", year, ")")), min.segment.length = 1, nudge_y=-0.02, nudge_x=-0.03, family="serif") +
  geom_text_repel(data=subset(fert, data=="Subnational Fertility Data" & region == "Chiapas" & year == 1990), aes(label=paste0(region, " (", year, ")")), min.segment.length = 1, nudge_y=0.02, nudge_x=-0.02, family="serif") +
  # Create the legends
  scale_colour_manual("Approach", values=c(MPIDRblue, MPIDRorange, MPIDRred, MPIDRpurple)) +
  new_scale_color() +
  geom_point(alpha=0.5, aes(colour=ordered(squeeze_count))) +
  scale_colour_brewer("Count of squeeze", palette = "Reds") +
  guides(colour="none") +
  theme(axis.title.y = element_text(angle=0, vjust=0.5),
        legend.position = c(0.1, 0.85))

ggsave(filename="figures/alternative_birth_squeeze.pdf", height=22, width=24, unit="cm")


# Plot the distribution
ggplot(subset(fert, data=="Subnational Fertility Data"), aes(x=tfr_female, y=tfr_ratio)) +
  #geom_hline(yintercept = 1, linewidth=1.5) +
  scale_y_continuous(TeX("$\\frac{TFR_{men}}{TFR_{women}}$"), trans="log10", n.breaks=10, limits = c(0.7, 1.4), expand = c(0, 0)) +
  scale_x_continuous(TeX("$TFR_{women}$"), trans="log10", n.breaks=10) + 
  #guides(colour="none") +
  # Add the boundaries for the approaches
  geom_function(fun=function(x) exp(alpha + beta * log(x)) + qnorm(0.1, sd = sigma), aes(colour="Data-based"), linewidth=1.1) +
  geom_function(fun=function(x) exp(alpha + beta * log(x)) + qnorm(0.9, sd = sigma), aes(colour="Data-based"), linewidth=1.1) +
  # Add the expert-based approach
  geom_hline(aes(yintercept = 0.9, colour="Expert-based"), linewidth=1.1) +
  geom_hline(aes(yintercept = 1.1, colour="Expert-based"), linewidth=1.1) +
  # Add the expert-based approach
  geom_hline( aes(yintercept = 0.78, colour="Stable-population"), linewidth=1.1) +
  geom_hline( aes(yintercept = 1.15, colour="Stable-population"), linewidth=1.1) +
  # Add the expert-based approach
  geom_hline( aes(yintercept = 0.86, colour="Outcome-based"), linewidth=1.1) +
  geom_hline( aes(yintercept = 1.13, colour="Outcome-based"), linewidth=1.1) +
  # Add points
  geom_text_repel(data=subset(fert, data=="Subnational Fertility Data"), aes(label=paste0(region, " (", year, ")")), family="serif") +
  # Create the legends
  scale_colour_manual("Approach", values=c(MPIDRblue, MPIDRorange, MPIDRred, MPIDRpurple)) +
  new_scale_color() +
  facet_wrap(~ country, nrow=4) +
  geom_point(alpha=0.5, aes(colour=ordered(squeeze_count))) +
  scale_colour_brewer("Count of squeeze", palette = "Reds") +
  guides(colour="none") +
  theme(axis.title.y = element_text(angle=0, vjust=0.5),
        legend.position = c(0.1, 0.85))

ggsave(filename="figures/alternative_birth_squeeze_panel.pdf", height=22, width=24, unit="cm")

# Plot the distribution
ggplot(fert, aes(x=tfr_female, y=tfr_male)) +
  geom_hline(yintercept = 1) +
  geom_point(alpha=0.3) +
  scale_y_continuous(TeX("$TFR_m$"), trans="log10", n.breaks=15, expand = c(0, 0)) +
  scale_x_continuous(TeX("$TFR_w$"), trans="log10", n.breaks=15, expand = c(0, 0)) + 
  guides(colour="none") +
  new_scale_color() +
  # Add the boundaries for the approaches
  geom_function(fun=function(x) x * (0.66 + 0.136 * x), aes(colour="Data-based")) +
  geom_function(fun=function(x) x * (0.85 + 0.136 * x), aes(colour="Data-based")) +
  # Add the expert-based approach
  geom_function(fun = function(x) x * 0.9, aes(colour="Expert-based")) +
  geom_function(fun = function(x) x * 1.1, aes(colour="Expert-based")) +
  # Add the expert-based approach
  geom_function(fun = function(x) x * 0.78, aes(colour="Stable-population")) +
  geom_function(fun = function(x) x * 1.15, aes(colour="Stable-population")) +
  # Add the expert-based approach
  geom_function(fun = function(x) x * 0.86, aes(colour="Outcome-based")) +
  geom_function(fun = function(x) x * 1.13, aes(colour="Outcome-based")) +
  scale_colour_viridis_d()

# Plot the trend in Germany ------------

# Filter the country data
fert %>% 
  filter(country=="Germany") %>% 
  mutate(east = if_else(region %in% east_germany, "East Germany", "West Germany")) %>% 
  ggplot(aes(x=year, y=tfr_ratio, group=region, colour=east, shape=east)) +
  geom_hline(yintercept = 1) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c(MPIDRgreen, MPIDRpurple)) +
  scale_y_continuous("TFR ratio", n.breaks = 10, trans="log10") +
  scale_x_continuous("Year", n.breaks = 12, expand = c(0, 0.1)) +
  theme(legend.position = c(0.17, 0.81),
        legend.title = element_blank())

ggsave("figures/tfr_ratio_germany_eastwest.pdf", height=15, width=20, unit="cm")


### END ################################