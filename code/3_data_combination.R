########################################
# Purpose: Regression modelling        #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

# Clean the environment
rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(janitor)
library(stargazer)
library(patchwork)
library(ggrepel)
library(ggExtra)
library(ppcor)
library(reshape2)
library(broom)
library(ineq)
library(latex2exp)
library(lme4)
library(texreg)

# Load the functions
source("Functions/Functions.R")
source("Functions/Graphics.R")

# Load the development data
load("data/development.Rda")

### Prepare the fertility data ===================================

# Preperate the Finnish data
fert_fin <- read.csv("U:/data/fin/birth_registers/regional_tfrs_macs.csv")

# Estimate the regional TFRS
fert_fin <- fert_fin |> 
  rename(region = province) |> 
  mutate(tfr_ratio = tfr_male / tfr_female,
         mac_diff = mac_male - mac_female, 
         country = "Finland")

# Estimate the Finnish fertility
fert_fin_nat <- fert_fin |> 
  group_by(year) |> 
  summarise(tfr_male = mean(tfr_male),
            tfr_female = mean(tfr_female),
            tfr_ratio = tfr_male / tfr_female,
            country = "Finland",
            .groups = "drop")

### Clean the Australian data -----------------------

# Prepare the Australian data
load("data/asfr_aus.Rda")
fert_aus <- asfr_aus |> 
  group_by(year, age_group, region) |> 
  summarise(asfr_male = sum(births_male) / sum(exposure_male),
            asfr_female = sum(births_female, na.rm = T) / sum(exposure_female, na.rm = T)) |> 
  group_by(year, region) |> 
  summarise(tfr_male = sum(asfr_male * 5, na.rm = T),
            tfr_female = sum(asfr_female * 5, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "Australia",
            .groups = "drop")


# Create the national data
fert_aus_nat <- asfr_aus |> 
  group_by(year, age_group) |> 
  summarise(asfr_male = sum(births_male) / sum(exposure_male),
            asfr_female = sum(births_female, na.rm = T) / sum(exposure_female, na.rm = T)) |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male * 5, na.rm = T),
            tfr_female = sum(asfr_female * 5, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "Australia",
            .groups = "drop")

### Clean the Spanish data -------------------------------

# Load the spanish data
load("Data/tfr_esp_nat.Rda")
load("data/tfr_esp.Rda")

# Clean the regional data
fert_esp <- tfr_esp_reg |> 
  mutate(country = "Spain") 

fert_esp_nat <- tfr_esp_nat |> 
  mutate(country = "Spain", 
         tfr_ratio = tfr_male / tfr_female)


### Prepare the french data -----------------------------

# Prepare the French data
load("Data/asfr_fra.Rda")

# Create the regional data
fert_fra <- asfr_fra |> 
  group_by(year, region) |> 
  summarise(tfr_female = sum(asfr_female, na.rm = TRUE),
            tfr_male   = sum(asfr_male, na.rm = TRUE),
            tfr_ratio  = tfr_male / tfr_female,
            mac_female = sum(asfr_female * age, na.rm = T) / sum(asfr_female, na.rm = TRUE),
            mac_male = sum(asfr_male * age, na.rm = T) / sum(asfr_male, na.rm = TRUE),
            mac_diff = mac_male - mac_female,
            country = "France",
            .groups = "drop")

# Create the national data
fert_fra_nat <- asfr_fra |> 
  group_by(year, age) |> 
  summarise(asfr_male = sum(births_male, na.rm = T) / sum(exposure_male, na.rm = T),
            asfr_female = sum(births_female, na.rm = T) / sum(exposure_female, na.rm = T)) |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male, na.rm = T),
            tfr_female = sum(asfr_female, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "France",
            .groups = "drop")


### Prepare the Mexican data -----------------------------

# Load the Mexican data fertility data
load("data/asfr_regional_mexico.Rda")
fert_mex <- asfr_reg |>
  group_by(entity_name, year) |> 
  summarise(tfr_male = sum(asfr_m),
            tfr_female = sum(asfr_f),
            tfr_ratio  = sum(asfr_m) / sum(asfr_f),
            mac_male = sum(asfr_m * age) / sum(asfr_m),
            mac_female = sum(asfr_f * age) / sum(asfr_f),
            mac_diff = mac_male - mac_female,
            country = "Mexico",
            .groups = "drop") |> 
  mutate(mac_diff = mac_male - mac_female,
         tfr_ratio = tfr_male / tfr_female) |> 
  rename(region = entity_name) 

# Load the national data
load("data/asfr_national_mexico.Rda")

# Create the national ldata
fert_mex_nat <- asfr_nat |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_m, na.rm = T),
            tfr_female = sum(asfr_f, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "Mexico",
            .groups = "drop")

### Prepare the US data ---------------------------

load("data/asfr_us.Rda")

# Estimate the regional TFR ratios
fert_us <- asfr_us |>
  group_by(state, year) |> 
  summarise(tfr_male = sum(asfr_male),
            tfr_female = sum(asfr_female),
            mac_male = sum(asfr_male * age) / sum(asfr_male),
            mac_female = sum(asfr_female * age) / sum(asfr_female),
            country = "United States",
            .groups = "drop") |> 
  mutate(tfr_ratio = tfr_male / tfr_female) |> 
  rename(region = state) 

# Estimate the regional TFR ratios
fert_us_nat <- asfr_us |>
  group_by(year, age) |> 
  summarise(asfr_male = sum(births_male) / sum(pop_male),
            asfr_female = sum(births_female) / sum(pop_female)) |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male),
            tfr_female = sum(asfr_female),
            tfr_ratio = tfr_male / tfr_female,
            country = "United States",
            .groups = "drop") 

### Prepare the german data ---------------------------

# Load the data
load("data/asfr_deu.Rda")

# Estimate the regional TFR ratios
fert_deu <- asfr_deu |> 
  group_by(region, year) |> 
  summarise(tfr_male = sum(asfr_male * 5, na.rm = T),
            tfr_female = sum(asfr_female * 5, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
         country = "Germany",
         .groups = "drop") 

# Estimate the national TFR ratios
fert_deu_nat <- asfr_deu |> 
  group_by(year, age_group) |> 
  summarise(asfr_male = sum(births_male) / sum(exposure_male),
            asfr_female = sum(births_female) / sum(exposure_female)) |> 
  group_by(year) |> 
  summarise(tfr_male = sum(asfr_male * 5, na.rm = T),
            tfr_female = sum(asfr_female * 5, na.rm = T),
            tfr_ratio = tfr_male / tfr_female,
            country = "Germany",
            .groups = "drop") 


### Load the Colombian data ---------------------------------
load("data/asfr_col.Rda")

# Estimate the TFR ratios
fert_col <- asfr_col[, .(tfr_male = sum(asfr_male*5),
                         tfr_female = sum(asfr_female*5),
                         tfr_ratio = sum(asfr_male*5) / sum(asfr_female*5), country = "Colombia"), by = .(year, region)]

# National fertility
fert_col_nat <- asfr_col[, .(asfr_male = sum(birth_father)/sum(exposure_males), asfr_female = sum(birth_mother)/sum(exposure_females)), by = .(age_group, year)]
fert_col_nat <- fert_col_nat[, .(tfr_male = sum(asfr_male * 5),
                                 tfr_female = sum(asfr_female * 5), 
                                 country = "Colombia"), by = year]
fert_col_nat[, tfr_ratio := tfr_male / tfr_female]

### Merge the country data sets -----------------------------

# Bind the national data
fert_nat <- bind_rows(fert_mex_nat, fert_us_nat, fert_aus_nat, fert_deu_nat, 
                      fert_esp_nat, fert_fra_nat, fert_fin_nat, fert_col_nat)

# Bind the data
fert <- bind_rows(fert_mex, fert_us, fert_aus, fert_deu, fert_fra, fert_fin, fert_esp, fert_col)

# Save the data
save(fert, file = "data/fert_data_subnational.Rda")
write.csv(fert, "data/fert_data_subnational.csv", fileEncoding = "utf-8")
write.csv(fert_nat, "data/fert_data_national.csv", fileEncoding = "utf-8")

# Combine
dev <- left_join(dev, fert, by = c("year", "region", "country"))

# Remove spain from the data
write.csv(dev, "data/final_fertility_development.csv", fileEncoding = "utf-8")

### Plot the male female difference ---------------------

# Collect the extreme values
max_ratio <- max(dev$tfr_ratio)
min_ratio <- min(dev$tfr_ratio)
max_diff  <- max(dev$mac_diff)
min_diff  <- min(dev$mac_diff)

# Plot the relationship of mean age of childbearing
ggplot(dev, aes(tfr_female, tfr_male, shape = country, fill = country, label = region)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(colour = "white", stroke = 0.3, size = 2) +
  ylab(TeX(r"($TFR_{male}$)")) + xlab(TeX(r"($TFR_{female}$)")) +
  scale_fill_viridis_d(name = "Country", option = "D") +
  scale_shape_manual(name = "Country", values = shape_countries) +
  geom_text_repel(data = subset(dev, tfr_ratio %in% c(max_ratio, min_ratio) | tfr_male == max(dev$tfr_male)), alpha = 1, colour = "black") +
  guides(alpha = "none") +
  coord_cartesian(xlim = c(0, 8), ylim = c(0, 8), expand = F)


ggsave(last_plot(), filename = "Figures/male_female_tfr.pdf", height = 15, width = 22, unit = "cm")

### Model the TFR ratio ---------------------------------

# Estimate a model of TFR ratio on TFR female
mod_ratio <- lm(tfr_ratio ~ tfr_female, data = fert)

# Predict
fert$tfr_ratio_pred <- predict(mod_ratio, fert)

# Plot
ggplot(fert, aes(tfr_ratio_pred - tfr_ratio, fill = country)) +
  geom_histogram() +
  geom_vline(xintercept = 0) +
  facet_wrap(~ country, nrow = 2, scales = "free_y") +
  scale_x_continuous("Residual: TFR ratio on female TFR") +
  scale_y_continuous("", expand = c(0, 0)) +
  scale_fill_viridis_d(option = "D")

# Filter the residuals
fert <- fert |> 
  mutate(residual = abs(tfr_ratio_pred - tfr_ratio))
labels <- fert |> 
  filter(residual > 0.2) |> 
  group_by(region) |> 
  mutate(max = max(residual)) |> 
  filter(max == residual) 
ggplot(subset(fert, residual > 0.2), aes(tfr_female, tfr_ratio, colour = country)) +
  geom_abline(intercept = coef(mod_ratio)[1], slope = coef(mod_ratio)[2]) +
  geom_text_repel(data = labels, aes(label = region)) +
  geom_point() +
  scale_x_continuous("TFR female") +
  scale_y_continuous("TFR ratio") +
  scale_fill_viridis_d(option = "D") + 
  guides(colour = "none")


### Quantum ---------------------------------------------

# Create the label vector
labels <- dev |> 
  group_by(country) |> 
  filter(!is.na(hdi) & !is.na(tfr_male) & year == 2004) |> 
  mutate(count = row_number()) |> 
  filter(count == 1)

# Estimate the regression model
plot_a <- ggplot(dev, aes(hdi, tfr_ratio, group = country, colour = country, fill = country, shape = country)) +
  geom_hline(yintercept = 1) +
  geom_point(colour = "white", stroke = 0.08, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, formula = "y ~ x") +
  scale_fill_viridis_d(name = "Country", option = "D") +
  scale_shape_manual(name = "Country", values = shape_countries) +
  scale_colour_viridis_d(name = "Country", option = "D") +
  scale_y_continuous(trans = "log") +
  ylab(TeX(r"( $\frac{TFR_{male}}{TFR_{female}}$)")) + 
  xlab("Human development indicator") +
  guides(alpha = "none") +
  theme(legend.position = "bottom")

# Save the figure
figs(plot_a, "dev_tfr_diff")

# Estimate the regression
model_quantum <- lm(log(tfr_ratio) ~ hdi, data = dev)

# Summary
summary(model_quantum)

# Print the results
stargazer(model_quantum,
          ci = TRUE,
          title = "Ordinary least squares regression model of the ratio of the male to female TFR on the Human Development Index (HDI).",
          label = "mod: quantum",
          out = "Results/reg_tfr_diff_hdi.tex")

# Summary
summary(model_quantum)

# Print the results
model_quantum_state <- lm(tfr_ratio ~ hdi + country, data = dev)
baselines = sapply( model_quantum_state$xlevels, "[[", 1)
names(model_quantum_state$coefficients)[1] = paste0( names(baselines), " = ", baselines, 
                                                     collapse="; ")
stargazer(model_quantum_state,
          covariate.labels = c("Intercept", "HDI", "France", "Mexico","U.S."),
          ci = TRUE,
          title = "Ordinary least squares regression model of the ratio of the male to female TFR on the Human Development Index (HDI). Reference country is Finland.",
          label = "mod: quantum",
          out = "Results/reg_tfr_diff_hdi_states.tex")


# Estimate the regression model
plot_b <- ggplot(dev, aes(gdi, tfr_ratio, fill = country, colour = country, group = country, shape = country)) +
  geom_hline(yintercept = 1) +
  geom_point(colour = "white", stroke = 0.08, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, formula = "y ~ x") +
  scale_fill_viridis_d(name = "Country", option = "D") +
  scale_colour_viridis_d(name = "Country", option = "D") +
  scale_shape_manual(name = "Country", values = shape_countries) +
  scale_y_continuous(trans = "log") +
  ylab(TeX(r"( $\frac{TFR_{male}}{TFR_{female}}$)")) + 
  xlab("Gender development indicator") + 
  guides(alpha = "none") +
  theme(legend.position = "bottom")


# Save the figure
figs(plot_b,  "gdi_tfr_diff")


# Estimate the regression
model_quantum2 <- lm(log(tfr_ratio) ~ gdi, data = dev)

# Summary
summary(model_quantum2)

# Print the results
stargazer(model_quantum2,
          ci = TRUE,
          title = "Ordinary least squares regression model of the ratio of the male to female TFR on the Gender Development Index (GDI).",
          label = "mod: quantum",
          out = "Results/reg_tfr_diff_gdi.tex")


# Assemble the plots
(plot_a + plot_b) + 
  plot_layout(guides = 'collect') + plot_annotation(tag_levels = "A", tag_suffix = ")")

ggsave(last_plot(), filename = "Figures/dev_fert_ratio.pdf")


stargazer(model_quantum, model_quantum2,
          ci = TRUE,
          title = "Ordinary least squares regression model of fertility quantum and timing on human development index.",
          label = "mod: hdi",
          dep.var.labels = c("$Quantum", "$\\Delta$ Quantum"),
          covariate.labels = c("HDI", "Intercept"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          out = "Results/reg_hdi_tfr_ratio.tex")

### Controlling for Gender and State fixed effects ------------------

# Demean
demean <- function (x) x - mean(x, na.rm = T)

# Create fixed effects data
dev_fe <- dev |> 
  group_by(region) |> 
  mutate(across(where(is.double), demean))

# Estimate the regression
model_quantum3 <- lm(tfr_ratio ~ hdi + gdi + factor(year), data = dev)
model_quantum3_country <- lm(tfr_ratio ~ -1 + hdi + gdi + factor(year), data = dev_fe)

# Print the result
stargazer(model_quantum3, model_quantum3_country,
          dep.var.labels = c("$\\Delta$ Quantum", "$\\Delta$ Quantum"),
          ci = TRUE,
          title = "Ordinary least squares regression model of the ratio of the male to female TFR and the difference in mean age of childbreaing on the Human Development Index (HDI) and Gender development Index (GDI). Reference country is Finland.",
          label = "mod: control",
          out = "Results/reg_fe_gdi_hdi.tex")

# Plot the results
model_quantum <- tidy(model_quantum3_country, conf.int = T) |> 
  filter(term %in% c("hdi", "gdi")) |> 
  mutate(outcome = "TFR ratio")


# Fixed effects regression
rbind(model_quantum) |> 
  mutate(term = toupper(term)) |> 
  mutate(label = ifelse(p.value < 0.05, paste0(round(estimate, 2), "*"), round(estimate, 2))) |> 
  ggplot(aes(x = term, y = estimate, colour = term)) +
  geom_point(size = 5) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), linewidth = 3) +
  geom_text(aes(label = label), nudge_x = 0.25, colour = "black", size = 8, family = "serif") +
  geom_hline(yintercept = 0) +
  facet_wrap(~ outcome) +
  guides(colour = "none") +
  scale_colour_viridis_d(name = "Country", option = "D") 
figs(last_plot(), "regression_coefficient")

# Partial correlation coefficients ----------------------------


# Partial correlation coefficient
demeaned <- dev |> 
  dplyr::select(hdi, gdi, mac_diff, tfr_ratio, region, country, year) |> 
  group_by(country) |> 
  mutate(across(c(hdi, gdi, mac_diff, tfr_ratio), ~ .x - mean(.x, na.rm = T))) |> 
  filter(!is.na(gdi) & !is.na(mac_diff))

# Estimate the
corr_ratio <- pcor(demeaned[, c("hdi", "tfr_ratio", "gdi")])$estimate
corr_diff <- pcor(demeaned[, c("hdi", "mac_diff", "gdi")])$estimate

rbind(melt(corr_ratio), melt(corr_diff)) |> 
  filter(Var2 %in% c("mac_diff", "tfr_ratio") & Var2 != Var1) |> 
  mutate(Var2 = str_replace(Var2, "_", " "),
         Var1 = toupper(Var1)) |> 
  ggplot(aes(x = Var1, y = value)) +
  geom_col() +
  geom_text(aes(y = value * 0.8, label = round(value, 2)), colour = "white", family = "serif", size = 10) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ Var2) +
  scale_y_continuous("Partial correlation coefficient") +
  theme(axis.title.x = element_blank())
ggsave(last_plot(), filename = "figures/partial_correlation_coefficient.pdf", height = 15, width = 15, unit = "cm")

# Trend in variation and mean ------------------------

# Functions for the estimation
gini <- function(data, column = "tfr_ratio")  ineq(data[[column]], type = "Gini")
est_mean <- function(data, column = "tfr_ratio") mean(data[[column]], na.rm = T)

# Split the data
fert_trend <- split(fert, f = list(fert$year, fert$country))

# Estimate the gini coefficient
gini.tfr_ratio <- sapply(fert_trend, gini, column = "tfr_ratio")
mean.tfr_ratio <- sapply(fert_trend, est_mean, column = "tfr_ratio")
trend_names <- names(gini.tfr_ratio)

# Create a data set
trend_fert <- mget(ls(pattern = "^gini.|^mean."))
trend_fert <- bind_cols(trend_fert)
trend_fert$year <- as.numeric(str_extract(trend_names, "^[0-9]+"))
trend_fert$country <- str_extract(trend_names, "[A-Za-z ]+$")
trend_fert <- pivot_longer(trend_fert, cols = contains("."), 
                           names_pattern = "(.*)\\.(.*)",
                           names_to = c("measure", "indicator"))
trend_fert <- trend_fert[!is.na(trend_fert$value) & trend_fert$country != "Spain", ]
trend_fert <- pivot_wider(trend_fert, names_from = "measure", values_from = "value")
trend_fert$indicator <- ifelse(trend_fert$indicator == "mac_diff", "MAC difference", "TFR ratio")


# Plot the result
ggplot(subset(trend_fert, indicator == "tfr_ratio"), aes(x = year, y = mean, colour = country)) +
  geom_line(linewidth = 2) +
  geom_line(data = fert, aes(group = region, y = tfr_ratio), alpha = .3)

# Plot the gini coefficient
gini_a <- ggplot(subset(trend_fert, indicator == "TFR ratio"), 
                 aes(x = year, y = gini, colour = country, fill = country, shape = country)) +
  geom_point(alpha = .5) +
  geom_smooth(linewidth = 1.5, method = "loess", formula = "y~x") +
  scale_shape_manual(values = shape_countries) +
  scale_x_continuous("Year", expand = c(0, 0)) +
  scale_y_continuous(TeX(r"(Gini coefficient: $\frac{TFR_{male}}{TFR_{female}}$)"), expand = c(0, 0)) +
  scale_colour_viridis_d(name = "Country", option = "D") +
  scale_fill_viridis_d(name = "Country", option = "D") +
  theme(
    panel.spacing.x = unit(1.2, "cm")
  ) +
  coord_cartesian(xlim = c(1990, 2020), ylim = c(0, 0.1))


# Plot the gini coefficient
mean_a <- ggplot(subset(trend_fert, indicator == "TFR ratio"), 
                 aes(x = year, y = mean, colour = country, fill = country, shape = country)) +
  geom_line(data = fert, aes(y = tfr_ratio, group = region), alpha = .3) +
  geom_hline(yintercept = 1) +
  geom_line(linewidth = 2) +
  geom_point(size = 3, aes(shape = country), colour = "white") +
  scale_x_continuous("Year", expand = c(0, 0)) +
  scale_y_continuous(TeX(r"( $\frac{TFR_{male}}{TFR_{female}}$)"), expand = c(0, 0)) +
  scale_colour_viridis_d(name = "Country", option = "D") +
  scale_fill_viridis_d(name = "Country", option = "D") +
  scale_shape_manual(name = "Country", values = shape_countries) +
  theme(
    panel.spacing.x = unit(1.2, "cm")
  ) +
  coord_cartesian(xlim = c(1990, 2020))


# Exceptions ---------------------------------------------------

fert |> 
  filter(country == "Germany") |> 
  ggplot(aes(x = year, y = tfr_ratio, group = state, colour = east_west, linetype = east_west)) + 
  geom_line(linewidth = 1.2) +
  geom_text_repel(data = subset(fert, country == "Germany" & year == 2018 & (tfr_ratio > 0.947 | tfr_ratio < 0.82)), aes(label = region), nudge_x = 1) +
  scale_x_continuous("Year", expand = c(0, 0)) +
  scale_y_continuous(TeX(r"( $\frac{TFR_{male}}{TFR_{female}}$)"), expand = c(0, 0)) +
  scale_colour_viridis_d("", option = "E") +
  scale_linetype("") +
  coord_cartesian(xlim = c(1995, 2021)) 

figs(last_plot(), "tfr_ratio_germany_eastwest")

# Multi-level modelling ---------------------------------------

# Basic model
yre_tfr <- lmer(tfr_ratio ~ hdi + (1 | year), data = dev)

# Random intercept model: country and year intercept
ri_tfr <- lmer(tfr_ratio ~ hdi + (1 | year) + (1 | country), data = dev, REML = FALSE)

# Random slope model
rs_tfr <- lmer(tfr_ratio ~ hdi + (1 + hdi | country) + (1 | year), data = dev, REML = FALSE)

# results
texreg(list(yre_tfr, ri_tfr, rs_tfr), file = "results/ml_model_hdi.tex")

# Create the Plot for random slopes
coeffs <- coef(rs_tfr)$country
countries <- unique(dev$country)
windowsFonts(A = windowsFont("Times New Roman"))
plot(dev$hdi, dev$tfr_ratio, bg = "white", pch = 19, col = viridis_palette[countries],
     xlab = "Human Development Index", ylab = "Male-Female TFR Ratio", family = "A", las = 1)
abline(1, 0)
abline(coeffs[countries[1], 1], coeffs[countries[1], 2], ol = viridis_palette[countries[1]], lwd = 2)
for (country in countries) {
  cat("Country:", country, "\n")
  abline(coeffs[country, 1], coeffs[country, 2], col = viridis_palette[country], lwd = 2)
}
for (y in seq(0.6, 0.9, by = 0.1)){
  abline(v = y, col = "grey", lty = 3)
}
for(x in seq(0.7, 1.2, by = 0.1)){
  abline(h = x, col = "grey", lty = 3)
}
legend(0.9, 1.35, legend = unique(dev$country), col = viridis_palette[unique(dev$country)], pch = 19)
pdf(file = "figures/random_slope_graph_dev.pdf", width = 20, height = 25)


### MAC -------------------

# Model comparison
anova(yre_tfr, ri_tfr, rs_tfr)

## Multi level modelling for age gap
yre_mac <- lmer(tfr_ratio ~ hdi + (1 | year), data = dev)

# Random intercept model: country and year intercept
ri_mac <- lmer(tfr_ratio ~ hdi + (1 | year) + (1 | country), data = dev, REML = FALSE)

# Random slope model
rs_mac <- lmer(tfr_ratio ~ hdi + (1 + hdi | country) + (1 | year), data = dev, REML = FALSE)

# Model comparison
anova(yre_mac, ri_mac, rs_mac)


### Compare the German pop pyramids

asfr_deu |> 
  filter(region == "Sachsen-Anhalt") |> 
  filter(year %in% c(1995, 2018) & lower < 45) |> 
  mutate(pop_ratio = 100 * exposure_male / exposure_female) |> 
  pivot_longer(cols = starts_with("exposure"), names_prefix = "exposure_", names_to = "sex") |> 
  mutate(pop = ifelse(sex == "male", -value, value) ) |> 
  ggplot(aes(x = lower, y = pop/100, fill = sex)) + 
  geom_col(width = 5, colour = "white") +
  geom_hline(yintercept = 0) +
  geom_text(aes(y = 0, label = round(pop_ratio, 0)), colour = "white", size = 10) +
  facet_wrap(~ year, scales = "free_x") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous("Age", expand = c(0, 0), n.breaks = 8) +
  scale_y_continuous("Pop in k", labels = function(x) paste0(abs(x), "k"))
figs(last_plot(), "pop_pyramid_sachsen_anhalt")

### END ############################################