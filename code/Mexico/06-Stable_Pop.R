### Stable population analysis  #########################
# Purpose: Analyse the male and female fertility rates  #
# Author: Henrik-Alexander Schubert                     #
# E-Mail: schubert@demogr.mpg.de                        #
# Date: 01th June 2023                                  #
# Prerequisites: functions                              #
#########################################################

### Settings -----------------------------------------------------------------

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")

# Load the fertility rates
load("Data/asfr_national_mexico.Rda")
load("Data/tfr_national_mexico.Rda")

# Load the mortality data
mortality <- fread("Raw/mortality_mexico_un.csv")

### Clean the mortality data ------------------------------------------------

# Load the mortality data
mortality <- fread("Raw/mortality_mexico_un.csv")

# Clean the names
mortality <- mortality |>
  clean_names() |> 
  select(indicator_short_name, time, author, location, sex, age, value) |> 
  mutate(age = as.integer(str_remove_all(age, "\\+")))

## Estimate life tables by sex
lifetables_f <- mortality |> filter(sex == "Female")
lifetables_m <- mortality |> filter(sex == "Male")

# Split the data
mx_f <- split(lifetables_f$value, lifetables_f$time)
mx_m <- split(lifetables_m$value, lifetables_m$time)

# Estimate the lifetables
lifetables_m <- lapply(mx_m, lifetable, sex = "M")
lifetables_f <- lapply(mx_f, lifetable, sex = "F")

# Combine the lifetables
lifetables_f <- bind_rows(lifetables_f, .id = "id") |> mutate(sex = "Female", year = as.numeric(id)) 
lifetables_m <- bind_rows(lifetables_m, .id = "id") |> mutate(sex = "Male", year = as.numeric(id))

# Combine the male and the female life table
lifetables <- bind_rows(lifetables_m, lifetables_f)

# Get the names
lifetables <- lifetables |>
  select(-id)

### Estimate mean generation length -------------------------------------

# Estimate it for the lifetables
px <- lifetables |> select(age, px, sex, year)

# Pivot wider
px <- pivot_wider(px, values_from = "px", names_from = "sex")

# JOin the data
mor_fer <- inner_join(px, asfr_nat, by = c("year", "age")) 

# Estimate the generation length
gen_length <- mor_fer |> 
  group_by(year) |> 
  summarise(gen_len_m = sum(age * cumprod(Male) * asfr_m) / sum(cumprod(Male) * asfr_m),
            gen_len_f = sum(age * cumprod(Female) * asfr_f) / sum(cumprod(Female) * asfr_f))


### Estimate the growth rate -------------------------------------------

# Get the lifetables
lifetables_f <- lifetables |> filter(sex == "Female")

# Join with fertility data
mor_fer <- inner_join(lifetables_f, asfr_nat, by = c("year", "age")) 

# Estimate the growth rate
growth_rate <- mor_fer |>
  filter(year %in% 1990:2021) |> 
  mutate(asfr_f = if_else(is.na(asfr_f), 0, asfr_f)) |> 
  group_by(year) |> 
  summarise(r = sum((asfr_f * 0.4886) * Lx / 100000))

### Estimate the mean age at childbearing -------------------------------

# Join the data
mean_age <- asfr_nat |>
  group_by(year) |>
  summarise(mac_f = sum(age * asfr_f)/ sum(asfr_f),
            mac_m = sum(age * asfr_m)/ sum(asfr_m), 
            difference =  mac_m - mac_f)

# Combine the data
surv_mean_age <- inner_join(lifetables, mean_age, by = c("year")) |>
  mutate(across(c(mac_f, mac_m), round)) |> 
  filter((age == mac_f & sex == "Female") | (age == mac_m & sex == "Male")) |> 
  select(px, year, mac_f, mac_m, sex) |>
  pivot_wider(names_from = "sex", values_from = "px", names_prefix =  "surv_")

### Estimate stable population ratio -----------------------------------

# Join the data
data <- inner_join(surv_mean_age, growth_rate, by = "year") |> 
  inner_join(., gen_length, by = "year")

# Estimate Schoumakers ratio
data <- data |> mutate(survivor_ratio = surv_Male / surv_Female,
                        gen_diff = gen_len_m - gen_len_f,
                        male_female_tfr = 1 / 105 * survivor_ratio * exp(r * (gen_diff)))

### Plotting -------------------------------------------------------------

# Plot the lifetable survivors for men and women over time
ggplot(lifetables, aes(age, lx, alpha = year / max(year), colour = sex, group = year)) +
  geom_line() +
  facet_wrap(~ sex) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(alpha = "none")

# Plot the generation length
ggplot(gen_length, aes(x = year)) +
  geom_line(aes(y = gen_len_f, colour = "females"), linewidth = 1.4) +
  geom_line(aes(y = gen_len_m, colour = "males"), linewidth = 1.4) +
  scale_colour_manual(name = "Sex:", values = c(MPIDRred, MPIDRblue)) +
  ylab("Mean generation length") + xlab("Year")

# Plot the growth rate
ggplot(growth_rate, aes(year, r)) + 
  geom_hline(yintercept = 1, colour = "firebrick") +
  geom_line() +
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  ylab("Net reproductive rate") + xlab("Year")

# Plot the male to female TFR ratio
ggplot(data, aes(x = year, y = male_female_tfr)) +
  geom_line(aes(linetype = "stable population"), linewidth = 1.4) +
  geom_line(data = tfr_nat, aes(y = tfr_m / tfr_f, linetype = "observed"), linewidth = 1.4) +
  scale_y_continuous(expand = c(0, 0), n.breaks = 10, limits = c(0, 4)) +
  scale_linetype_manual(name = "Estimation:", values = c("solid", "dashed")) +
  theme(legend.position = c(0.8, 0.9)) +
  ylab("Male TFR / Female TFR")

# Plot the development of the mean age of childbearing
ggplot(mean_age, aes(x = year)) +
  geom_line(aes(y = mac_f, colour = "females"), linewidth = 1.4) +
  geom_line(aes(y = mac_m, colour = "males"), linewidth = 1.4) +
  scale_colour_manual(name = "Sex:", values = c(MPIDRred, MPIDRblue)) +
  ylab("Mean age of childbearing") + xlab("Year")

# Plot the components
ggplot(data, aes(x = year)) +
  geom_line(aes(y = gen_diff, colour = "Generation difference"), linewidth = 1.4) +
  geom_line(aes(y = survivor_ratio, colour = "Survivorship ratio"), linewidth = 1.4) +
  geom_line(aes(y = r, colour = "Growth rate"), linewidth = 1.4) +
  geom_hline(aes(yintercept = 1.05, colour = "Sex ratio at birth"), linewidth = 1.4) +
  scale_colour_manual(name = "Component:",
                      values = c(MPIDRred, MPIDRgreen, MPIDRpurple,  MPIDRorange)) +
  scale_y_continuous(trans = "log10", labels = scales::label_number_si()) +
  theme(legend.key.width = unit(0.4, "cm")) +
  ylab("Value") + xlab("Year")


### END ########################################################################  