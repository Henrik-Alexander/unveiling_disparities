#######################################
# Project: Subnational birth squeezes #
# Purpose: Bayesian linear regression #
# Author: Henrik-Alexander Schubert   #
# E-mail: schubert@demogr.mpg.de      #
# Date: 20.09.2024                    #
#######################################

# Load the packages
library(cmdstanr)
library(posterior)
library(bayesplot)
library(stargazer)

# Set the seed
seed <- 1789

### Functions ---------------------------

init_generator <- function(md = md, chain_id = 1) {
  
  result <- vector("list")
  
  # Create the input values
  result[['alpha']] <- runif(1, -10, 10)
  result[['beta']] <- runif(md$K, -10, 10)
  result[['sigma']] <- runif(1, 0, 1e2)
  
  return(result)
}

# Get the prediction intervals
predict_interval <- function(mu, sigma=0.084, alpha=0.05, lower=T) {
  p <- ifelse(lower, 0.05, 0.95)
  exp(mu + sigma * qnorm(p))
}


### Data wrangling ----------------------

# Load the data
df <- read.csv("raw/country_male_fertility.csv")

# Define the colours
cols <- viridisLite::viridis(n = 200)
names(cols) <- unique(df$country)

# Plot the data
plot(NA, 
     xlim = range(df$tfr_female), 
     ylim = range(df$tfr_ratio),
     xlab = "TFR female",
     ylab = "TFR ratio",
     main = "Total fertility rate to ratio")

# Plot the countries
for (i in unique(df$country)) {
  tmp <- df[df$country == i, ]
  points(tmp$tfr_female, tmp$tfr_ratio, 
         pch = 16,
        lwd = 1, col = cols[i])
}

# Add the line
abline(lm(tfr_ratio ~ tfr_female, data = df), 
       lwd = 2, lty = 2)
abline(h = 1)


#legend("bottomright", legend = unique(df$country), cex = 15)

# Select the columns
df <- df[, c("tfr_ratio", "tfr_female")]

# Remove missing values
df <- na.omit(df)

# Take the logarithm of the values
df$tfr_ratio <- log(df$tfr_ratio)

### Prior predictive checks -------------------------------

# Size
n <- 1e5
x <- 2.1

# Define the parameters 
alpha <- rnorm(n, 0, 2)
beta <- rgamma(n, 1)
sigma <- abs(rnorm(n, 0, 1))

# Predictor variable
x <- seq(0.1, 3, by = 0.1)

# Create the plot
plot(NA, 
     xlim = range(x),
     ylim = c(0, 10))

# Number of points
sim <- 1000

# Create y
# for (i in seq_along(x)) {
#   # Create the mu
#   mu <- alpha + beta * x[i]
#   cat("Mu:", exp(mu[1]), "; Sigma=", sigma[1], "\n")
#   res <- rlnorm(sim, meanlog = mu[1], sdlog = sigma[1])
#   points(x = rep(x[i], sim), y = res,
#        main = paste0("Mu=", exp(mu[1]), "; sigma=", sigma[1]))
#   Sys.sleep(1)
# }

 

### Create the model matrix -------------------------------

# Compile the model
model <- cmdstan_model("code/thresholds/country_regression.stan")

# Set the parameters
parameters <- c("tfr_female")

# Format the data as list for Stan
md <- list(N = nrow(df),
           K = length(parameters),
           y = df$tfr_ratio,
           x = as.matrix(log(df[, parameters])),
           seed = seed
           )

# Create the MCMC
chains <- 4
warmup <- 1e3
samples <- 2e3
inits <- lapply(1:chains, function(id) init_generator(md=md, chain_id=id))

# Run the model
fit <- model$sample(data = md,
                  parallel_chains = chains,
                  init = inits,
                  iter_sampling = samples,
                  iter_warmup = warmup,
                  save_warmup = TRUE,
                  seed = md$seed)

# Save the result
saveRDS(fit, "results/bayesian_result.RDS")

### Post estimation analysis -----------------------------

# Make trace plots
trace_style_np(div_color = "green")
bayesplot::mcmc_trace(fit$draws(),
                     pars = c("alpha",
                              "beta[1]",
                              "sigma"))


# MCMC sampler diagnostic
fit$diagnostic_summary()

### Extract the results
draws <- fit$draws()

# Plot the result
plot(exp(md$x), exp(md$y), xlim=c(0, 9), ylim = c(0, 2.5))
curve(exp(median(draws[, , "alpha"]) + median(draws[, , "beta[1]"]) * log(x)), add=T)


### Explore results -------------------------------

# Summary of the model
pars <- c("alpha", "beta[1]", "sigma")
summary_table <- fit$summary(variables = pars)

# Extract the important information from the summary table
summary_table <- summary_table %>% 
  select(variable, median, q5, q95) %>% 
  mutate(across(is.numeric, round, digits=3)) %>% 
  mutate(variable = paste0("$", str_remove(variable, "\\[1\\]"), "$"))
stargazer(summary_table, summary=F, out="results/log_mod_table.tex")

# Marginal posterior distribution
bayesplot::mcmc_areas(fit$draws(), pars=pars, prob=0.95)
bayesplot::mcmc_hist(fit$draws(), pars=pars)

### Plot the result --------------------------------


# Check normality
check_normality <- function(x) {
  hist(x, breaks=length(x)*0.05, col="white", main="Check for normality")
  abline(v=mean(x), lwd=2, col="red")
  lines(density(x), col="red", lwd=3, lty=2)
  lines(sort(x), dnorm(x=sort(x), mean=mean(x), sd=sd(x)), lwd=3)
  legend("topright", legend = c("Normal distribution", "Observed"), col=c("black", "red"), lty=c(1, 2))
}

# Check for normality
check_normality(draws[, , "beta[1]"])
check_normality(draws[, , "alpha"])
check_normality(draws[, , "sigma"])


### Create the value -----------------------------------


# Predict the data
predict_data_based <- function(tfr_female,
                               alpha= median(draws[, , "alpha"]),
                               beta = median(draws[, , "beta[1]"]),
                               sigma = median(draws[, , "sigma"]),
                               lower = TRUE) {
  if (lower) {
    return(exp(alpha + beta * log(tfr_female)) + qnorm(0.1, sd=sigma))
  } else {
    #return(alpha[1] + beta * tfr_female + sigma)
    return(exp(alpha + beta * log(tfr_female)) + qnorm(0.9, sd=sigma))
  }
}


# Estimate the diciles for the country-level data
lower_th <- predict_data_based(tfr_female = fert$tfr_female, lower = TRUE)
upper_th <- predict_data_based(tfr_female = fert$tfr_female, lower = FALSE)
fert$data_based_approach <- factor(ifelse(fert$tfr_ratio < lower_th | fert$tfr_ratio > upper_th, 1, 0), 
                                     labels = c("no squeeze", "birth squeeze"))


# Store the results data
save(draws, file="results/tfr_ratio_draws.Rda")

### END ################################################
