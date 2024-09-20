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


### Data wrangling ----------------------

# Load the data
df <- read.csv("country_male_fertility.csv")

# Define the colours
cols <- sample(colours(10), replace = F)
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
abline(h= 1)
legend("bottomright", legend = unique(df$country), cex = 15)

# Select the columns
df <- df[, c("tfr_ratio", "tfr_female")]

# Remove missing values
df <- na.omit(df)



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
model <- cmdstanr::cmdstan_model("country_regression.stan")

# Set the parameters
parameters <- c("tfr_female")

# Format the data as list for Stan
md <- list(N = nrow(df),
           K = length(parameters),
           y = df$tfr_ratio,
           x = as.matrix(df[, parameters]),
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


### Post estimation analysis -----------------------------

# Make trace plots
trace_style_np(div_color = "green")
bayesplot::mcmc_trace(fit$draws(),
                     pars = c("alpha",
                              "beta[1]",
                              "sigma"))

# Posterior
rhat(fit$draws())

# MCMC sampler diagnostic
fit$diagnostic_summary()

### Explore results -------------------------------

# Summary of the model
pars <- c("alpha", "beta[1]", "sigma")
fit$summary(variables = pars)

# Marginal posterior distribution
bayesplot::mcmc_areas(fit$draws(), pars=pars, prob=0.95)
bayesplot::mcmc_hist(fit$draws(), pars=pars)


### Plot the result

# Create the predict functions
predict_post <- function(x) {
  draws[, , "alpha"] + x * draws[, , "beta[1]"]
}

# Extract the results
draws <- fit$draws()[, , pars]
tfr_females <- seq(1, 5, by = 0.2)
plot(x = rep(tfr_females, each = dim(draws)[1])+rnorm(dim(draws)[1], sd = 0.2), 
     y = sapply(tfr_females, predict),
     col = rgb(0, 0, 0, 0.05),
     pch = 16,
     cex = 0.3,
     xlab = "TFR female",
     ylab = "TFR ratio",
     main = "Posterior prediction results")
abline(a = median(draws[, , "alpha"]),
       b = median(draws[, , "beta[1]"]),
       lwd = 3)


### END ################################################