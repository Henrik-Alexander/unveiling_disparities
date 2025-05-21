# Population projection
n_age_groups <- nrow(A)
n_projections <- 1000
K <- matrix(0, nrow = n_age_groups, ncol = n_projections+1)
K[, 1] <- rep(100000, length = n_age_groups)

# Do the projections
for (i in 2:(n_projections+1)) {
  K[, i] <- A %*% K[, i-1]
}

# Project
names(K) <- seq(2020, 2020+n_projections, by = 1)
rownames(K) <- 1:nrow(K)
#K <- apply(K, 2, function(x) x / sum(x))

# Create a data frame
data <- reshape2::melt(K)
names(data) <- c("age", "year", "population")

# Plot the distribution
ggplot(data, aes(year, population, colour = age, group = age)) +
  geom_line() +
  scale_colour_viridis_c()

### END ##################################