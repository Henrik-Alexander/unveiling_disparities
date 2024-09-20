# Estimate slope
slope <- function(x2, y2, x1, y1) {
  (y2 - y1) / (x2 - x1)
}

# Estimate Intercept
intercept <- function(y, x, beta) {
  y - x * beta
}

# Containers
intercepts_05 <- intercepts_95 <- slopes_05 <- slopes_95 <- numeric(length = nrow(fert))

# Create the results
for (j in 1:nrow(fert)) {
  beta05 <- slopes_05[j] <- slope(fert$tfr_female[1], pp$`5%`[1],
                        pp$tfr_female[j], pp$`5%`[j])
  intercepts_05[j] <- intercept(pp$`5%`[j], fert$tfr_female[j], beta05)
  beta95 <- slopes_95[j] <- slope(fert$tfr_female[1], pp$`95%`[1],
                                  pp$tfr_female[j], pp$`95%`[j])
  intercepts_95[j] <- intercept(pp$`95%`[j], fert$tfr_female[j], beta95)
}

# Estimate the intercepts
beta05 <- mean(slopes_05, na.rm = T)
beta95 <- mean(slopes_95, na.rm = T)
intercept_05 <- mean(intercepts_05, na.rm = T)
intercept_95 <- mean(intercepts_95, na.rm = T)


ggplot(pp) +
  geom_line(aes(tfr_female, `5%`), col = "blue", alpha = 0.2) +
  geom_line(aes(tfr_female, `95%`), col = "red", alpha = 0.2) +
  geom_abline(aes(intercept = intercept_05, slope = beta05), col = "blue") +
  geom_abline(aes(intercept = intercept_95, slope = beta95), col = "red")
