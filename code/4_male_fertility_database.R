########################################
# Purpose: Male frrtility data         #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################


#rm(list = ls()); gc(T)

library(stringr)

# Load USA data
load("data/male_national_fertility.Rda")

# Filter countries
hfc <- ccd[ccd$data == "Human Fertility Collection", ]

# Create dimensions
countries <- unique(hfc$country)
cols <- viridis::cividis(length(countries))

par(family = "serif", mar = c(3, 4, 2, 0.2), lwd = 2)

### Create the plot
plot(NA, 
     xlim = range(hfc$year),
     ylim = range(hfc$tfr_ratio),
     axes = FALSE, ann = F,
     xaxs = "i")
abline(h = 1)

for (i in seq_along(countries)) {
  tmp <- hfc[hfc$country == countries[i], ]
  lines(tmp$year, tmp$tfr_ratio, col = cols[i], lwd = 2)
  points(tmp$year, tmp$tfr_ratio, col=cols[i], pch=16)
  text(tmp$year[1]+1, tmp$tfr_ratio[1]+0.005, labels=unique(tmp$country), col=cols[i])
}

# Axis
axis(1, seq(1960, 2030, by = 10), lwd = 2)
axis(1, seq(1960, 2030, by = 5), labels = F)
axis(2, seq(0, 3, by = 0.05), lwd = 2, las = 1)
#axis(2, seq(0, 3, by = 0.05), labels = F)

# Annotations
title(main = "TFR ratio", line = .5, cex.main = 2)
title(xlab = "Year", line = 2, cex.lab = 1.5, font.lab = 2)
title(ylab = "TFR men / TFR women", line = 2.5, cex.lab = 1.5, font.lab = 2)

# Plot the same for the schoumaker data ---------------------

# Extract the Schoumaker's data
schoumaker <- ccd[ccd$data=="Schoumaker's fertility estimates", ]

### END ##################################################