########################################
# Purpose: Reviewer comments           #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

library(data.table)
library(tidyverse)

# Load the graphics package
source("functions/Graphics.R")

# Reviewer 1 ===================================================================



# Reviewer 2 ===================================================================

# R2 C1 ------------------------------------------------------------------------

# Purpose: Look at the subnational and national data
# Aggregate the subnational data



# Load the subnational data
load("data/fert_data_subnational.Rda")
fert_subnat <- as.data.table(fert)


# Load the national data
fert_nat <- fread("data/fert_data_national.csv")

# Aggregate the subnational data
fert_agg_subnat <- fert_subnat[, .(tfr_ratio = mean(tfr_male)/mean(tfr_female),
                            tfr_male = mean(tfr_male),
                            tfr_female = mean(tfr_female)), by = .(country, year)]


# Merge the national data with the subnational data
fert <- merge(fert_nat, fert_agg_subnat, by= c("country", "year"), suffixes = c("_national", "_subnational"))


# Plot the difference



# R2 C4 ------------------------------------------------------------------------

# Define the coordinates
min_x <- 0.5
max_x <- 1.1
min_y <- 0.5
max_y <- 1.1

# Define the position of the subplot window
min_y_subplot <- 3
max_y_subplot <- 8
min_x_subplot <- 0.55
max_x_subplot <- 1.7


# Replot the Figure 2
main <- ggplot(fert_subnat, aes(x=tfr_female, y=tfr_male, colour = country, shape = country)) +
  geom_rect(aes(xmin = min_x, xmax=max_x, ymin = min_y, ymax=max_y), fill=NA, colour="black") +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(alpha = 0.5, size = 0.5) +
  scale_x_log10("TFR female", n.breaks = 15) +
  scale_y_log10("TFR male", n.breaks = 15) +
  scale_colour_viridis_d("Country") +
  scale_shape_manual("Country", values = 13:20) +
  theme(legend.position = c(0.8, 0.2)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  geom_segment(aes(x = min_x, y = max_y, xend = min_x_subplot, yend = min_y_subplot), colour = "grey") +
  geom_segment(aes(x = max_x, y = max_y, xend = max_x_subplot, yend = min_y_subplot), colour = "grey")
  


# Create the subplot
subplot_bl <- ggplot(fert_subnat, aes(x=tfr_female, y=tfr_male, colour = country, shape = country)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(alpha = 0.5, size = 2) +
  scale_x_log10("", breaks = c(0.6, 0.7, 0.8, 0.9, 1), expand = c(0, 0)) +
  scale_y_log10("", breaks = c(0.6, 0.7, 0.8, 0.9, 1), expand = c(0, 0)) +
  scale_colour_viridis_d("Country") +
  scale_shape_manual("Country", values = 13:20) +
  coord_cartesian(xlim = c(min_x, max_x), ylim = c(min_y, max_y)) +
  guides(colour = "none", shape = "none") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #panel.border = element_blank(),
    axis.ticks = element_blank(),
    margins = margin(0, 0, 0, 0),
    plot.margin = margin(0, 0, 0, 0)
  )

# Combine the two graphs
main + annotation_custom(ggplotGrob(subplot_bl),
                         xmin = min_x_subplot, 
                         xmax = max_x_subplot,
                         ymin = min_y_subplot,
                         ymax = max_y_subplot)

# Save the figure
ggsave("figures/tfr_ratios.svg", height=10, width=10)

# Reviewer 3 ===================================================================



### END ########################################################################