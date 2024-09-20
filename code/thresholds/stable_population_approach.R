#######################################
# Project: Subnational birth squeezes #
# Purpose: Stable Population Approach #
# Author: Henrik-Alexander Schubert   #
# E-mail: schubert@demogr.mpg.de      #
# Date: 20.09.2024                    #
#######################################


# Create data for the normal range
stable_pop_normal <-  expand.grid(age_diff = seq(2, 4, by = 0.2),
                                  pop_diff = seq(0.915, 1.085, by = 0.005),
                                  srb = seq(1.04, 1.06, by = 0.01),
                                  r = seq(-0.025, 0.025, by = 0.025))
stable_pop_normal$tfr_ratio <- with(stable_pop_normal, 1/srb * pop_diff * exp(r*(age_diff)))
hist(stable_pop_normal$tfr_ratio,
     main = "TFR ratio in stable population using plausible values",
     col="black", border="white", breaks=20)

# Function
stable_pop <- function(variable = "r", data = stable_pop_normal) {
  tmp <- data[data[[variable]] %in% range(data[[variable]]), ]
  print(variable)
  tapply(tmp$tfr_ratio, tmp[[variable]], function(x) round(range(x), 2))
}

# Apply the function
vars <- c("srb", "pop_diff", "age_diff", "r")
lapply(vars, FUN = stable_pop)

# Create the birth squeeze variable
stable_pop_th <- range(stable_pop_normal$tfr_ratio)
fert$stable_pop_approach <- factor(ifelse(fert$tfr_ratio < stable_pop_th[1] | fert$tfr_ratio > stable_pop_th[2], 1, 0), 
                                   labels = c("no squeeze", "birth squeeze"))

### Extreme values -----------------------



## Function to create extreme with plausible values -------------

# Create data for the extreme range
stable_pop_extreme <-  stable_pop_normal
stable_pop_extreme$tfr_ratio <- with(stable_pop_extreme, 1/srb * pop_diff * exp(r*(age_diff)))

# Create the impact of extreme events
impact_extreme <- function(values, variable, normal = stable_pop_extreme) {
  if (any(str_detect(names(normal), "tfr_ratio"))) {
    normal <- normal[, !(names(normal) == "tfr_ratio")]
  }
  
  # Create the normal ranges
  ranges <- apply(normal, 2, range, simplify = T)
  ranges[, variable] <- values
  
  # Create the results
  results <- expand.grid(
    age_diff = ranges[, "age_diff"],
    pop_diff = ranges[, "pop_diff"],
    srb = ranges[, "srb"],
    r = ranges[, "r"]
  )
  results$tfr_ratio <- with(results, 1/srb * pop_diff * exp(r*(age_diff)))
  tapply(results$tfr_ratio, results[[variable]], function(x) range(round(x, 3)))
}

# Create impact of extreme values
impact_extreme(c(1, 1.18), "srb")
impact_extreme(c(0.56, 3.9), "pop_diff")
impact_extreme(c(1.9, 17), "age_diff")
impact_extreme(c(-0.5, .5), "r")

# Function to obatin the range of TFR values
obtain_range <- function(variable = "srb", data = stable_pop_extreme){
  tmp_range <- range(data[[variable]])
  tmp <- data[data[[variable]] == tmp_range[1] | data[[variable]] == tmp_range[2], ]
  tapply(tmp$tfr_ratio, tmp[[variable]], function(x) round(range(x), 2))
}


# Create the range
map(c("srb", "pop_diff", "age_diff", "r"), obtain_range)
