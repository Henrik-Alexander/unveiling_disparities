#######################################
# Project: Subnational birth squeezes #
# Purpose: Impact approach            #
# Author: Henrik-Alexander Schubert   #
# E-mail: schubert@demogr.mpg.de      #
# Date: 20.09.2024                    #
#######################################

# Create the formal
impact_approach <- function(mu=0.05, alpha=-1.1363211, beta=0.3472586, r=1) {
  if (abs(mu) > 1){
    warning("Childlessness cannot be lower than 0!")
  }
  
  # Subfunctions
  estimate_l <- function(alpha, beta, r) {
    exp(alpha + beta * r)
  }
  
  # Estimate parameters
  l <- estimate_l(alpha, beta, r)
  
  # Estimate the result
  den <- log((-l-mu*l-mu)/(mu-1+mu*l)) - alpha - beta * r
  return(den / beta)
}

# Estimate the result
impact_approach <- 1 + sapply(c(-0.01, 0.01), impact_approach)


# Create the birth squeeze variable
fert$outcome_based_approach <- factor(
  ifelse(fert$tfr_ratio < impact_approach[1] | fert$tfr_ratio > impact_approach[2], 1, 0), 
  labels = c("no squeeze", "birth squeeze"))