#######################################
# Project: Subnational birth squeezes #
# Purpose: Expert-based approach      #
# Author: Henrik-Alexander Schubert   #
# E-mail: schubert@demogr.mpg.de      #
# Date: 20.09.2024                    #
#######################################

# Compare the different datas-ource
distr_tfr_ratio <- tapply(fert$tfr_ratio, fert$data,
                          function(x) quantile(x, probs = c(0.1, 0.5,  0.9)), simplify = F)
distr_tfr_ratio <- array2DF(distr_tfr_ratio)
distr_tfr_ratio$N <- as.numeric(tapply(fert$tfr_ratio, fert$data, length))
distr_tfr_ratio <- distr_tfr_ratio[, c("Var1", "N", "10%", "50%", "90%")]
stargazer(distr_tfr_ratio, summary = F)

# Estimate the diciles for the country-level data
country_fert <- fert[fert$data != "Subnational Fertility Data", ]
review_threshold <- round(quantile(country_fert$tfr_ratio, probs = c(0.1, 0.9)), 3)
fert$expert_based_approach <- factor(ifelse(fert$tfr_ratio < 0.9 | fert$tfr_ratio > 1.1, 1, 0), 
                                     labels = c("no squeeze", "birth squeeze"))


### END ###########################################