########################################
# Purpose: Set the paths for the data  #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.09.2023                     #
########################################

# Set the paths for the countries
path_fin <- "U:/data/fin/birth_registers/tfr_ratios_maakunta.csv"
path_esp <- "U:/data/esp/fertility_rates/asfr_esp.Rda"
path_aus <- "U:/data/aus/fertility_rates/asfr_aus.Rda"
path_fra <- "U:/data/fra/fertility_rates/data/asfr_fra.Rda"
path_deu <- "U:/data/deu/fertility_rates/asfr_deu.Rda"
path_mex <- "U:/data/mex/fertility_rates/asfr_mex.Rda"
path_usa <- "U:/data/usa/fertility_rates/data/asfr_us.Rda"
path_col <- "U:/data/col/fertility_rates/asfr_col.Rda"

# Collect the paths
paths_countries <- mget(ls(pattern = "^path_[a-z]{3}"))

### END ########################################################################