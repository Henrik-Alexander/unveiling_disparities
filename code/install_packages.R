packages <- c("brms", "tidyverse", "readxl", "HMDHFDplus", "patchwork",
              "data.table", "tidyverse", "janitor", "stargazer", "reshape2",
              "texreg", "readxl", "stringr", "ggnewscale", "ggrepel", "latex2exp")

install.packages(packages)

# Install cmdstanr
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))

# Install cmdstan
install_cmdstan()

### END #####################################