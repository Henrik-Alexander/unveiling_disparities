## Run the different code files

# Install the packages
install <- FALSE
if(install) {
  source("code/install_packages.R")
}


# Get the code files
code_files <- list.files("code", pattern = "\\d.+R$", full.names=T)

# Run the files with commeting
for (i in seq_along(code_files)) {
  cat("---------------------\n")
  cat("File:", i, "\n")
  
  # Run the fill
  source(code_files[i])
  
}

### END #############################