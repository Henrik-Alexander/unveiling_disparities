# Function to define Leslie matrix, based on nLx and nFx values
rates_to_leslie <- function(nLx, nFx, n_age_groups=10, sex = "f"){
  ffab <- 0.4886
  if (sex == "m") ffab <- 1 - ffab
  L <- matrix(0, nrow = n_age_groups, ncol = n_age_groups)
  L[1,] <- ffab * nLx[1]*(nFx[1:n_age_groups]+nFx[2:(n_age_groups+1)]*nLx[2:(n_age_groups+1)]/nLx[1:n_age_groups])/2 # top row 
  diag(L[2:n_age_groups,1:(n_age_groups-1)]) <- nLx[2:n_age_groups] / nLx[1:(n_age_groups-1)] # subdiagonal
  return(L)
}

# Function
create_leslie <- function(data) {
  if(nrow(data)==0) {
    return(NULL)
  } else {
  # Filter only ages
  data <- data[!is.na(data$Age), ]
  data$ASFR[is.na(data$ASFR)] <- 0
  
  # Create the number of age groups
  A <- rates_to_leslie(data$Lx, data$ASFR, n_age_groups = nrow(data))
  A[is.na(A)] <- 0
  return(A)
  }
}

# Estimate the long-term growth rate
stable_pop_growth <- function(A) {
  if(is.matrix(A)) {
    log(Re(eigen(A)$values[1]))
  } else {
    return(NA)
  }
}

# Stable population age structure
stable_pop_age <- function(A) {
  if(is.matrix(A)) {
    log(Re(eigen(A)$vectors[, 1]))
  } else {
    return(NA)
  }
}

### END #########################################