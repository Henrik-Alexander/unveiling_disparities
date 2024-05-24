#### Functions from ....

# Rowise Kronecker product
rowwisekr <- function (X1, X2){
  one.1 <- matrix(1, 1, ncol(X1))
  one.2 <- matrix(1, 1, ncol(X2))
  kronecker(X1, one.2) * kronecker(one.1, X2)
}

# Create the baseline splines
Bspline_MM <- function(coord, k = 15, deg = 3, ord_D = 2){
  #Creates from 2D coordinates B-splines and then applies SVD
  #on penality matrix to construct mixed model representation
  #of the coordinates.
  #
  #Args:
  # coord: 2D matrix, with x- and y-coordinates.
  # k: Number of knots.
  # deg: Degree of each polynomial spline.
  # ord_D: Order of difference matrix.
  #
  #Creating the B-splines
  coord1.min <- min(coord[, 1])
  coord1.max <- max(coord[, 1])
  d1 <- (coord1.max - coord1.min)/(k - deg)
  knots1 <- seq(from = coord1.min - deg * d1, to = coord1.max + deg * d1, by = d1)
  B1 <- splines::splineDesign(x = coord[, 1], knots = knots1, ord = deg + 1,
                              outer.ok = TRUE, sparse = TRUE) #Jxk
  #
  coord2.min <- min(coord[, 2])
  coord2.max <- max(coord[, 2])
  d2 <- (coord2.max - coord2.min)/(k - deg)
  knots2 <- seq(from = coord2.min - deg * d2, to = coord2.max + deg * d2, by = d2)
  B2 <- splines::splineDesign(x = coord[, 2], knots = knots2, ord = deg + 1,
                              outer.ok = TRUE, sparse = TRUE) #Jxk
  #
  #Constructing difference matrix
  D <- diff(diag(k), diff = ord_D) #(k - ord_D) x k
  #
  #SVD of P and constructing X and Z for two dimensions
  P <- t(D)%*%D #kxk
  eigen.P <- eigen(P)
  U <- eigen.P$vectors[, 1:(k - ord_D)] #kx(k - ord_D), non-null eigenvectors
  Sigma <- eigen.P$values[1:(k - ord_D)] #(k - ord_D), non-null eigenvalues
  #
  # Reparametrisation - Mixed model decomposition.
  L <- U %*% diag(sqrt(Sigma))
  Z1 <- B1 %*% L %*% solve(t(L) %*% L) #Jx(k - ord_D)
  Z2 <- B2 %*% L %*% solve(t(L) %*% L) #Jx(k - ord_D)
  #
  X_mat <- cbind(rep(1, k), 1:k) #kxp
  X1 <- B1 %*% X_mat #Jxp
  X2 <- B2 %*% X_mat #Jxp
  #Z <- B %*% t(D) %*% solve(D %*% t(D)) #Jxq
  #
  #Constructing X and Z with row-wise Kronecker product
  X <- rowwisekr(X2, X1)
  Z <- cbind(rowwisekr(Z2, X1), rowwisekr(X2, Z1), rowwisekr(Z2, Z1))
  #
  #Results
  results <- list()
  results$X <- X
  results$Z <- Z
  return(results)
}


## Function of composite linke model
PCLMM <- function(X, Z, y, interval, offset, C_index = FALSE){
  #Applies a penalized composite link mixed model to Poisson data.
  #
  #Args:
  # X: Vector with fixed effects from mixed model representation
  # of B-spline coordinates, de-aggregated.
  # Z: Vector with random effects from mixed model representation
  # of B-spline coordinates, de-aggregated.
  # y: The response variable in aggregated form.
  # interval: Vector containing number of interval of each coordinate.
  # offset: Vector containing offest of eta.
  # C_index: Default FALSE. If TRUE I == J and composite matrix reduces
  # to the identity matrix.
  #
  #Obtaining initial parameters
  I <- nrow(y) #n
  J <- nrow(X)
  q <- ncol(Z)
  y <- as.matrix(y, ncol = 1)
  #
  if (C_index == FALSE){
    #Constructing composite matrix
    C <- t(model.matrix(~ interval - 1)) #IxJ
    C <- Matrix(C, sparse = TRUE)
    #
    #Obtaining initial estimates
    row_C <- rowSums(C)
    mean_row <- y / row_C
    y_long <- rep(mean_row, row_C) #Vector of J
  } else {
    #Creating identity composite matrix
    C <- .sparseDiagonal(I)
    #
    y_long <- y
  }
  fit <- glm(y_long ~ 0 + as.matrix(X))
  #
  #Setting up preliminaries for loop
  dif <- 10
  theta <- c(0)
  beta_old <- fit$coefficients #Vector of p
  u_old <- c(rep(0, q)) #Vector of q
  eta_init <- fit$fitted.values #Jx1
  gamm <- exp(eta_init + offset) #Jx1
  Gamm <- .sparseDiagonal(n = J, x = as.vector(gamm)) #JxJ
  #
  H <- numeric(1)
  s <- numeric(1)
  #
  mu <- C %*% gamm #Ix1
  mu <- Matrix(mu, sparse = TRUE)
  W <- .sparseDiagonal(n = I, x = as.vector(mu)) #IxI
  Winv <- .sparseDiagonal(n = I, x = (1 / as.vector(mu))) #IxI
  #
  X_breve <- Winv %*% C %*% Gamm %*% X #Ixp
  Z_breve <- Winv %*% C %*% Gamm %*% Z #Ixq
  z <- X_breve %*% beta_old + Z_breve %*% u_old + Winv %*% (y - mu) #Ix1
  #
  G <- .sparseDiagonal(n = q, x = theta) #qxq
  ZGZ <- Z_breve %*% G %*% t(Z_breve) #IxI
  V <- Winv + ZGZ #IxI
  V <- Matrix(V, sparse = TRUE)
  V_inv <- solve(V) #IxI
  #
  while(any(dif > 1e-6)){
    #Estimating Coefficients
    beta <- solve(t(X_breve) %*% V_inv %*% X_breve) %*% t(X_breve) %*% V_inv %*% z #p
    u <- G %*% t(Z_breve) %*% V_inv %*% (z - X_breve %*% beta_old) #q
    eta <- X %*% beta + Z %*% u #Jx1
    #
    #Applying Newton-Raphson to calculate theta
    P <- V_inv - V_inv %*% X_breve %*% solve(t(X_breve) %*% V_inv %*% X_breve) %*%
      t(X_breve) %*% V_inv
    #
    #Hessian matrix
    H <- (1/2) %*% sum(diag((t(Z_breve) %*% P %*% Z_breve %*% t(Z_breve)
                             %*% P %*% Z_breve))) - t(z) %*% P %*% Z_breve %*% t(Z_breve) %*%
      P %*% Z_breve %*% t(Z_breve) %*% P %*% z
    #Score vector
    s <- - (1/2) %*% sum(diag((t(Z_breve) %*% P %*% Z_breve))) + (1/2) %*%
      (t(z) %*% P %*% Z_breve %*% t(Z_breve) %*% P %*% z)
    #
    #Calculating variance-covariance parameters
    theta_new <- as.numeric(theta - solve(H) * s) #1
    gamm <- as.numeric(exp(eta + offset)) #J
    Gamm <- .sparseDiagonal(n = J, x = as.vector(gamm)) #JxJ
    mu <- C %*% gamm #Ix1
    mu <- Matrix(mu, sparse = TRUE)
    W <- .sparseDiagonal(n = I, x = as.vector(mu)) #IxI
    Winv <- .sparseDiagonal(n = I, x = (1 / as.vector(mu))) #IxI
    #
    X_breve <- Winv %*% C %*% Gamm %*% X #Ixp
    Z_breve <- Winv %*% C %*% Gamm %*% Z #Ixq
    z <- X_breve %*% beta + Z_breve %*% u + Winv %*% (y - mu) #Ix1
    #
    G <- .sparseDiagonal(n = q, x = theta_new) #qxq
    ZGZ <- Z_breve %*% G %*% t(Z_breve) #IxI
    V <- Winv + ZGZ #IxI
    V <- Matrix(V, sparse = TRUE)
    V_inv <- solve(V) #IxI
    #
    #Updating Parameters
    dif <- abs(beta - beta_old)
    beta_old <- beta
    theta <- theta_new
  }
  # Combine the results
  results <- list()
  results$beta <- beta
  results$u <- u
  results$gamma <- gamm
  results$mu <- mu
  results$eta <- eta
  return(results)
  
}
