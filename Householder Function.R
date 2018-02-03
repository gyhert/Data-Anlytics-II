X <- matrix(c(3, 0, 4, -2, 3, 4), nrow = 3, ncol = 2)
y <- matrix(c(3, 5, 4), nrow = 3, ncol = 1)

# Householder Function

  nr <- length(y)
  nc <- NCOL(X)

  
  j <- 1  
  for (j in seq_len(nc))
{
    id <- seq.int(j, nr) 
    sigma <- sum(X[id,j]^2) 
    s <- sqrt(sigma) 
    diag_ej <- X[j,j] 
    gamma <- 1.0 / (sigma + abs(s * diag_ej)) 
    kappa <- if (diag_ej < 0) s else -s 
    X[j,j] <- X[j,j] - kappa
    if (j < nc)
      for (k in seq.int(j+1, nc))
      {
        yPrime <- sum(X[id,j] * X[id,k]) * gamma
        X[id,k] <- X[id,k] - X[id,j] * yPrime
      }
    yPrime <- sum(X[id,j] * y[id]) * gamma
    y[id] <- y[id] - X[id,j] * yPrime
    X[j,j] <- kappa
  } 
RX <- as.matrix(X[1:ncol(X), ])
Ry <- as.matrix(y[1:ncol(X),])  

  