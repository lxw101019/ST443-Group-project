library(MASS)
library(matrixcalc)

simulation <- function(p, n){
  #a generate the lower triangle part of the pxp matrix with 10% to be 0.5 and 90% to be 0.
  a <- rbinom(n = p * (p - 1) / 2 , size = 1, prob = 0.1)
  a[a == 1] <- 0.5
  
  #B become the B in the sheet. Diagnal is all 0.
  B <- matrix(0, p, p)
  B[lower.tri(B, diag = FALSE)] <- a
  B[upper.tri(B)] <- t(B)[upper.tri(B)]
  
  #Identity matrix
  I <- diag(x = 1, p, p)
  
  #delta is something I am pretty not sure about. I am confused with the word 'chosen' used in the project
  #guidance paper so I just simply sample one number from 1 to 100 which can make the theta postive 
  #definite. 
  delta <- 5
  
  #theta
  theta = B + delta*I
  
  #standardize the theta
  standard_theta <- cov2cor(theta)
  
  #calculate the inverse of theta
  covMatrix <- solve(standard_theta)
  
  #generate n random samples from a multivariate gaussian distribution with zero mean and the covariance matrix sigma = theta^-1.
  testdata <- mvrnorm(n = n, mu = numeric(p), Sigma = covMatrix, tol = 0, empirical = FALSE, EISPACK = FALSE)
  ls1 <-  list("data" = testdata, "standardtheta" = standard_theta, "theta" = theta)
  
  return(ls1)
}

