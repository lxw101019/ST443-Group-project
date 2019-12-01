# Lin: 
# This is the code for simulating multivariate gaussian distribution with zero mean and the covariance matrix sigma = theta^-1.
# Be careful about 'the choice of delta'. Will discuss about the official way to choose delta in the future days.
# 
# Arguments:1. number_of_dimensions: the number of variables/features
#           2. how_many_sets_of_data_you_need: As the name said, the number of rows of data do you want to simulate?
#
# Retutn: 1.testdata: the simulated data in a matrix.
#
simulation <- function(number_of_dimensions, how_many_sets_of_data_you_need){
  
  library(MASS)
  library(matrixcalc)
  #number of dimension
  nod <- number_of_dimensions
  
  #a generate the lower triangle part of the nodxnod matrix with 10% to be 0.5 and 90% to be 0.
  a <- rbinom(n = nod * (nod - 1) / 2 , size = 1, prob = 0.1)
  a[a == 1] <- 0.5
  
  #B become the B in the sheet. Diagnal is all 0.
  B <- matrix(0, nod, nod)
  B[lower.tri(B, diag = FALSE)] <- a
  B[upper.tri(B)] <- t(B)[upper.tri(B)]
  
  #indentity matrix
  I <- diag(x = 1, nod, nod)
  
  #delta is something I am pretty not sure about. I am confused with the word 'chosen' used in the project
  #guidance paper so I just simply sample one number from 1 to 100 which can make the theta postive 
  #definite. 
  delta <- 0
  
  is.positive.definite(B+delta*I, tol=0)
  while (is.positive.definite(B+delta*I, tol=0)==FALSE){delta <- delta + 1}
  
  #theta
  theta = B + delta*I
  
  #standardize the theta
  standard_theta <- cov2cor(theta)
  
  #calculate the inverse of theta
  covMatrix <- solve(standard_theta)
  
  #generate nod random samples from a multivariate gaussian distribution with zero mean and the covariance matrix sigma = theta^-1.
  testdata <- mvrnorm(n = how_many_sets_of_data_you_need, mu = numeric(nod), Sigma = covMatrix, tol = 0, empirical = FALSE, EISPACK = FALSE)
  ls1 <-  list("data" = testdata, "standardtheta" = standard_theta, "theta" = theta)

  return(ls1)
}

x <- simulation(3,100)
x$standardtheta
x$theta
set.seed(10)
