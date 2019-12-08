determineRho <- function(p,n){
  sampleData <- simulation(p = p, n = n)
  trainIndex <- sample(seq(nrow(sampleData$data)), floor(nrow(sampleData$data))/2, replace=FALSE)
  train <- sampleData$data[trainIndex,]
  test <- sampleData$data[-trainIndex,]
  sTrain <- cov(train)
  sTest <- cov(test)  # Empirical Covariance Matrix
  ll <- data.frame(rho = vector(length=1001), logl = vector(length = 1001))
  count <- 0
  
  for(rhos in seq(0,1,0.001)){
    count <- count + 1
    glassoOutput <- glasso(sTrain, rho = rhos)
    modelPrecMat <- glassoOutput$wi
    ll$logl[count] <- log(det(modelPrecMat)) - sum(diag(sTest %*% modelPrecMat))
    ll$rho[count] <- rhos
  }
  
  index <- which.max(ll$logl)
  rhoBest <- ll$rho[index]
  return(rhoBest)
}

optRho.5.10 <- vector(length = 100)
for(i in 1:100){
  optRho.5.10[i] <- determineRho(5,10)
  print(i)
}


# Some observations: p = 5 completely useless model. p = 20 works well except for n = 10
# p = 50 works well except for n = 10