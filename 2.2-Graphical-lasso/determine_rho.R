determineRho <- function(p,n){
  sampleData <- simulation(p = p, n = n)
  trainIndex <- sample(seq(nrow(sampleData$data)), floor(nrow(sampleData$data))/2, replace=FALSE)
  train <- sampleData$data[trainIndex,]
  test <- sampleData$data[-trainIndex,]
  sTrain <- cov(train)
  sTest <- cov(test)  # Empirical Covariance Matrix
  ll <- data.frame(rho = vector(length=500), logl = vector(length = 500))
  count <- 0
  
  for(rhos in seq(0.001,0.5,0.001)){
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
  optRho.5.10[i] <- determineRhoK(5,10)
  print(i)
}

optRho.5.100 <- vector(length = 100)
for(i in 1:100){
  optRho.5.100[i] <- determineRhoK(5,100)
  print(i)
}

optRho.5.1000 <- vector(length = 100)
for(i in 1:100){
  optRho.5.1000[i] <- determineRhoK(5,1000)
  print(i)
}

optRho.5.10000 <- vector(length = 100)
for(i in 1:100){
  optRho.5.10000[i] <- determineRhoK(5,10000)
  print(i)
}

optRho5 <- data.frame('5,10' = optRho.5.10, '5,100' = optRho.5.100,
                      '5,1000' = optRho.5.1000, '5,10000' = optRho.5.10000)

write.csv(optRho5, file = "optRho5.csv")

library(glasso)
optRho.20.100 <- vector(length = 100)
for(i in 1:100){
  optRho.20.100[i] <- determineRhoK(20,100)
  print(i)
}

optRho.20.1000 <- vector(length = 100)
for(i in 1:100){
  optRho.20.1000[i] <- determineRhoK(20,1000)
  print(i)
}

optRho.20.10000 <- vector(length = 100)
for(i in 1:100){
  optRho.20.10000[i] <- determineRhoK(20,10000)
  print(i)
}

optRho20 <- data.frame('20,100' = optRho.20.100,
                      '20,1000' = optRho.20.1000, '20,10000' = optRho.20.10000)
write.csv(optRho20, file = 'optRho20.csv')



optRho.50.100 <- vector(length = 25)
for(i in 1:25){
  optRho.50.100[i] <- determineRhoK(50,100)
  print(i)
}

optRho.50.1000 <- vector(length = 25)
for(i in 1:25){
  optRho.50.1000[i] <- determineRhoK(50,1000)
  print(i)
}

optRho.50.10000 <- vector(length = 25)
for(i in 1:25){
  optRho.50.10000[i] <- determineRhoK(50,10000)
  print(i)
}

optRho50 <- data.frame('50,100' = optRho.50.100,
                       '50,1000' = optRho.50.1000, '50,10000' = optRho.50.10000)
write.csv(optRho50, file = 'optRho50.csv')

# Some observations: p = 5 completely useless model. p = 20 works well except for n = 10
# p = 50 works well except for n = 10

optRho.100.1000 <- vector(length = 25)
for(i in 1:25){
  optRho.100.1000[i] <- determineRhoK(100,1000)
  print(i)
}

optRho.100.10000 <- vector(length = 25)
for(i in 1:25){
  optRho.100.10000[i] <- determineRhoK(100,10000)
  print(i)
}

optRho100 <- data.frame('100,1000' = optRho.100.1000,
                       '100,10000' = optRho.100.10000)

write.csv(optRho100, file = 'optRho100.csv')




