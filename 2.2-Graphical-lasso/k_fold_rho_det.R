determineRhoK <- function(p,n){
  sampleData <- simulation(p = p, n = (3/2)*n)
  dat <- sampleData$data
  ll <- data.frame(rho = vector(length=300), logl1 = vector(length = 300), 
                   logl2 = vector(length = 300), logl3 = vector(length = 300))
  k <- 3 
  folds <- sample(rep(1:k, length=n))
  
  for(ki in 1:3){
    sTrain <- cov(dat[which(folds!=ki),])
    sTest <- cov(dat[which(folds==ki),])
    count <- 0
    for(rhos in seq(0.001,0.3,0.001)){
      count <- count + 1
      glassoOutput <- glasso(sTrain, rho = rhos)
      modelPrecMat <- glassoOutput$wi
      ll[count,ki + 1] <- log(det(modelPrecMat)) - sum(diag(sTest %*% modelPrecMat))
      ll[count, 1] <- rhos
    }
  }
  ll$mean <- rowMeans(ll[,c(2:4)])
  index <- which.max(ll$mean)
  rhoBest <- ll$rho[index]
  return(rhoBest)
}
