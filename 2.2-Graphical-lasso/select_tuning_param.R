library(glasso)
library(qgraph)


for(i in 1:50){
  sampleData <- simulation(p = 20, n = 1000)
  trueTheta <- sampleData$standardtheta
  
  trainIndex <- sample(seq(nrow(sampleData$data)), floor(nrow(sampleData$data))/2, replace=FALSE)
  train <- sampleData$data[trainIndex,]
  test <- sampleData$data[-trainIndex,]
  sTrain <- cov(train)
  sTest <- cov(test)  # Empirical Covariance Matrix
  sAll <- cov(sampleData$data)
  
  ll <- data.frame(rho = vector(length=2001), logl = vector(length = 2001))
  count <- 0
  
  for(rhos in seq(0,0.2,0.0001)){
    count <- count + 1
    glassoOutput <- glasso(sTrain, rho = rhos)
    modelPrecMat <- glassoOutput$wi
    ll$logl[count] <- log(det(modelPrecMat)) - sum(diag(sTest %*% modelPrecMat))
    ll$rho[count] <- rhos
  }
  
  index <- which.max(ll$logl)
  glassoBest <- glasso(sAll, ll$rho[index])
  
  true <- true_edge(trueTheta)
  modelfit <- true_edge(glassoBest$wi)
  cm <- confusion_matrix(modelfit,true)
  cm$TP_rate
  cm$FP_rate
}
  

EBICgraph <- qgraph(glassoBest$wi - diag(diag(glassoBest$wi)), layout = "spring", title = "EBIC")

# log det theta - trace(S theta)

# p 5, 20, 50, 100
# n 10, 100, 1000, 10000


