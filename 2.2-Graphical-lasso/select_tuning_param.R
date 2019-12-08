library(glasso)
library(qgraph)

p <- c(rep(5,8), rep(20,8), rep(50,8))
n <- rep(c(10, 10, 100, 100, 1000, 1000, 10000, 10000),3)
rates <- rep(c("TPR", "FPR"), 12)
ratedf <- data.frame(p = p, n = n, optimalrates = rates, optRho = optRho)
for(i in 5:54){
  ratedf[,i] <- NA 
}

for(i in 1:12){
  pi <- ratedf$p[2*i]
  ni <- ratedf$n[2*i]
  rhoi <- ratedf$optRho[2*i]
  for(j in 1:50){
    sampleData <- simulation(p = pi, n = ni)
    trueTheta <- sampleData$standardtheta
    #    trainIndex <- sample(seq(nrow(sampleData$data)), floor(nrow(sampleData$data))/2, replace=FALSE)
    #    train <- sampleData$data[trainIndex,]
    #    test <- sampleData$data[-trainIndex,]
    #    sTrain <- cov(train)
    #    sTest <- cov(test)  # Empirical Covariance Matrix
    sAll <- cov(sampleData$data)
    
    #    ll <- data.frame(rho = vector(length=2001), logl = vector(length = 2001))
    #    count <- 0
    #    
    #    for(rhos in seq(0,0.2,0.1)){
    #      count <- count + 1
    #      glassoOutput <- glasso(sTrain, rho = rhos)
    #      modelPrecMat <- glassoOutput$wi
    #      ll$logl[count] <- log(det(modelPrecMat)) - sum(diag(sTest %*% modelPrecMat))
    #      ll$rho[count] <- rhos
    #    }
    #    
    #    index <- which.max(ll$logl)
    #    glassoBest <- glasso(sAll, ll$rho[index])
    
    glassoBest <- glasso(sAll, rhoi)
    true <- true_edge(trueTheta)
    modelfit <- true_edge(glassoBest$wi)
    cm <- confusion_matrix(modelfit,true)
    ratedf[2*i-1,j+4] <- cm$TP_rate
    ratedf[2*i,j+4] <- cm$FP_rate
  }
}   
#EBICgraph <- qgraph(glassoBest$wi - diag(diag(glassoBest$wi)), layout = "spring", title = "EBIC")

# log det theta - trace(S theta)

# p 5, 20, 50, 100
# n 10, 100, 1000, 10000
