library(glasso)
library(qgraph)

p <- c(rep(20,9), rep(50,9), rep(100,6))
n <- c(rep(c(rep(100,3),rep(1000,3), rep(10000,3)),2),rep(1000,3),rep(10000,3))
rates <- rep(c("TPR", "FPR","MCE"), 8)
optRho <- c(rep(0.1692,3),rep(0.0415,3),rep(0.0115,3), rep(0.2054,3),rep(0.0465,3),rep(0.0121,3),rep(0.0480,3),rep(0.0129,3))
ratedf <- data.frame(p = p, n = n, optimalrates = rates, optRho = optRho)
for(i in 5:54){
  ratedf[,i] <- NA 
}

for(i in 1:8){
  pi <- ratedf$p[3*i]
  ni <- ratedf$n[3*i]
  rhoi <- ratedf$optRho[3*i]
  for(j in 1:50){
    sampleData <- simulation(p = pi, n = ni)
    trueTheta <- sampleData$standardtheta
    sAll <- cov(sampleData$data)
    glassoBest <- glasso(sAll, rhoi)
    true <- true_edge(trueTheta)
    modelfit <- true_edge(glassoBest$wi)
    cm <- confusion_matrix(modelfit,true)
    MCE <- (cm$FP + cm$FN)/(cm$FP + cm$FN + cm$TP + cm$TN)
    ratedf[3*i-2,j+4] <- cm$TP_rate
    ratedf[3*i-1,j+4] <- cm$FP_rate
    ratedf[3*i,j+4] <- MCE
  }
}   

#write.csv(ratedf,'50repeatsKfoldwithMCE.csv')

library(readr)
kfold <- read.csv('50repeatsKfoldwithMCE.csv')
kfold <- kfold[,-1]
kfold <- ratedf
for(i in 1:24){
  cat('For p = ' ,kfold[i,1],' and n = ' ,kfold[i,2],' ')
  cat('mean = ' ,mean(t(kfold[i,c(5:54)])),' ')
  cat('sd = ' ,sd(t(kfold[i,c(5:54)])))
  print('')
}

p20n100 <- read.csv('dfp20n100bp.csv')


