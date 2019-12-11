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
#EBICgraph <- qgraph(glassoBest$wi - diag(diag(glassoBest$wi)), layout = "spring", title = "EBIC")

# log det theta - trace(S theta)
write.csv(ratedf,'50repeatsKfoldwithMCE.csv')
#library(tibble)
#library(ggplot2)
#library(dplyr)
#
#p20 <- as_tibble(t(ratedf[c(1:6),c(5:54)]) )
#colnames(p20) <- c("p = 20, n = 100, TPR","p = 20, n = 100, FPR","p = 20, n = 1000, TPR",
#                   "p = 20, n = 1000, FPR","p = 20, n = 10000, TPR","p = 20, n = 10000, FPR")
#boxplot(p20$`p = 20, n = 100, TPR`)
#ggplot(ddata = p20) + geom_boxplot(aes(x= "p = 20, n = 100, TPR",y= ""))
#
#male <-  as_tibble(c(127,44,28,83,0,6,78,6,5,213,73,20,214,28,11)) # data from page 66
#ggplot(data = male, aes(x = "", y = male)) + 
#  geom_boxplot() +
#  coord_cartesian(ylim = c(0, 150))

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


