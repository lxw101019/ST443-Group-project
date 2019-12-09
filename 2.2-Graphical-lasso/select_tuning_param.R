library(glasso)
library(qgraph)

p <- c(rep(20,6), rep(50,6), rep(100,4))
n <- c(rep(c(100, 100, 1000, 1000, 10000, 10000),2),1000,1000,10000,10000)
rates <- rep(c("TPR", "FPR"), 8)
optRho <- c(0.1692,0.1692,0.0415,0.0415,0.0115,0.0115,0.2054,0.2054,0.0465,0.0465,0.0121,0.0121,0.0480,0.0480,0.0129,0.0129)
ratedf <- data.frame(p = p, n = n, optimalrates = rates, optRho = optRho)
for(i in 5:54){
  ratedf[,i] <- NA 
}

for(i in 1:8){
  pi <- ratedf$p[2*i]
  ni <- ratedf$n[2*i]
  rhoi <- ratedf$optRho[2*i]
  for(j in 1:50){
    sampleData <- simulation(p = pi, n = ni)
    trueTheta <- sampleData$standardtheta
    sAll <- cov(sampleData$data)
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
write.csv(ratedf,'50repeatsKfold.csv')
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
kfold <- read.csv('50repeatsKfold.csv')
kfold <- kfold[,-1]
for(i in 1:16){
  cat('For p = ' ,kfold[i,1],' and n = ' ,kfold[i,2],' ')
  cat('mean = ' ,mean(t(kfold[i,c(5:54)])),' ')
  cat('sd = ' ,sd(t(kfold[i,c(5:54)])))
  print('')
}





