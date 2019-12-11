dfAll <- read.csv('50repeatsKfoldwithMCE.csv')
dfAll <- dfAll[,-1]

dfp20n100bp <- data.frame(rate = vector(length=150),type = vector(length=150))
dfp20n1000bp <- data.frame(rate = vector(length=150),type = vector(length=150))
dfp20n10000bp <- data.frame(rate = vector(length=150),type = vector(length=150))
dfp50n100bp <- data.frame(rate = vector(length=150),type = vector(length=150))
dfp50n1000bp <- data.frame(rate = vector(length=150),type = vector(length=150))
dfp50n10000bp <- data.frame(rate = vector(length=150),type = vector(length=150))
dfp100n1000bp <- data.frame(rate = vector(length=150),type = vector(length=150))
dfp100n10000bp <- data.frame(rate = vector(length=150),type = vector(length=150))

library(tidyr)
dfp20n100bp$type <- c(rep("TPR",50),rep("FPR",50),rep("MCE",50))
for(i in 1:150){
  if (i > 100){
    dfp20n100bp$rate[i] <- dfAll[3,i+4-100]
  } else if (i > 50){
    dfp20n100bp$rate[i] <- dfAll[2,i+4-50]
  } else {
    dfp20n100bp$rate[i] <- dfAll[1,i+4]
  }
}
library(tidyverse)
dfBP <- read.csv('transformWen.csv')
dfBP <- dfBP[,-1]
dfBP <- dfBP[,-4]
dfBP$p <- as.factor(dfBP$p)
dfBPn100 <- filter(dfBP, n == 100)
dfBPn1000 <- filter(dfBP, n == 1000)
dfBPn10000 <- filter(dfBP, n == 10000)


#TPR
pn100TPR <- ggplot(dfBPn100, aes(x=method, y=TPR)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 100") + ylim(0,1)
pn1000TPR <- ggplot(dfBPn1000, aes(x=method, y=TPR)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 1000") + ylim(0,1)
pn10000TPR <- ggplot(dfBPn10000, aes(x=method, y=TPR)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 10000") + ylim(0,1)


#FPR
pn100FPR <- ggplot(dfBPn100, aes(x=method, y=FPR)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 100") + ylim(0,0.5)
pn1000FPR <- ggplot(dfBPn1000, aes(x=method, y=FPR)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 1000") + ylim(0,0.5)
pn10000FPR <- ggplot(dfBPn10000, aes(x=method, y=FPR)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 10000") + ylim(0,0.5)

#MCE
pn100MCE <- ggplot(dfBPn100, aes(x=p, y=MCE)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 100") + ylim(0,0.5)
pn1000MCE <- ggplot(dfBPn1000, aes(x=p, y=MCE)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 1000") + ylim(0,0.5)
pn10000MCE <- ggplot(dfBPn10000, aes(x=p, y=MCE)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 10000") + ylim(0,0.5)

library(gridExtra)
grid.arrange(pn100TPR, pn1000TPR, pn10000TPR, ncol = 3)
gTPR <- arrangeGrob(pn100TPR, pn1000TPR, pn10000TPR, ncol = 3) #generates g
ggsave(file="TPRbp.pdf", gTPR)

grid.arrange(pn100FPR, pn1000FPR, pn10000FPR, ncol = 3)
gFPR <- arrangeGrob(pn100FPR, pn1000FPR, pn10000FPR, ncol = 3) #generates g
ggsave(file="FPRbp.pdf", gFPR)

grid.arrange(pn100MCE, pn1000MCE, pn10000MCE, ncol = 3)
gMCE <- arrangeGrob(pn100MCE, pn1000MCE, pn10000MCE, ncol = 3) #generates g
ggsave(file="MCEbp.pdf", gMCE)





