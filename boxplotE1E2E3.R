library(tidyverse)

dfAll <- read.csv('E1E2E3withMCE.csv')
dfAll <- dfAll[,-1]
dfAll$p <- as.factor(dfAll$p)
dfAllTPR <- filter(dfAll, method == 'cv_min' | method == 'loglikhood')
dfAllFPR <- filter(dfAll, method == 'cv1se' | method == 'loglikhood')

dfAllTPRn100 <- filter(dfAllTPR, n == 100)
dfAllTPRn1000 <- filter(dfAllTPR, n == 1000)
dfAllTPRn10000 <- filter(dfAllTPR, n == 10000)

dfAllFPRn100 <- filter(dfAllFPR, n == 100)
dfAllFPRn1000 <- filter(dfAllFPR, n == 1000)
dfAllFPRn10000 <- filter(dfAllFPR, n == 10000)


#TPR
pn100TPR <- ggplot(dfAllTPRn100, aes(x=Estimated_E, y=TPR)) +
  geom_boxplot(aes(fill = p)) +
  ggtitle("n = 100") + ylim(0,1)
pn1000TPR <- ggplot(dfAllTPRn1000, aes(x=Estimated_E, y=TPR)) +
  geom_boxplot(aes(fill = p)) +
  ggtitle("n = 1000") + ylim(0,1)
pn10000TPR <- ggplot(dfAllTPRn10000, aes(x=Estimated_E, y=TPR)) +
  geom_boxplot(aes(fill = p)) +
  ggtitle("n = 10000") + ylim(0,1)


#FPR
pn100FPR <- ggplot(dfAllFPRn100, aes(x=Estimated_E, y=FPR)) +
  geom_boxplot(aes(fill = p)) +
  ggtitle("n = 100") + ylim(0,0.5)
pn1000FPR <- ggplot(dfAllFPRn1000, aes(x=Estimated_E, y=FPR)) +
  geom_boxplot(aes(fill = p)) +
  ggtitle("n = 1000") + ylim(0,0.5)
pn10000FPR <- ggplot(dfAllFPRn10000, aes(x=Estimated_E, y=FPR)) +
  geom_boxplot(aes(fill = p)) +
  ggtitle("n = 10000") + ylim(0,0.5)

#MCE 
pn100MCE <- ggplot(dfAllFPRn100, aes(x=Estimated_E, y=MCE)) +
  geom_boxplot(aes(fill = p)) +
  ggtitle("n = 100") + ylim(0,0.5)
pn1000MCE <- ggplot(dfAllFPRn1000, aes(x=Estimated_E, y=MCE)) +
  geom_boxplot(aes(fill = p)) +
  ggtitle("n = 1000") + ylim(0,0.5)
pn10000MCE <- ggplot(dfAllFPRn10000, aes(x=Estimated_E, y=MCE)) +
  geom_boxplot(aes(fill = p)) +
  ggtitle("n = 10000") + ylim(0,0.5)

library(gridExtra)
grid.arrange(pn100TPR, pn1000TPR, pn10000TPR, ncol = 3)
gTPR <- arrangeGrob(pn100TPR, pn1000TPR, pn10000TPR, ncol = 3) #generates g
ggsave(file="TPRbpALL.pdf", gTPR)

grid.arrange(pn100FPR, pn1000FPR, pn10000FPR, ncol = 3)
gFPR <- arrangeGrob(pn100FPR, pn1000FPR, pn10000FPR, ncol = 3) #generates g
ggsave(file="FPRbpALL.pdf", gFPR)

grid.arrange(pn100MCE, pn1000MCE, pn10000MCE, ncol = 3)
gMCE <- arrangeGrob(pn100MCE, pn1000MCE, pn10000MCE, ncol = 3) #generates g
ggsave(file="MCEbpALL.pdf", gMCE)

