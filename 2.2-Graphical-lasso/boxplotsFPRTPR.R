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



pp20n100 <- ggplot(dfp20n100bp, aes(x=type, y=rate)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 100, p = 20") + ylim(0,1)

pp20n1000 <- ggplot(dfp20n1000bp, aes(x=type, y=rate)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 1000, p = 20") + ylim(0,1)

pp20n10000 <- ggplot(dfp20n10000bp, aes(x=type, y=rate)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 10000, p = 20") + ylim(0,1)

pp50n100 <- ggplot(dfp50n100bp, aes(x=type, y=rate)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 100, p = 50") + ylim(0,1)

pp50n1000 <- ggplot(dfp50n1000bp, aes(x=type, y=rate)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 1000, p = 50") + ylim(0,1)

pp50n10000 <- ggplot(dfp50n10000bp, aes(x=type, y=rate)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 10000, p = 50") + ylim(0,1)

pp100n1000 <- ggplot(dfp100n1000bp, aes(x=type, y=rate)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 1000, p = 100") + ylim(0,1)

pp100n10000 <- ggplot(dfp100n10000bp, aes(x=type, y=rate)) +
  geom_boxplot(color = 'blue') +
  ggtitle("n= 10000, p = 100") + ylim(0,1)

library(gridExtra)
grid.arrange(pp20n100, pp20n1000, pp20n10000, ncol = 3)
g1 <- arrangeGrob(pp20n100, pp20n1000, pp20n10000, ncol = 3) #generates g
ggsave(file="p20bp.pdf", g1)


grid.arrange(pp50n100, pp50n1000, pp50n10000, ncol = 3)
g2 <- arrangeGrob(pp50n100, pp50n1000, pp50n10000, ncol = 3) #generates g
ggsave(file="p50bp.pdf", g2)


grid.arrange(pp100n1000, pp100n10000, ncol = 2)
g3 <- arrangeGrob(pp100n1000, pp100n10000, ncol = 2) #generates g
ggsave(file="p100bp.pdf", g3)




