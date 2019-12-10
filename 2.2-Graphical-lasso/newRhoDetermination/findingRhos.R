rhoList <- c(20, 50, 100)
optRho <- data.frame(count = 1:100)
for(rho in rhoList){
  filename <- paste('optRho',rho,'.csv', sep = "")
  csvthing <- read.csv(filename)
  optRho <- cbind(optRho,csvthing)
}
mean(optRho$X100.10000)
sd(optRho$X100.10000[c(1:25)])
