library(qgraph)

tprfpr100 <- data.frame(tpr = vector(length = 50), fpr = vector(length = 50))
for(i in 1:50){
  sampleDataEbic <- simulation(p = 100, n = 10000)
  dat <- sampleDataEbic$data
  s <- cov(dat)
  ebic <- EBICglasso(s, nrow(dat), returnAllResults = TRUE, 
                     nlambda = 100, lambda.min.ratio = 0.001, gamma = 0)
  optLambdaEBIC <- ebic$lambda[which.min(ebic$ebic)]
  optMatrixEBIC <- ebic$optnet
  estimated <- true_edge(optMatrixEBIC)
  trueTet <- true_edge(sampleDataEbic$standardtheta)
  cm <- confusion_matrix(estimated,trueTet)
  #EBICgraph <- qgraph(optMatrixEBIC, layout = "spring", title = "EBIC")
  tprfpr100[i,1] <- cm$TP_rate
  tprfpr100[i,2] <- cm$FP_rate
}
ebictf <- data.frame('p20' = tprfpr20, 'p50' = tprfpr50, 'p100' = tprfpr100)
write.csv(ebictf, file = 'ebictprfpr.csv')

mean(ebictf$p100.fpr)
sd(ebictf$p50.fpr)
