library(qgraph)

sampleDataEbic <- simulation(p = 20, n = 1000)
dat <- sampleDataEbic$data
s <- cov(dat)

ebic <- EBICglasso(s, n = 500, returnAllResults = TRUE, 
                   nlambda = 1000, lambda.min.ratio = 0.001, gamma = 0.5, threshold = TRUE)
optLambdaEBIC <- ebic$lambda[which.min(ebic$ebic)]
optMatrixEBIC <- ebic$optnet

EBICgraph <- qgraph(optMatrixEBIC, layout = "spring", title = "EBIC")
