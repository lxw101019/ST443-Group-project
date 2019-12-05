library(glasso)

sampleData <- simulation(p = 20, n = 5000)
trueTheta <- sampleData$theta

trainIndex <- sample(seq(nrow(sampleData$testdata)), floor(nrow(sampleData$testdata))/2, replace=FALSE)
train <- sampleData$testdata[trainIndex,]
test <- sampleData$testdata[-trainIndex,]
s <- var(train)  # Empirical Covariance Matrix

ebic <- EBICglasso(s, n = 2500, returnAllResults = TRUE)
optLambdaEBIC <- ebic$lambda[which.min(ebic$ebic)]
optMatrixEBIC <- ebic$optnet

grid <- seq(0,0.5,0.01)
for(i in grid){
  a<-glasso(s, rho=.01)
  print(a$wi) # use to calculate edge set
  aa<-glasso(s,rho=.02, w.init=a$w, wi.init=a$wi)
  aa$wi
}

# Calculate true positive rate, false positive rate. 
# Can plot this over a grid of values of lambda, produces ROC curve. 


#glasso(s, rho, nobs=NULL, zero=NULL, thr=1.0e-4, maxit=1e4,  approx=FALSE, penalize.diagonal=TRUE, start=c("cold","warm"), w.init=NULL,wi.init=NULL, trace=FALSE)

