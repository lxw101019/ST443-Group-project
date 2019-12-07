library(glasso)
library(qgraph)

set.seed(123)
sampleData <- simulation(p = 20, n = 1000)
trueTheta <- sampleData$standardtheta

trainIndex <- sample(seq(nrow(sampleData$data)), floor(nrow(sampleData$data))/2, replace=FALSE)
train <- sampleData$data[trainIndex,]
test <- sampleData$data[-trainIndex,]
sTrain <- cov(train)
sTest <- cov(test)  # Empirical Covariance Matrix
sAll <- cov(sampleData$data)

ll <- data.frame(rho = vector(length=2001), logl = vector(length = 2001))
count <- 0
for(rhos in seq(0,0.2,0.0001)){
  count <- count + 1
  glassoOutput <- glasso(sTrain, rho = rhos)
  modelPrecMat <- glassoOutput$wi
  ll$logl[count] <- log(det(modelPrecMat)) - sum(diag(sTest %*% modelPrecMat))
  ll$rho[count] <- rhos
}

index <- which.max(ll$logl)
glassoGood <- glasso(sAll, ll$rho[index])

true <- true_edge(trueTheta)
modelfit <- true_edge(glassoGood$wi)
confusion_matrix(modelfit,true)
#0.0505


ebic <- EBICglasso(s, n = 500, returnAllResults = TRUE, 
                   nlambda = 1000, lambda.min.ratio = 0.001, gamma = 0.5, threshold = TRUE)
optLambdaEBIC <- ebic$lambda[which.min(ebic$ebic)]
optMatrixEBIC <- ebic$optnet

EBICgraph <- qgraph(glassoGood$wi - diag(diag(glassoGood$wi)), layout = "spring", title = "EBIC")

# log det theta - trace(S theta)

#glasso(s, rho, nobs=NULL, zero=NULL, thr=1.0e-4, maxit=1e4,  approx=FALSE, penalize.diagonal=TRUE, start=c("cold","warm"), w.init=NULL,wi.init=NULL, trace=FALSE)


