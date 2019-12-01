# Lin: 
# This is the code for choosing best lambda based on the simulation data
# 
# Arguments:1. data: In this case we input the simulation data
#           
#
# Retutn: 1.final: which is the best lambda,in each case of making X_i as response, there will be a lambda that minimised the test error.
#                  In this case, the best lambda 'final' is the mean of these lambda. Further discussion needed here.
choose_best_lambda <- function(data){
  numOfRows <- nrow(data)
  numOfDims <- ncol(data)
  
  lambda.best_list = rep(0,numOfDims)
  for (i in seq(numOfDims)){
    y <-(testdata[,i])
    x <- (testdata[,-c(i)])
    
    train <-sample(seq(numOfRows), 0.6*numOfRows, replace=FALSE)
    lasso.train <-glmnet(x[train,], y[train])
    pred.test <-predict(lasso.train, x[-train,])
    rmse <-sqrt(apply((y[-train]-pred.test)^2,2,mean))
    lambda.best <-lasso.train$lambda[order(rmse)[1]]
    lambda.best_list[i] <- lambda.best
  } 
  final <- mean(lambda.best_list)
  return(final)
}

choose_best_lambda(testdata)