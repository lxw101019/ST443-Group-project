# This is the code for choosing best lambda based using cross validation (select lambda corresponds to lowest MSE)
# 
# Arguments:1. data: In this case we input the simulation data
#           2. numOfFolds: 5,10 or loocv. need to type in the exact integer.
#           3. MIN_1SE : Input "MIN" or "1SE".
# Retutn: 1.final: lambda. a number.
cv_best_lambda <- function(data,numOfFolds, MIN_1SE){
  library(glmnet)
  numOfRows <- nrow(data)
  numOfDims <- ncol(data)

  lambda.best_list = rep(0,numOfDims)
  for (i in seq(numOfDims)){
    y <- (data[,i])
    x <- (data[,-c(i)])
    lasso.train <-cv.glmnet(x, y,type.measure = "mse",nfolds = numOfFolds)
    if (MIN_1SE == 'MIN'){
      lambda.best_list[i] <-lasso.train$lambda.min
    } else if (MIN_1SE == '1SE'){
      lambda.best_list[i] <-lasso.train$lambda.1se
    }
  } 
  final <- mean(lambda.best_list)
  return(final)
}
