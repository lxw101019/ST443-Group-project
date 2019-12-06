cv_best_lambda <- function(data){
  library(glmnet)
  numOfRows <- nrow(data)
  numOfDims <- ncol(data)
  
  lambda.best_list = rep(0,numOfDims)
  for (i in seq(numOfDims)){
    y <- (data[,i])
    x <- (data[,-c(i)])
    lasso.train <-cv.glmnet(x, y,type.measure = "mse",nfolds = 10)
    lambda.best_list[i] <-lasso.train$lambda.min
  } 
  final <- mean(lambda.best_list)
  return(final)
}
