library(ggplot2)
ROC_curve <- function(data_set, theta, estimate_way, how_many_lambda_in_roc = 1000){

  numOfDims <- ncol(data_set)
  print(numOfDims)
  max_lambda = 1
  min_lambda = 0
  points <- data.frame(matrix(ncol = 3, nrow = how_many_lambda_in_roc))
  
  for (i in seq(numOfDims)){
    y <- (data_set[,i])
    x <- (data_set[,-c(i)])
    lasso.train <-glmnet(x, y)
    if (lasso.train$lambda[1]>max_lambda){
      max_lambda <- lasso.train$lambda[1]
    }
    if (lasso.train$lambda[length(lasso.train$lambda)]<min_lambda){
      min_lambda <- lasso.train$lambda[length(lasso.train$lambda)]
    }
  } 
  #print(max_lambda)
  #print(min_lambda)
  lambda_seq <- seq(from = min_lambda, to = max_lambda, length.out= how_many_lambda_in_roc)
  #print(lambda_seq)
  points[,3] <- lambda_seq
  true_edge <- true_edge(theta)
  for (i in seq(length(lambda_seq))){
    estimate_edge <- edge_table(data_set, lambda_seq[i])
    #print(estimate_edge)
    confusion <- confusion_matrix(estimate_edge, true_edge, estimate_way)
    #print(confusion)
    TPR <- confusion$TP_rate
    FPR <- confusion$FP_rate
    #print(TPR)
    #print(FPR)
    points[i,1] <- TPR
    points[i,2] <- FPR
  }
  colnames(points) <- c("TPR","FPR","Lambda")
  
  return(points)
}
ggplot(testtest, aes(FPR, TPR)) + geom_step()

#testdata <- simulation(100,1000)
#data <- testdata$data
#theta <- testdata$standardtheta
#testtest <- ROC_curve(data,theta,"either")
