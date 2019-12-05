# This is the code for preparing the points that should be plotted on a ROC curve.
# Since we know for different lambda there will be different lasso coef->different estimated table->different confusion matrix->different FPR TPR points
# This function will first calculate the lambda range then use evenly spaced lambdas to generate points(TPR and FPR).
# This function is the most time-consuming since it call all the other function I wrote and loop them over and over.
# 
# Arguments:1. data_set: our simulated data set.
#           2. theta: the standard theta/general theta for simulation. we use it to run true_edge/confusion_matrix functions.
#           3. estimate_way: input 'both' or 'either' here. 'both' stands for E_1 estimator. 'either' stands for E_2 estimator.
#           4. how_many_lambda_in_roc : default is 200 This instance how many points there will be shown on the ROC Curve. Since the ROC curve was plotted by (FPR, TPR) for different lambda,
#              so after we calculate the range of lambda, we evenly space the range into 'how_many_lambda_in_roc' pieces. 
# 
# Retutn: 1.points:a (how_many_lambda_in_roc,3) sized data frame. Storing three data: FPR,TPR,Lambda as columns.
#
# Usage: after generating ROC Points, we should manually use ggplots to plot these point, sample code are provided after the function.
########################################################################################################################################################################################################

ROC_curve <- function(data_set, theta, estimate_way, how_many_lambda_in_roc = 200){

  numOfDims <- ncol(data_set)
  points <- data.frame(matrix(ncol = 3, nrow = how_many_lambda_in_roc))
  
  # This loop I was trying to calculate the range of lambda that I should use. If you can't find better way to replace it, just use it. It works
  max_lambda = 1
  min_lambda = 0
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
  #Lambdas that I am going to use.
  lambda_seq <- seq(from = min_lambda, to = max_lambda, length.out= how_many_lambda_in_roc)
  #Put it into `points` which is a dataframe we just built
  points[,3] <- lambda_seq
  true_edge <- true_edge(theta)
  
  for (i in seq(length(lambda_seq))){
    print(c('Calculating points:',i))
    estimate_edge <- edge_table(data_set, lambda_seq[i])
    confusion <- confusion_matrix(estimate_edge, true_edge, estimate_way)
    TPR <- confusion$TP_rate
    FPR <- confusion$FP_rate
    # put them into `points` as well, they will be the coordinates.
    points[i,1] <- TPR
    points[i,2] <- FPR
  }
  colnames(points) <- c("TPR","FPR","Lambda")
  return(points)
}

#after generating ROC Points, we should manually use ggplots to plot these point:
library(ggplot2)
ggplot(testtest, aes(FPR, TPR)) + geom_step()

