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

ROC_curve <- function(data_set, theta, lambdamin = 0, lambdamax = 1, lambdacount = 200){
  
  numOfDims <- ncol(data_set)
  
  max_lambda = lambdamax
  min_lambda = lambdamin
  points <- data.frame(matrix(ncol = 3, nrow = lambdacount))
  
# for (i in seq(numOfDims)){
#   y <- (data_set[,i])
#   x <- (data_set[,-c(i)])
#   glassoOutput <- glasso
#   if (lasso.train$lambda[1]>max_lambda){
#     max_lambda <- lasso.train$lambda[1]
#   }
#   if (lasso.train$lambda[length(lasso.train$lambda)]<min_lambda){
#     min_lambda <- lasso.train$lambda[length(lasso.train$lambda)]
#   }
# } 
# #print(max_lambda)
  #print(min_lambda)
  lambda_seq <- seq(from = min_lambda, to = max_lambda, length.out= lambdacount)
  #print(lambda_seq)
  points[,3] <- lambda_seq
  trueedge <- true_edge(theta)
  
  for (i in seq(length(lambda_seq))){
    print(c('Calculating points:',i))
    glassotry <- glasso(s, rho = lambda_seq[i])
    estimate_edge <- true_edge(glassotry$wi)
    confusion <- confusion_matrix(estimate_edge, trueedge)
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


