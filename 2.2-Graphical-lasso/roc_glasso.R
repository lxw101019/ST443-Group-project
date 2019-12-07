ROC_curve <- function(data_set, theta, lambdamin = 0, lambdamax = 1, lambdacount = 200){
  
  numOfDims <- ncol(data_set)
  
  max_lambda = lambdamax
  min_lambda = lambdamin
  points <- data.frame(matrix(ncol = 3, nrow = lambdacount))
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


