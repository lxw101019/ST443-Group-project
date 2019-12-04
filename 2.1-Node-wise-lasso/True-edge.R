# Lin: 
# A very short code to generate the true edge table based on the theta(we use it to simulate data)
# 
# Arguments:1. theta:
#           
#
# Retutn: 1.tf: A dataframe, col and row are the number of predictors(eg. row 2 means X_2)
#               each element in the frame means if there is an edge between row index and column index
#               (eg: element at (1,3) is FALSE, then X_1 and X_3 don't have edge inside.)

true_edge <- function(theta){
  numOfDims <- ncol(theta)
  theta <- data.frame(theta)
  tf <- data.frame(lapply(theta, function(x) {x!=0}))
  colnames(tf) <- seq(numOfDims)
  return(tf)
}

