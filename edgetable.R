# Lin: 
# This is the code for creating the Edge table based on the node-wise lasso
# With some adjustment it is expected to be applied into graphical lasso.
#
# Arguments:1. data: In this case we input the simulation data
#           2. lambda_choice: this is supposed to be the best data we choose
#
# Retutn: 1.tf: A dataframe, col and row are the number of predictors(eg. row 2 means X_2)
#               each element in the frame means if there is an edge between row index and column index
#               (eg: element at (1,3) is FALSE, then X_1 and X_3 don't have edge inside.)
#
#This function is supposed to used after the choose_best_lambda function.
#This function is supposed to used for 1.2.1 NodeWise lasso approach.
#However it only needs a few modification before we can apply it to the graphical lasso approach.

edge_table <- function(data_set, lambda_choice){
  
  numOfRows <- nrow(data_set)
  numOfDims <- ncol(data_set)
  
  edge <- data.frame(matrix(ncol = numOfDims, nrow = numOfDims))
  
  for (i in seq(numOfDims)){
    y <- (data_set[,i])
    x <- (data_set[,-c(i)])
    coeff <- coef(glmnet(x,y, lambda=lambda_choice))
    coeff <- as.matrix(coeff)[-c(1)]
    coeff <- append(coeff, 1, after= i-1)
    edge[i,] <- coeff
  }
  tf <- data.frame(lapply(edge, function(x) {x!=0}))
  colnames(tf) <- seq(numOfDims)
  return(tf)
}

#Test part:(Please import function: Simulation and choose_best_lambda first.)
#testdata <- simulation(10,100)$data
#final <- choose_best_lambda(testdata)
#edge_table(testdata, final)
