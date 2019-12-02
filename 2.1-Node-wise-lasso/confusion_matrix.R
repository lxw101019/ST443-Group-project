# This is the code for generating confusion matrix based on given edtimated edge table and true edge table.
# 
# Arguments:1. estimate_edge: the TRUE/FALSE table from edgetable.R
#           2. true_edge: the TRUE/FALSE table from True_edge.R
#           3. estimate_way: input 'both' or 'either' here. 'both' stands for E_1 estimator. 'either' stands for E_2 estimator.
#
# Retutn: 1.confusion: a list. Use list$TP/FP/FN/TN to get each confusion element.
#
confusion_matrix <- function(estimate_edge, true_edge, estimate_way){
  numOfDims <- nrow(estimate_edge)
  confusion <- list(TP = 0,FP = 0,FN = 0,TN = 0)
  
  for (i in seq(numOfDims)){
    for (j in seq(numOfDims)[seq(numOfDims)>i]){
      if (estimate_way == 'either'){
        if (b[i,j] == TRUE){
          if (a[i,j] == FALSE & a[j,i] == FALSE){
            confusion$FN <- confusion$FN + 1
          } else {
            confusion$TP <- confusion$TP + 1
          }
        } else {
          if (a[i,j] == FALSE & a[j,i] == FALSE){
            confusion$TN <- confusion$TN + 1
          } else {
            confusion$FP <- confusion$FP + 1
          }
        }
      } else if (estimate_way == 'both') {
        if (b[i,j] == TRUE){
          if (a[i,j] == TRUE & a[j,i] == TRUE){
            confusion$TP <- confusion$TP + 1
          } else {
            confusion$FN <- confusion$FN + 1
          }
        } else {
          if (a[i,j] == TRUE & a[j,i] == TRUE){
            confusion$FP <- confusion$FP + 1
          } else {
            confusion$TN <- confusion$TN + 1
          }
        }
      }
    }
  }
  return(confusion)
}
