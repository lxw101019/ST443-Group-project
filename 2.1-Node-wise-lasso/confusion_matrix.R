# This is the code for generating confusion matrix/ROC curve point based on given edtimated edge table and true edge table.
# 
# Arguments:1. estimate_edge: the TRUE/FALSE table from edgetable.R
#           2. trueEdge: the TRUE/FALSE table from trueEdge.R
#           3. estimate_way: input 'both' or 'either' here. 'both' stands for E_1 estimator. 'either' stands for E_2 estimator.
#
# Retutn: 1.confusion: a list. Use list$TP/FP/FN/TN/TP_rate/FP_rate to get each confusion element or ROC curve point
#
confusion_matrix <- function(estimate_edge, trueEdge, estimate_way){
  numOfDims <- nrow(estimate_edge)
  confusion <- list(TP = 0,FP = 0,FN = 0,TN = 0,TP_rate = 0,FP_rate = 0)
  
  for (i in seq(numOfDims-1)){
    for (j in seq(numOfDims)[seq(numOfDims)>i]){
      if (estimate_way == 'either'){
        if (trueEdge[i,j] == TRUE){
          if (estimate_edge[i,j] == FALSE & estimate_edge[j,i] == FALSE){
            confusion$FN <- confusion$FN + 1
          } else {
            confusion$TP <- confusion$TP + 1
          }
        } else {
          if (estimate_edge[i,j] == FALSE & estimate_edge[j,i] == FALSE){
            confusion$TN <- confusion$TN + 1
          } else {
            confusion$FP <- confusion$FP + 1
          }
        }
      } else if (estimate_way == 'both') {
        if (trueEdge[i,j] == TRUE){
          if (estimate_edge[i,j] == TRUE & estimate_edge[j,i] == TRUE){
            confusion$TP <- confusion$TP + 1
          } else {
            confusion$FN <- confusion$FN + 1
          }
        } else {
          if (estimate_edge[i,j] == TRUE & estimate_edge[j,i] == TRUE){
            confusion$FP <- confusion$FP + 1
          } else {
            confusion$TN <- confusion$TN + 1
          }
        }
      }
    }
  }
  confusion$TP_rate <- confusion$TP/(confusion$TP + confusion$FN)
  confusion$FP_rate <- confusion$FP/(confusion$FP + confusion$TN)
  return(confusion)
}
