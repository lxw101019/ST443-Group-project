confusion_matrix <- function(estimate_edge, trueEdge){
  numOfDims <- nrow(estimate_edge)
  confusion <- list(TP = 0,FP = 0,FN = 0,TN = 0,TP_rate = 0,FP_rate = 0)
  for (i in seq(numOfDims-1)){
    for (j in seq(numOfDims)[seq(numOfDims)>i]){
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
  confusion$TP_rate <- confusion$TP/(confusion$TP + confusion$FN)
  confusion$FP_rate <- confusion$FP/(confusion$FP + confusion$TN)
  return(confusion)
}
