true_edge <- function(theta){
  numOfDims <- ncol(theta)
  theta <- data.frame(theta)
  tf <- data.frame(lapply(theta, function(x) {x!=0}))
  colnames(tf) <- seq(numOfDims)
  return(tf)
}

