# This is the code for simulating multivariate gaussian distribution with zero mean and the covariance matrix sigma = theta^-1.
# here p=50 and n = 100
testsample <- simulation(50,100)

# testdata
testdata <- testsample$data

# testtheta
testtheta <-testsample$standardtheta

# Here is one way to calculate the best lambda. 
# This is a validation-set approach.
# choose the lambda cause leas RMSE
lambda_MINTESTERROR <- choose_best_lambda(testdata)
#This function use the thets to generate the "real" edge for reference later
trueedge <- true_edge(testtheta)
# This is the estimated edge we produced with an lambda as argument. Here we use the lambda_MINTESTERROR we calculated as least rmse
estimate_edge <- edge_table(testdata, lambda_MINTESTERROR)
#confusion matrix based on the estimated edge and "real" edge we just generated.

#This function will return a list which include not just the confusion matrix but also TPR and FPR
table <- confusion_matrix(estimate_edge, trueedge, "either")
table

#roc curve: this function use different lambdas(calculated inside) to calculate each TPR and FPR, and return a "point matrix", will need ggplot to plot it later
roc <- ROC_curve(testdata, testtheta,"either",200)

# based on the result given by the ROC_curve , we plot the ROC curve here.
library(ggplot2)
ggplot(roc, aes(FPR, TPR)) + geom_step() 
