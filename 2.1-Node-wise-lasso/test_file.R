# This file is a test file for showing the whole process of nodewise lasso.

# This is the code for simulating multivariate gaussian distribution with zero mean and the covariance matrix sigma = theta^-1.
# here p=50 and n = 100
testsample <- simulation(50,100)

# testdata
testdata <- testsample$data

# testtheta
testtheta <-testsample$standardtheta

# Here is one way to calculate the best lambda. 
# This is a validation-set approach.
# choose the lambda cause least RMSE
lambda_rmse <- rmse_best_lambda(testdata)
# use crossvalidation with lambda correspond to lowest MSE
lambda_cv_MIN <- cv_best_lambda(testdata,10,"MIN")
# cv with lambda correspond to point with lower number of varaibles, within 1se of the best lambda we just calculated.
lambda_CV_1SE <- cv_best_lambda(testdata,10,"1SE")

#This function use the thets to generate the "real" edge for reference later
trueedge <- true_edge(testtheta)
# This is the estimated edge we produced with an lambda as argument. Here we use the lambda_MINTESTERROR we calculated as least rmse
estimate_edge <- edge_table(testdata, lambda_MINTESTERROR)
#confusion matrix based on the estimated edge and "real" edge we just generated.

#This function will return a list which include not just the confusion matrix but also TPR and FPR
table <- confusion_matrix(estimate_edge, trueedge, "either")
table

#roc curve: this function use different lambdas(calculated inside) to calculate each TPR and FPR, and return a "point matrix", will need ggplot to plot it later
#Here 'either' means we are estimate E by E_2 method
E2_roc <- ROC_curve(testdata, testtheta,"either",200)

#So here is E_1 method.
E1_roc <- ROC_curve(testdata, testtheta,"both",200)

# based on the result given by the ROC_curve , we plot the ROC curve here.
library(ggplot2)
#This plot the E2_roc
ggplot(E2_roc, aes(FPR, TPR)) + geom_step()
#Here for E1_roc
ggplot(E1_roc, aes(FPR, TPR)) + geom_step()
#Here for the integrated plot for both method(preferred).
ggplot()+geom_step(data=E2_roc,mapping = aes(FPR, TPR,colour = 'E_2: either'))+
  geom_step(data=E1_roc, mapping = aes(FPR, TPR,colour = 'E_1: both'))

#This is the AUC function.
#here we can see that E2 method has higher AUC 0.7715, while it is  0.7394 for E1 method
#install.packages("DescTools")
library("DescTools")
AUC(E2_roc$FPR,E2_roc$TPR,method="step")
AUC(E1_roc$FPR,E1_roc$TPR,method="step")

