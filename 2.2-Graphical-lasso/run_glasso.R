# This is the code for simulating multivariate gaussian distribution with zero mean and the covariance matrix sigma = theta^-1.
# here p=50 and n = 100
library(ggplot2)
library(DescTools)

testsample <- simulation(100,1000)

# testdata
testdata <- testsample$data

# testtheta
testtheta <-testsample$standardtheta

trueedge <- true_edge(testtheta)

s <- cov(testdata)
glassotry <- glasso(s, rho = 0.1)

estimate_edge <- true_edge(glassotry$wi)
#confusion matrix based on the estimated edge and "real" edge we just generated.

#This function will return a list which include not just the confusion matrix but also TPR and FPR
table <- confusion_matrix(estimate_edge, trueedge)
table

#roc curve: this function use different lambdas(calculated inside) to calculate each TPR and FPR, and return a "point matrix", will need ggplot to plot it later
roc <- ROC_curve(testdata, testtheta, lambdamin = 0, lambdamax = 0.2, 200)

# based on the result given by the ROC_curve , we plot the ROC curve here.
ggplot(roc, aes(FPR, TPR)) + geom_step() + xlim(0,1) + ylim(0,1) 
DescTools::AUC(x = roc$FPR,y = roc$TPR)
