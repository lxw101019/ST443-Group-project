library(ggplot2)
library(DescTools)
library(glasso)

testsample <- simulation(100,100)

# testdata
testdata <- testsample$data

# testtheta
testtheta <-testsample$standardtheta

trueedge <- true_edge(testtheta)

s <- cov(testdata)


#roc curve: this function use different lambdas(calculated inside) to calculate each TPR and FPR, and return a "point matrix", will need ggplot to plot it later
roc <- ROC_curve(testdata, testtheta, lambdamin = 0, lambdamax = 0.2, 200)

# based on the result given by the ROC_curve , we plot the ROC curve here.
ggplot(roc, aes(FPR, TPR)) + geom_step() + xlim(0,1) + ylim(0,1)  + geom_point(alpha = 0.3)

DescTools::AUC(x = roc$FPR,y = roc$TPR)

# p 5, 20, 50, 100
# n  10000
# calculate AUC