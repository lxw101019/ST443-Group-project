testsample <- simulation(100,20)
testdata <- testsample$data
testtheta <-testsample$standardtheta
lambda_MINTESTERROR <- choose_best_lambda(testdata)

trueedge <- true_edge(testtheta)
estimate_edge <- edge_table(testdata, lambda_MINTESTERROR)
table <- confusion_matrix(estimate_edge, trueedge, "either")
table

roc <- ROC_curve(testdata, testtheta,"either",1000)

library(ggplot2)
ggplot(roc, aes(FPR, TPR)) + geom_step() + geom_point()
