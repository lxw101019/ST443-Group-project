# This file is a test file for showing the whole process of nodewise lasso.

# This is the code for simulating multivariate gaussian distribution with zero mean and the covariance matrix sigma = theta^-1.

#set.seed(123)
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
estimate_edge <- edge_table(testdata, lambda_cv_MIN)
#confusion matrix based on the estimated edge and "real" edge we just generated.

#This function will return a list which include not just the confusion matrix but also TPR and FPR
table <- confusion_matrix(estimate_edge, trueedge, "either")
table

#roc curve: this function use different lambdas(calculated inside) to calculate each TPR and FPR, and return a "point matrix", will need ggplot to plot it later
#Here 'either' means we are estimate E by E_2 method
E2_roc <- ROC_curve(testdata, testtheta,"either",100)

#So here is E_1 method.
E1_roc <- ROC_curve(testdata, testtheta,"both",100)

# based on the result given by the ROC_curve , we plot the ROC curve here.
library(ggplot2)
#This plot the E2_roc
ggplot(E2_roc, aes(FPR, TPR)) + geom_step()
#Here for E1_roc
ggplot(E1_roc, aes(FPR, TPR)) + geom_step()
#Here for the integrated plot for both method(preferred).
ggplot()+geom_step(data=E2_roc,mapping = aes(FPR, TPR,colour = 'E_2: either'))+
  geom_step(data=E1_roc, mapping = aes(FPR, TPR,colour = 'E_1: both'))+
  ggtitle("Figure 2.3.2.2")


#This is the AUC function.
#here we can see that E2 method has higher AUC 0.7715, while it is  0.7394 for E1 method
#install.packages("DescTools")
library("DescTools")
AUC(E2_roc$FPR,E2_roc$TPR,method="step")
AUC(E1_roc$FPR,E1_roc$TPR,method="step")




# 50 times setting for ROC COMPARISON
E1_auc_list = rep(0,50)
E2_auc_list = rep(0,50)

for (i in seq(50)){
  testsample <- simulation(250,1000)
  testdata <- testsample$data
  testtheta <-testsample$standardtheta
  E1_roc <- ROC_curve(testdata, testtheta,"both",100)
  E2_roc <- ROC_curve(testdata, testtheta,"either",100)
  E1_auc <- AUC(E1_roc$FPR,E1_roc$TPR,method="step")
  E2_auc <- AUC(E2_roc$FPR,E2_roc$TPR,method="step")
  E1_auc_list[i] <- E1_auc
  E2_auc_list[i] <- E2_auc
}

mean_E1_auc <- mean(E1_auc_list)
mean_E2_auc <- mean(E2_auc_list)




#sapply attempt(it's actually not much faster, so..)
E1_auc_list = rep(0,50)
E2_auc_list = rep(0,50)

sapply(seq(50), function(x){
  testsample <- simulation(250,1000)
  testdata <- testsample$data
  testtheta <-testsample$standardtheta
  E1_roc <- ROC_curve(testdata, testtheta,"both",100)
  E2_roc <- ROC_curve(testdata, testtheta,"either",100)
  E1_auc <- AUC(E1_roc$FPR,E1_roc$TPR,method="step")
  E2_auc <- AUC(E2_roc$FPR,E2_roc$TPR,method="step")
  E1_auc_list[x] <- E1_auc
  E2_auc_list[x] <- E2_auc
})

mean_E1_auc <- mean(E1_auc_list)
mean_E2_auc <- mean(E2_auc_list)



# ROC Curve of same methods with different $p$ and $n$(don't use this part)
E1_auc_list = rep(0,6)
#E2_auc_list = rep(0,2)
graph <- ggplot()
colorss <- c("red","yellow","blue","purple","green","black")
numofdata<-c(2000,1000,500,250,125)
novariable<-c(100,100,100,100,100)
for (j in seq(50)){
  for (i in seq(5)){
    
    testsample <- simulation(novariable[i],numofdata[i])
    testdata <- testsample$data
    testtheta <-testsample$standardtheta
  
    E1_roc <- ROC_curve(testdata, testtheta,"both",100)
    #E2_roc <- ROC_curve(testdata, testtheta,"either",100)
    #graph <- graph + geom_step(data=E1_roc, mapping = aes(FPR, TPR),color = colorss[i])
    E1_auc <- AUC(E1_roc$FPR,E1_roc$TPR,method="step")
    #E2_auc <- AUC(E2_roc$FPR,E2_roc$TPR,method="step")
    E1_auc_list[i] <- E1_auc_list[i] + E1_auc
    #E2_auc_list[i] <- E2_auc
  }
}
E1_auc_list <- E1_auc_list / 50
graph
