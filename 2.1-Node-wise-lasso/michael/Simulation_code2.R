library("DescTools")
library(ggplot2)
library(MASS)
library(glmnet)
library(matrixcalc)
setwd("/Users/michaelongwenyun/Desktop/Github/ST443/ST443_Group_project/2.1-Node-wise-lasso/michael")

n = 10000
p = 20
test_df <- data.frame(matrix(ncol=27, nrow=50))
colnames(test_df) <- c("lambda_rmse", "RMSE_either_FP", "RMSE_either_FN", "RMSE_either_TPR", "RMSE_either_TFR", 
                       "RMSE_both_FP", "RMSE_both_FN", "RMSE_both_TPR", "RMSE_both_TFR", 
                       "lambda_cv_min", "cv_min_either_FP", "cv_min_either_FN", "cv_min_either_TPR", "cv_min_either_TFR", 
                       "cv_min_both_FP", "cv_min_both_FN", "cv_min_both_TPR", "cv_min_both_TFR",
                       "lambda_cv1se", "cv1se_either_FP", "cv1se_either_FN", "cv1se_either_TPR", "cv1se_either_TFR", 
                       "cv1se_both_FP", "cv1se_both_FN", "cv1se_both_TPR", "cv1se_both_TFR")

for (i in seq(50)){
  testsample <- simulation(p,n)
  testdata <- testsample$data
  testtheta <-testsample$standardtheta
  trueedge <- true_edge(testtheta)
  
  lambda_rmse <- rmse_best_lambda(testdata)
  estimate_edge_rmse <- edge_table(testdata, lambda_rmse)
  table_rmse_either <- confusion_matrix(estimate_edge_rmse, trueedge, "either")
  table_rmse_both <- confusion_matrix(estimate_edge_rmse, trueedge, "both")
  
  lambda_cv_MIN <- cv_best_lambda(testdata,10,"MIN")
  estimate_edge_cv <- edge_table(testdata, lambda_cv_MIN)
  table_cv_either <- confusion_matrix(estimate_edge_cv, trueedge, "either")
  table_cv_both <- confusion_matrix(estimate_edge_cv, trueedge, "both")
  
  lambda_CV_1SE <- cv_best_lambda(testdata,10,"1SE")
  estimate_edge_cv1se <- edge_table(testdata, lambda_CV_1SE)
  table_cv1se_either <- confusion_matrix(estimate_edge_cv1se, trueedge, "either")
  table_cv1se_both <- confusion_matrix(estimate_edge_cv1se, trueedge, "both")
  
  test_df[i,1] <- lambda_rmse
  test_df[i,2] <- table_rmse_either[2]
  test_df[i,3] <- table_rmse_either[3]
  test_df[i,4] <- table_rmse_either[5]
  test_df[i,5] <- table_rmse_either[6]
  test_df[i,6] <- table_rmse_both[2]
  test_df[i,7] <- table_rmse_both[3]
  test_df[i,8] <- table_rmse_both[5]
  test_df[i,9] <- table_rmse_both[6]
  test_df[i,10] <- lambda_cv_MIN
  test_df[i,11] <- table_cv_either[2]
  test_df[i,12] <- table_cv_either[3]
  test_df[i,13] <- table_cv_either[5]
  test_df[i,14] <- table_cv_either[6]
  test_df[i,15] <- table_cv_both[2]
  test_df[i,16] <- table_cv_both[3]
  test_df[i,17] <- table_cv_both[5]
  test_df[i,18] <- table_cv_both[6]
  test_df[i,19] <- lambda_CV_1SE
  test_df[i,20] <- table_cv1se_either[2]
  test_df[i,21] <- table_cv1se_either[3]
  test_df[i,22] <- table_cv1se_either[5]
  test_df[i,23] <- table_cv1se_either[6]
  test_df[i,24] <- table_cv1se_both[2]
  test_df[i,25] <- table_cv1se_both[3]
  test_df[i,26] <- table_cv1se_both[5]
  test_df[i,27] <- table_cv1se_both[6]
}


write.csv(test_df, "20p10000n_data")


