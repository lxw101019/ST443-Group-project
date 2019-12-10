
groupdf <- function(df) {
  df$rmse <- "rmse"
  df$cv_min <- "cv_min"
  df$cv1se <- "cv1se"
  df$e1 <- "E_1"
  df$e2 <- "E_2"
  
  df_rmse_e1 <- df[,c("RMSE_either_FP","RMSE_either_FN","rmse","e1")]
  colnames(df_rmse_e1) = c("FP", "FN", "method", "Estimated_E")
  df_cv_min_e1 <- df[,c("cv_min_either_FP","cv_min_either_FN","cv_min","e1")]
  colnames(df_cv_min_e1) = c("FP","FN","method", "Estimated_E")
  df_cv1se_e1 <- df[,c("cv1se_either_FP","cv1se_either_FN","cv1se","e1")]
  colnames(df_cv1se_e1) = c("FP", "FN", "method", "Estimated_E")
  
  df_rmse_e2 <- df[,c("RMSE_both_FP","RMSE_both_FN","rmse","e2")]
  colnames(df_rmse_e2) = c("FP", "FN", "method", "Estimated_E")
  df_cv_min_e2 <- df[,c("cv_min_both_FP","cv_min_both_FN","cv_min","e2")]
  colnames(df_cv_min_e2) = c("FP","FN","method", "Estimated_E")
  df_cv1se_e2 <- df[,c("cv1se_both_FP","cv1se_both_FN","cv1se","e2")]
  colnames(df_cv1se_e2) = c("FP", "FN", "method", "Estimated_E")
  
  df_all <- rbind(df_rmse_e1,df_cv_min_e1, df_cv1se_e1, df_rmse_e2,df_cv_min_e2, df_cv1se_e2)
}

df5p100n <- read.csv("5p100n_data")
df20p100n <- read.csv("20p100n_data")
df50p100n <- read.csv("50p100n_data")
df100p100n <- read.csv("100p100n_data")

df5p1000n <- read.csv("5p1000n_data")
df20p1000n <- read.csv("20p1000n_data")
df50p1000n <- read.csv("50p1000n_data")
df100p1000n <- read.csv("100p1000n_data")

df5p10000n <- read.csv("5p10000n_data")
df20p10000n <- read.csv("20p10000n_data")
df50p10000n <- read.csv("50p10000n_data")
df100p10000n <- read.csv("100p10000n_data")

df5p10n <- read.csv("5p10n_data")
df20p10n <- read.csv("20p10n_data")
df50p10n <- read.csv("50p10n_data")
df100p10n <- read.csv("100p10n_data")

#colnames(test_df) <- c("lambda_rmse", "RMSE_either_FP", "RMSE_either_FN", "RMSE_either_TPR", "RMSE_either_TFR", 
"RMSE_both_FP", "RMSE_both_FN", "RMSE_both_TPR", "RMSE_both_TFR", 
"lambda_cv_min", "cv_min_either_FP", "cv_min_either_FN", "cv_min_either_TPR", "cv_min_either_TFR", 
"cv_min_both_FP", "cv_min_both_FN", "cv_min_both_TPR", "cv_min_both_TFR",
"lambda_cv1se", "cv1se_either_FP", "cv1se_either_FN", "cv1se_either_TPR", "cv1se_either_TFR", 
"cv1se_both_FP", "cv1se_both_FN", "cv1se_both_TPR", "cv1se_both_TFR")

df_5p10n <- groupdf(df5p10n)
df_20p10n <- groupdf(df20p10n)
df_50p10n <- groupdf(df50p10n)
df_100p10n <- groupdf(df100p10n)

df_5p100n <- groupdf(df5p100n)
df_20p100n <- groupdf(df20p100n)
df_50p100n <- groupdf(df50p100n)
df_100p100n <- groupdf(df100p100n)

df_5p1000n <- groupdf(df5p1000n)
df_20p1000n <- groupdf(df20p1000n)
df_50p1000n <- groupdf(df50p1000n)
df_100p1000n <- groupdf(df100p1000n)

df_5p10000n <- groupdf(df5p10000n)
df_20p10000n <- groupdf(df20p10000n)
df_50p10000n <- groupdf(df50p10000n)
df_100p10000n <- groupdf(df100p10000n)

par(mfrow=c(2,2))
ggplot(df_5p10000n, aes(x=method, y=FP)) +
  geom_boxplot(aes(fill = Estimated_E)) +
  ggtitle("n=100, p = 5)")

ggplot(df_20p10000n, aes(x=method, y=FP)) +
  geom_boxplot(aes(fill = Estimated_E)) +
  ggtitle("n=100, p = 20)")

ggplot(df_50p10000n, aes(x=method, y=FP)) +
  geom_boxplot(aes(fill = Estimated_E))+
  ggtitle("n=100, p = 50)")

ggplot(df_100p10000n, aes(x=method, y=FP)) +
  geom_boxplot(aes(fill = Estimated_E))+
  ggtitle("n=100, p = 100)")
