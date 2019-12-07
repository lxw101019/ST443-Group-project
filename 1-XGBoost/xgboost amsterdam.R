rm(list = ls())

library(readr)
library(dplyr)
library(xgboost)
library(stringr)
library(caret)
library(car)
library(fastDummies)
library(ModelMetrics)

amsterdam <- read_csv('st443_final_data')
amsterdam <- amsterdam[,-c(1,15)]

amsterdam <- mutate(amsterdam,
                    instant_bookable = ifelse(instant_bookable == TRUE, 1, 0))

amsterdam <- fastDummies::dummy_cols(amsterdam)
amsterdam <- amsterdam[,-c(5,10,13,16,22,26)]


traingsize = floor(0.7*nrow(amsterdam))
set.seed(123)
trainindex = sample(seq_len(nrow(amsterdam)),size = traingsize)

train_df <- amsterdam[trainindex,]
test_df <- amsterdam[-trainindex,]

trainmatrix <- as.matrix(train_df, rownames.force = NA)
testmatrix <- as.matrix(test_df, rownames.force = NA)
dtrain <- as(trainmatrix, "sparseMatrix")
dtest <- as(testmatrix, "sparseMatrix")

train_data <- xgb.DMatrix(data = dtrain[,-12], label = dtrain[,"logprice"])
test_data <- xgb.DMatrix(data = dtest[,-12])

xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)

my_control <-trainControl(method="cv", number=5)

# Not run, takes ages
#xgb_caret <- train(x = train_df[-12], y = train_df$logprice, 
#                   method='xgbTree', trControl= my_control, 
#                   tuneGrid = xgb_grid) 
#xgb_caret$bestTune
# nrounds = 1000, max_depth = 5, eta = 0.01, min_child_weight = 1

#xgb_tune <-train(logprice ~.,
#                 data = train_df,
#                 method="xgbLinear",
#                 metric = "RMSE",
#                 trControl = cv.ctrl,
#                 tuneGrid = xgb.grid
#)

default_param <- list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.01, #default = 0.3
  gamma=0,
  max_depth=5, #default=6
  min_child_weight=1, #default=1
  subsample=1,
  colsample_bytree=1
)

xgbcv <- xgb.cv( params = default_param, 
                 data = dtrain, nrounds = 2000, 
                 nfold = 5, 
                 showsd = T, 
                 stratified = T, 
                 print_every_n = 40, 
                 early_stopping_rounds = 10, 
                 maximize = F,
                 label = dtrain[,"logprice"])

xgb_mod <- xgb.train(data = train_data, params = default_param, nrounds = 1300)
XGBpred <- predict(xgb_mod, test_data)
rmse <- rmse(test_df$logprice,XGBpred)

library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance(feature_names = colnames(train_df[-12]),model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)

