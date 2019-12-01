rm(list = ls())

setwd("~/Documents/Rworkspace/ST443")

library(readr)
library(dplyr)
library(xgboost)
library(stringr)
library(caret)
library(car)
library(fastDummies)
library(ModelMetrics)

amsterdam <- read_csv('st445_final_data')
amsterdam <- amsterdam[,-c(1,15)]

amsterdam <- mutate(amsterdam,
                    instant_bookable = ifelse(instant_bookable == TRUE, 1, 0))
#sparse_matrix <- sparse.model.matrix(logprice ~ .-1,
#                                     data = amsterdam)

#output_vector = amsterdam[,'logprice']
amsterdam <- fastDummies::dummy_cols(amsterdam)
amsterdam <- amsterdam[,-c(5,10,13,16,22,26)]


set.seed(1)
trainindex <-sample(seq(15018), 7509, replace=FALSE)
train_df <- amsterdam[trainindex,]
test_df <- amsterdam[-trainindex,]

train <- as.matrix(train_df, rownames.force = NA)
test <- as.matrix(test_df, rownames.force = NA)
train <- as(train, "sparseMatrix")
test <- as(test, "sparseMatrix")

train_data <- xgb.DMatrix(data = train[,-12], label = train[,"logprice"])

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3)

xgb.grid <- expand.grid(nrounds = 500,
                        lambda = c(0,1),
                        alpha = c(0,1),
                        eta = c(0.01,0.3, 1)
)

xgb_tune <-train(logprice ~.,
                 data = train_df,
                 method="xgbLinear",
                 metric = "RMSE",
                 trControl = cv.ctrl,
                 tuneGrid = xgb.grid
)

test_data <- xgb.DMatrix(data = test[,-12])

predictionXGB <- predict(xgb_tune, test_df)
rmse(test_df$logprice,predictionXGB)


