rm(list = ls())
# Neural network

#https://jjallaire.github.io/deep-learning-with-r-notebooks/notebooks/3.6-predicting-house-prices.nb.html
#install.packages("keras")
#install.packages("tensorflow")
#install_tensorflow()
library(keras)
library(readr)
library(dplyr)
library(xgboost)
library(stringr)
library(caret)
library(car)
library(fastDummies)
library(ModelMetrics)
library(tidyverse)
library(tensorflow)
library(ISLR)
tf$constant("Hellow Tensorflow")

amsterdam <- read.csv('st445_final_data')
amsterdam <- amsterdam[,-c(1,15)]
dataset <- dataset_boston_housing()
#dummify the data
amsterdam <- mutate(amsterdam,
                    instant_bookable = ifelse(instant_bookable == TRUE, 1, 0))
#sparse_matrix <- sparse.model.matrix(logprice ~ .-1,
#                                     data = amsterdam)

#output_vector = amsterdam[,'logprice']
amsterdam <- fastDummies::dummy_cols(amsterdam)
amsterdam <- amsterdam[,-c(5,10,13,16,22,26)]
#amsterdam <- amsterdam[,-c(5,10,13)]

train_mat <- 
# split out the train and test data
?list
set.seed(1)
trainindex <-sample(seq(15018), 10513, replace=FALSE)
train_df <- amsterdam[trainindex,]
train_data <- as.matrix(train_df[,-12])
train_targets <- as.array(train_df[,12])


vectorize_sequences <- function(sequences, dimension = 10000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}
test <- data.matrix(test_df[,-12], rownames.force=NA)
str(test)
test_df <- amsterdam[-trainindex,]
test_data <- as.matrix(test_df[,-12])
test_targets <- as.array(test_df[,12])

str(train_mat)
str(test_data)
str(train_targets)


#library(MASS)
#boston_df <- Boston
#dataset <- dataset_boston_housing()
#class(dataset)
#c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset
#str(train_data)
#str(test_data)
#str(train_targets)

#Scale the data so that all variables are between 0 and 1

mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)

#Build network - Qn: do we change the units to more than 64? what is ideal?
build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(train_data)[[2]]) %>% 
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 1) 
  
  model %>% compile(
    optimizer = "rmsprop", 
    loss = "mse", 
    metrics = c("mae")
  )
}

#K-fold validation
k <- 2
indices <- sample(1:nrow(train_data))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) 
num_epochs <- 10
all_scores <- c()
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  # Prepare the validation data: data from partition # k
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  # Prepare the training data: data from all other partitions
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  # Build the Keras model (already compiled)
  model <- build_model()
  
  # Train the model (in silent mode, verbose=0)
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 1, verbose = 0)
  
  # Evaluate the model on the validation data
  results <- model %>% evaluate(val_data, val_targets, verbose = 0)
  #all_scores <- c(all_scores, results$mean_absolute_error)
  all_scores <- c(all_scores, results$mae)
} 

all_scores
mean(all_scores)
# average MSE of around 0.31
# tried to run epoch100 times, but too consuming on PC.

# Some memory clean-up
k_clear_session()

# increase num_epoch to 500
num_epochs <- 500
all_mae_histories <- NULL
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  # Prepare the validation data: data from partition # k
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  # Prepare the training data: data from all other partitions
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  # Build the Keras model (already compiled)
  model <- build_model()
  
  # Train the model (in silent mode, verbose=0)
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 1, verbose = 0
  )
  mae_history <- history$metrics$val_mean_absolute_error
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

# calculate average per-epoch MAE score for all folds
average_mae_history <- data.frame(
  epoch = seq(1:ncol(all_mae_histories)),
  validation_mae = apply(all_mae_histories, 2, mean)
)

library(ggplot2)
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_line()
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_smooth()

result
