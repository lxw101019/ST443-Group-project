
## Regression Tree
## Adapted from Lab 8.1.2
?write.csv
drops <- c("host_since", "price", "neighbourhood_cleansed","accommodates","near_centre","far_from_centre")
final_data_new <- final_data[ , !(names(final_data) %in% drops)]
write.csv(final_data_new, "st445_final_data")


setwd("/Users/michaelongwenyun/Desktop/Github/ST443/ST443-Group-project")
library(MASS)
library(tree)
library(ISLR)

# Change the dataset to be used. 
# i.e. "st443amsterdamdata_new" is the dataset with anomaly cleaning
final_data <- read.csv("st443amsterdamdata_new")
final_data <- final_data[,-1]


# Regression tree using realprice, not logprice
set.seed(1)
train <- sample(1:nrow(final_data), nrow(final_data) / 2)

#14 independent variables, 7 continuouse and 7 categorical variables
tree.final_data <- tree(realprice ~ review_scores_rating + host_is_superhost +  
                                 host_listings_count + host_identity_verified + 
                                 room_type + bathrooms + bedrooms + 
                                 minimum_nights + number_of_reviews + cancellation_policy + 
                                 instant_bookable + host_since_duration + location_3ways + 
                                 cleaning_fee,  final_data, subset = train)

summary(tree.final_data)
plot(tree.final_data)
text(tree.final_data) # There is a terminal node with average price of $1,896 vastly different from the rest

levels(final_data$cancellation_policy)
levels(final_data$location_3ways)

cv.final_data <- cv.tree(tree.final_data, K=10)
plot(cv.final_data$size, cv.final_data$dev, type = "b")
# Output is volatile. Unable to determine the number of nodes to "prune" the tree. 


## In this case, the most complex tree is selected by cross-validation
## However, if we wish to prune the tree, we could do so as follows using prune.tree() function
?prune.tree
prune.tree_final_data <- prune.tree(tree.final_data, best = 5) #either 1 or 7, but cannot use either, so use 5
summary(prune.tree_final_data)
plot(prune.tree_final_data)
text(prune.tree_final_data) 
# Again, there is a terminal node with average price of $1,896 vastly different from the rest


## In keeping with the CV results, we use the unpruned tree to make predictions on the test data set
## Predicted values on the testing data using regression tree
yhat <- predict(tree.final_data, newdata = final_data[-train,])
## True values on the testing data
tree_final_data.test <- final_data[-train, "realprice"]

plot(yhat, tree_final_data.test)
abline(0, 1)
## Compute the test MSE
mean((yhat - tree_final_data.test) ^ 2)
#MSE of 14449.3

#mean(final_data$realprice) #156.3
#plot(final_data$realprice)

## Bagging
library(randomForest)
set.seed(1)
## Recall that bagging is simply a special case of a random forest with m=p, here we use mtry=14
bag.final_data <- randomForest(realprice ~ review_scores_rating + host_is_superhost +  
                                 host_listings_count + host_identity_verified + 
                                 room_type + bathrooms + bedrooms + 
                                 minimum_nights + number_of_reviews + cancellation_policy + 
                                 instant_bookable + host_since_duration + location_3ways + 
                                 cleaning_fee,  data = final_data, subset = train, mtry=14, importance=TRUE)
bag.final_data

## Predicted values on the testing data using bagging
yhat.bag <-predict(bag.final_data, newdata=final_data[-train,])
plot(yhat.bag, tree_final_data.test)
abline(0,1)
## Compute the test MSE
mean((yhat.bag - tree_final_data.test)^2)
#12086.6

## We can view the importance of each variable
importance(bag.final_data)
varImpPlot(bag.final_data)
# Room_type, location and cancelation policy are the 3 main variable, affecting MSE more than 30%

## Random Forest
set.seed(1)
## Recall that bagging is simply a special case of a random forest with m=p, here we use mtry=14
forest.final_data <- randomForest(realprice ~ review_scores_rating + host_is_superhost +  
                                 host_listings_count + host_identity_verified + 
                                 room_type + bathrooms + bedrooms + 
                                 minimum_nights + number_of_reviews + cancellation_policy + 
                                 instant_bookable + host_since_duration + location_3ways + 
                                 cleaning_fee,  data = final_data, subset = train, mtry=sqrt(14), importance=TRUE)
forest.final_data

## Predicted values on the testing data using bagging
yhat.forest <-predict(forest.final_data, newdata=final_data[-train,])
plot(yhat.forest, tree_final_data.test) #better than initial, but can see outliers
abline(0,1)
## Compute the test MSE
mean((yhat.forest - tree_final_data.test)^2)
#11594.6 (better than bagging - 12086.6)

## We can view the importance of each variable
importance(forest.final_data)
varImpPlot(forest.final_data)
# host_listing, bedrooms are the 2 main variable, affecting MSE only 8%.


#-------------------------------------------#
# Regression tree using logprice            #
# Using logprice                            #
#-------------------------------------------#
tree.final_data_log <- tree(logprice ~ review_scores_rating + host_is_superhost +  
                              host_listings_count + host_identity_verified + 
                              room_type + bathrooms + bedrooms + 
                              minimum_nights + number_of_reviews + cancellation_policy + 
                              instant_bookable + host_since_duration + location_3ways + 
                              cleaning_fee,  final_data, subset = train)

summary(tree.final_data_log)
plot(tree.final_data_log)
text(tree.final_data_log)

cv.final_data_log <- cv.tree(tree.final_data_log, K=10)
plot(cv.final_data_log$size, cv.final_data_log$dev, type = "b") 
#plot patterns looks more normal, i.e. with increasing size of tree(nodes), sum of deviance decrease. 
# 3 nodes sufficient

## In this case, the most complex tree is selected by cross-validation
## However, if we wish to prune the tree, we could do so as follows using prune.tree() function
prune.tree_final_data_log <- prune.tree(tree.final_data_log, best = 3)
summary(prune.tree_final_data_log)
plot(prune.tree_final_data_log)
text(prune.tree_final_data_log)

## In keeping with the CV results, we use the unpruned tree to make predictions on the test data set
## Predicted values on the testing data using regression tree
yhat_log <- predict(prune.tree_final_data_log, newdata = final_data[-train,])
## True values on the testing data
tree_final_data_log.test <- final_data[-train, "logprice"]

plot(yhat_log, tree_final_data_log.test)
abline(0,1)
## Compute the test MSE
mean((yhat_log - tree_final_data_log.test)^2)
#MSE of 0.167 - bear in mind that this is a log - linear relationship. Nevertheless, 0.165 is small

#mean(final_data$logprice) #4.92
#plot(final_data$logprice)



## Bagging and Random Forest
library(randomForest)
set.seed(1)
## Recall that bagging is simply a special case of a random forest with m=p, here we use mtry=13
bag.final_data_log <- randomForest(logprice ~ review_scores_rating + host_is_superhost +  
                                 host_listings_count + host_identity_verified + 
                                 room_type + bathrooms + bedrooms + 
                                 minimum_nights + number_of_reviews + cancellation_policy + 
                                 instant_bookable + host_since_duration + location_3ways + 
                                 cleaning_fee,  data = final_data, subset = train, mtry=14, importance=TRUE)
bag.final_data_log

## Predicted values on the testing data using bagging
yhat_log.bag <-predict(bag.final_data_log, newdata=final_data[-train,])
plot(yhat_log.bag, tree_final_data_log.test) #looks better
abline(0,1)
## Compute the test MSE
mean((yhat_log.bag - tree_final_data_log.test)^2)
# 0.1307, better than 0.167 (original) 

## We can view the importance of each variable
importance(bag.final_data_log)
varImpPlot(bag.final_data_log)
#bedrooms, room_type, cleaning_fee and location, same as decision tree (w/o bagging) above

?varImpPlot
#What does this mean?
#The first graph shows that if a variable is assigned values by random permutation by 
#how much will the MSE increase. So in your case if you randomly permute the year 
#(i.e. an observation which had year =2014 but you randomly assign the year = 2012 
#(if that is present in your bagged sample) and so on) the MSE will increase by 100% on an average. 
#Which make sense that bike demand may have increased in recent years. Higher the value, 
#higher the variable importance.
#
#On the other hand, Node purity is measured by Gini Index which is the the difference 
#between RSS before and after the split on that variable. Since the concept of criteria 
#of variable importance is different in two cases, you have different rankings for different variables.

## Random Forest
library(randomForest)
set.seed(1)
## Recall that bagging is simply a special case of a random forest with m=p, here we use mtry=sqrt(14)
forest.final_data_log <- randomForest(logprice ~ review_scores_rating + host_is_superhost +  
                                     host_listings_count + host_identity_verified + 
                                     room_type + bathrooms + bedrooms + 
                                     minimum_nights + number_of_reviews + cancellation_policy + 
                                     instant_bookable + host_since_duration + location_3ways + 
                                     cleaning_fee,  data = final_data, subset = train, mtry=sqrt(14), importance=TRUE)
forest.final_data_log

## Predicted values on the testing data using bagging
yhat_log.forest <-predict(forest.final_data_log, newdata=final_data[-train,])
plot(yhat_log.forest, tree_final_data_log.test) #similar to bagging (of course...)
abline(0,1) 
## Compute the test MSE
mean((yhat_log.forest - tree_final_data_log.test)^2)
# 0.1257 (Not much different from bagging - 0.1307) 

## We can view the importance of each variable
importance(forest.final_data_log)
varImpPlot(forest.final_data_log)
# bedrooms, room_type, location and cleaning fees are the more important, same as bagging.


# Results comparison table:

Tree_type <- c("Decision Tree", "Bagging (m=14)", "Random Forest (m=sqrt(14))")
Measurement <- c("Nodes","MSE")
Dep_var <- c("realprice","logprice")

?data.frame
comp_table <- data.frame(Tree_type,Measurement,Dep_var)











