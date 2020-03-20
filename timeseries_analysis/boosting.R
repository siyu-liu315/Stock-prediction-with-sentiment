library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(glmnet)
library(rpart)
library("rpart.plot")

## set a standard seed number for reproducible results
set.seed(123456)
## This is our new dataset for the tests
final_15 <- as.data.frame(final_list[15])
## Let Us Create The Same Train And Test
final_15$train <- sample(c(0, 1), nrow(final_15), replace = TRUE, prob = c(.3, .7))
test <- final_15 %>% filter(train == 0)
train <- final_15 %>% filter(train == 1)

y_train <- train %>% select(X15.bResult)
y_test <- test %>% select(X15.bResult)
y_train <- as.matrix(y_train)
y_test <- as.matrix(y_test)

## Creating The Test And Train For The Trees
train_tree <- train %>% select(-train, -X15.matchname, -X15.min,-X15.baron_accum,-X15.elder_dragon_accum,-X15.nexus_turret_accum)
test_tree <- test %>% select(-train, -X15.matchname, -X15.min,-X15.baron_accum,-X15.elder_dragon_accum,-X15.nexus_turret_accum)

## Unline Regression Models Trees and Forests Can Do Classification
train_tree$X15.bResult <- as.factor(train_tree$X15.bResult)
test_tree$X15.bResult <- as.factor(test_tree$X15.bResult)

## Create A Tree
f1 <- as.formula(X15.bResult ~ .)

tree_final_15 <- rpart(f1,
                       train_tree, 
                       method = "class")
## Create The Predictions For Our Tree 
yhat.train.tree <- predict(tree_final_15, train_tree)
yhat.test.tree <- predict(tree_final_15, test_tree)
## Plot The Tree
rpart.plot(tree_final_15)
## Boosted Trees for train data
train_boost_final_15 <- gbm(f1,
                      data = train_tree,
                      distribution = "gaussian",
                      n.trees = 5000,
                      interaction.depth = 2,
                      shrinkage = 0.001)
relative.influence(train_boost_final_15)
train_boost_yhat_btree <- predict(train_boost_final_15, train_tree, n.trees = 5000)
train_boost_mse_btree <- mean((train_boost_yhat_btree - y_train) ^ 2)
### MSE for Train Data
print(train_boost_mse_btree)

## Boosted Trees for test data
test_boost_final_15 <- gbm(f1,
                      data = test_tree,
                      distribution = "gaussian",
                      n.trees = 5000,
                      interaction.depth = 2,
                      shrinkage = 0.001)
relative.influence(test_boost_final_15)
test_boost_yhat_btree <- predict(test_boost_final_15, test_tree, n.trees = 5000)
test_boost_mse_btree <- mean((test_boost_yhat_btree - y_test) ^ 2)
### MSE for Test Data
print(test_boost_mse_btree)
