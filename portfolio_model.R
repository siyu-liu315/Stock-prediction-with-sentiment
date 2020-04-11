set.seed(123456)
library(tidyverse)
library(scorer)
library(glmnet)
library(gbm)
library(rpart)
library(randomForest)
library(janitor)

data <- read_csv("portfolio_data.csv")
data <- data[-c(1)]
data <- data %>% select(X1_1, everything())
# Deleting any prices from around the Corona Virus
data <- data[-c(150:505)]
data <- data[-c(2:130)]


## Split data for portfolio and for selecting model dropping ticker symbol and the tag 
smp_size <- floor(0.5 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
model_data <- data[train_ind, ]
model_data <- model_data[-c(1)]
port_data <- data[-train_ind, ]


## Splt into Train Test
smp_size <- floor(0.75 * nrow(model_data))
train_ind <- sample(seq_len(nrow(model_data)), size = smp_size)
train <- model_data[train_ind, ]
test <- model_data[-train_ind, ]
test_real <- as.vector(test[[c(1)]])


results <- tibble(x = 0, model = 0, mse = 0)



for (x in 2:ncol(train)) {
  y <- train[c(1:x)]
  y_test <- test[c(2:x)]
  
  ## Linear
  fit <- lm(y$`10/2/2019` ~ ., data = y)
  fit_predict <- predict(fit, y_test)
  mse <- mean_squared_error(test_real, fit_predict)
  results <- results %>% add_row(x = x, model = "linear", mse = mse)
  
  ## Decision Tree
  tree <- rpart(y$`10/2/2019` ~ ., y)
  fit_predict <- predict(tree, y_test)
  mse <- mean_squared_error(test_real, fit_predict)
  results <- results %>% add_row(x = x, model = "Decision Tree", mse = mse)
  
  ## Random Forest
  y <- clean_names(y)
  y_test <- clean_names(y_test)
  rf <- randomForest(x10_2_2019 ~ ., y, ntree = 2500, do.trace = F)
  fit_predict <- predict(rf, y_test)
  mse <- mean_squared_error(test_real, fit_predict)
  results <- results %>% add_row(x = x, model = "RForest", mse = mse)
 
  
  ## Boosting
  # boost <- gbm(x10_2_2019 ~ .,
  #                       data = y,
  #                       n.trees = 250,
  #                       interaction.depth = 2,
  #                       shrinkage = 0.001)
  # fit_predict <- predict(boost, y_test)
  # mse <- mean_squared_error(test_real, fit_predict)
}

for (x in 3:ncol(train)){
  y_matrix <- as.matrix(train[c(2:x)])
  y_test <- as.matrix(test[c(2:x)])
  y_train_real <- as.matrix(train[c(1)])
  
  lasso <- cv.glmnet(y_matrix, y_train_real, alpha = 1, nfolds = 10)
  fit_predict <- predict(lasso, y_test)
  mse <- mean_squared_error(test_real, fit_predict)
  results <- results %>% add_row(x = x, model = "Lasso", mse = mse)
}

for (x in 3:ncol(train)){
  y_matrix <- as.matrix(train[c(2:x)])
  y_test <- as.matrix(test[c(2:x)])
  y_train_real <- as.matrix(train[c(1)])
  
  ridge <- cv.glmnet(y_matrix, y_train_real, alpha = 0, nfolds = 10)
  fit_predict <- predict(ridge, y_test)
  mse <- mean_squared_error(test_real, fit_predict)
  results <- results %>% add_row(x = x, model = "Ridge", mse = mse)
}

results <- results[order(results$mse, decreasing = F),] 
View(results)
