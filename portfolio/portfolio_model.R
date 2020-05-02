set.seed(123456)
library(tidyverse)
library(scorer)
library(glmnet)
library(gbm)
library(rpart)
library(randomForest)
library(janitor)
library(scales)

data <- read_csv("portfolio/portfolio_data.csv")
data[2:104] <- scale(data[2:104])
data <- as.data.frame(t(data))
data <- na.omit(data)
data <- data[,order(ncol(data):1)]
names(data) <- as.matrix(data[1, ])
data[] <- lapply(data, function(x) as.numeric(as.character(x)))
data <- data[-1,]


## Split data for portfolio and for selecting model dropping ticker symbol and the tag
smp_size <- floor(0.5 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
model_data <- data[train_ind, ]
port_data <- data[-train_ind, ]


## Splt into Train Test
smp_size <- floor(0.75 * nrow(model_data))
train_ind <- sample(seq_len(nrow(model_data)), size = smp_size)
train <- model_data[train_ind, ]
test <- model_data[-train_ind, ]
test_real <- as.vector(test[[c(1)]])

results <-  tibble(
    x = NA,
    model = NA,
    mse = NA)

for (x in 2:ncol(train)){
  y <- train[c(1:x)]
  y_test <- test[c(2:x)]
  
  ## Linear
  fit <- lm(y$`6/15/2016` ~ ., data = y)
  fit_predict <- predict(fit, y_test)
  mse <- mean_squared_error(test_real, fit_predict)
  results <- results %>% add_row(x = x, model = "linear", mse = mse)

  ## Decision Tree
  tree <- rpart(y$`6/15/2016` ~ ., y)
  fit_predict <- predict(tree, y_test)
  mse <- mean_squared_error(test_real, fit_predict)
  results <- results %>% add_row(x = x, model = "Decision Tree", mse = mse)
  
  ## Random Forest
  y <- clean_names(y)
  y_test <- clean_names(y_test)
  rf <- randomForest(x6_15_2016 ~ ., y, ntree = 2500, do.trace = F)
  fit_predict <- predict(rf, y_test)
  mse <- mean_squared_error(test_real, fit_predict)
  results <- results %>% add_row(x = x, model = "RForest", mse = mse)
 
  # # Boosting
  # boost <- gbm(x6_15_2016 ~ . , 
  #              data = y,
  #              n.trees = 250,
  #              shrinkage = .001)
  # fit_predict <- predict.gbm(boost, y_test, 250)
  # mse <- mean_squared_error(test_real, fit_predict)
  # results <- results %>% add_row(x = x, model = "Boost", mse = mse)
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
results[1,]

## Creating the Final Model
#### The Best performing model from above was simple linear regression with a window of 54
final_model_data <- model_data[2:10]
colnames(final_model_data) <- c(1:9)
final_model <- lm(final_model_data$`1` ~ ., data = final_model_data)

## Preparing The Portfolio Data
tickers <- tibble::rownames_to_column(port_data, "VALUE")
tickers <- tickers %>% select(VALUE)
port_data <- port_data[1:9]
real_port <- port_data[c(1)]
port_data <- port_data[-c(1)]
colnames(port_data) <- c(2:9)

## Predicting For Portfolio Data
port_predictions <- predict(final_model, port_data)
port_data <- cbind(tickers$VALUE, port_predictions, real_port, port_data)
port_data <- port_data %>%  mutate(spread = (port_predictions - port_data$`2`))
port_data <- port_data[order(port_data$spread, decreasing = T),] 
port_data <- port_data %>% mutate(actual_spread = (port_data$`6/15/2016` - port_data$`2`))
aggregate <- mean(port_data$actual_spread)

### Random Baskets For Comparision 
rb1 <- port_data %>% sample_n(5)
set.seed(2)
rb2 <- port_data %>% sample_n(5)
set.seed(3)
rb3 <- port_data %>% sample_n(5)
set.seed(4)
rb4 <- port_data %>% sample_n(5)
set.seed(5)
rb5 <- port_data %>% sample_n(5)
set.seed(6)
rb6 <- port_data %>% sample_n(5)
set.seed(7)
rb7 <- port_data %>% sample_n(5)
set.seed(8)
rb8 <- port_data %>% sample_n(5)
set.seed(9)
rb9 <- port_data %>% sample_n(5)
set.seed(10)
rb10 <- port_data %>% sample_n(5)

## Adding Sentiment Piece
labels <- read_csv("label_output")
labels <- labels %>% filter(date == "2016-06-15")
port_data <- inner_join(port_data, labels, by= c("tickers$VALUE" = "tic"))
port_data <- port_data %>%  filter(buy == 0)

### Winning Basket
winning_basket <- port_data[1:5,]

### Gains/Losses
Gains_Losses <- c(sum(winning_basket$actual_spread),
                   sum(rb1$actual_spread),
                   sum(rb2$actual_spread),
                   sum(rb3$actual_spread),
                   sum(rb4$actual_spread),
                   sum(rb5$actual_spread),
                   sum(rb6$actual_spread),
                   sum(rb7$actual_spread),
                   sum(rb8$actual_spread),
                   sum(rb9$actual_spread),
                   sum(rb10$actual_spread))

### Average Gains/Losses
Average <- c(sum(winning_basket$actual_spread)/5,
                  sum(rb1$actual_spread)/5,
                  sum(rb2$actual_spread)/5,
                  sum(rb3$actual_spread)/5,
                  sum(rb4$actual_spread)/5,
                  sum(rb5$actual_spread)/5,
                  sum(rb6$actual_spread)/5,
                  sum(rb7$actual_spread)/5,
                  sum(rb8$actual_spread)/5,
                  sum(rb9$actual_spread)/5,
                  sum(rb10$actual_spread)/5)

### Portfolio Names
Name <- c('Winner',
               'Basket1',
               'Basket2',
               'Basket3',
               'Basket4',
               'Basket5',
               'Basket6',
               'Basket7',
               'Basket8',
               'Basket9',
               'Basket10')

Portfolio <- data.frame(Name, Gains_Losses, Average)
Portfolio <- Portfolio %>%  mutate(Beat_Winner = if_else(Gains_Losses > Portfolio[1,2], "Yes", "No"))
Portfolio <- Portfolio %>%  mutate(Beat_Aggregate = if_else(Average > aggregate, "Yes", "No"))

### Doing Some Analysis
Analysis <- Portfolio[2:11,]
Analysis <- Analysis %>%  mutate(Beat_Winner = if_else(Beat_Winner == "Yes", 1, 0))
Analysis <- Analysis %>%  mutate(Beat_Aggregate = if_else(Beat_Aggregate == "Yes", 1, 0))

### Amount That Beat The Winning Portfolio
percent(mean(Analysis$Beat_Winner))

### Amount That Beat The Aggregate
percent(mean(Analysis$Beat_Aggregate))

write.csv(Portfolio, file = "portfolio/portfolio_table.csv")
