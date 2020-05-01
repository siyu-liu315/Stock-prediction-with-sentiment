data <- read_csv("portfolio/portfolio_data.csv")
data[2:104] <- scale(data[2:104])
data <- as.data.frame(t(data))
names(data) <- as.matrix(data[1, ])
data <- data[-1,]
data <- na.omit(data)
data <- data[,order(ncol(data):1)]
data['tic'] = rownames(data)
View(data)
## Split data for portfolio and for selecting model dropping ticker symbol and the tag

smp_size <- floor(0.5 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
model_data <- data[train_ind, ]
#model_data <- model_data[-c(1)]
port_data <- data[-train_ind, ]


## Splt into Train Test
smp_size <- floor(0.75 * nrow(model_data))
train_ind <- sample(seq_len(nrow(model_data)), size = smp_size)
train <- model_data[train_ind, ]
test <- model_data[-train_ind, ]
#test_real <- as.vector(test[[c(1)]])

train_long <- pivot_longer(train,cols = -tic,names_to = 'date',values_to = 'price' ) %>% 
  arrange(tic,date)
test_long <- pivot_longer(test,cols = -tic,names_to = 'date',values_to = 'price' ) %>% arrange(tic,date)


column_name = paste('lag',1,sep = '_')
train_long[[column_name]] <- lag(train_long$price,1)
date_remove <- train_long$date[is.na(train_long[[column_name]])]
train_long <- train_long %>% filter(date != date_remove)
test_long[[column_name]] <- lag(test_long$price,1)
test_long <- test_long %>% filter(date != date_remove)


## Linear
fit <- lm(train_long$price ~ ., data = train_long)
fit_predict <- predict(fit, y_test)
mse <- mean_squared_error(test_real, fit_predict)
results <- results %>% add_row(x = x, model = "linear", mse = mse)

## Decision Tree
tree <- rpart(y$x6_15_2016 ~ ., y)
fit_predict <- predict(tree, y_test)
mse <- mean_squared_error(test_real, fit_predict)
results <- results %>% add_row(x = x, model = "Decision Tree", mse = mse)

## Random Forest
y <- clean_names(y)
y_test <- clean_names(y_test)
rf <- randomForest(x6_15_2016 ~ ., y, ntree = 250, do.trace = F)
fit_predict <- predict(rf, y_test)
mse <- mean_squared_error(test_real, fit_predict)
results <- results %>% add_row(x = x, model = "RForest", mse = mse)

# Boosting
boost <- gbm(x6_15_2016 ~ . , 
             data = y,
             n.trees = 250,
             shrinkage = .001)
fit_predict <- predict.gbm(boost, y_test, 250)
mse <- mean_squared_error(test_real, fit_predict)
results <- results %>% add_row(x = x, model = "Boost", mse = mse)
}


print(nrow(train))
print(nrow(test))
lst[n] <- get_accuracy(train, test)


lst <- list()

for(n in seq_len(19)){
  column_name = paste('lag',n+1,sep = '_')
  df_class[[column_name]] <- lag(df_class$lag_sen,n+1)
  date_remove <- df_class$date[is.na(df_class[[column_name]])]
  df_class <- df_class %>% filter(date != date_remove)
  train <- df_class %>% filter(date < as.Date('2016-05-31')) %>% select(-date)
  test <- df_class %>% filter(date >= as.Date('2016-05-31')) %>% select(-date)
  print(nrow(train))
  print(nrow(test))
  lst[n] <- get_accuracy(train, test)
}


