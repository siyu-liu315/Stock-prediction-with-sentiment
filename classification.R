library(tidyverse)
library(caret)

a <- read.csv('a_group.csv')
a$date <- as.Date(as.character(a$date),format = '%m/%d/%Y')

a_finance <- a %>% select(date, TICKER,PRC,VOL) %>% 
  mutate(pct = (PRC-lag(PRC))/lag(PRC),
         buy = as.numeric(pct > 0 ))%>%
           filter(date >= as.Date('2016-03-10') & date <= as.Date('2016-06-15'))

<<<<<<< HEAD
View(a)

ggplot(data = a, aes(x = as.factor(date), y = pct, fill = TICKER)) +
  geom_col(position = "dodge")+
  facet_wrap(~TICKER) +
  labs(title = "Percent Change of A-group Stock Price in Selected Period", x = "Date", y ="Percent Change")
             
=======
names(a_sentiment) = c('date','sentiment_score','TICKER')
a_sentiment$date = as.Date(a_sentiment$date)
a_finance$buy <- as.factor(a_finance$buy)

df_cla <- left_join(a_finance,a_sentiment,by =c('date','TICKER'))             
df_cla$sentiment_score[is.na(df_cla$sentiment_score)] <-0 

df_1 <- df_cla %>% select(date,TICKER,pct,buy,sentiment_score) %>% 
  arrange(TICKER) %>% mutate(lag_sen = lag(sentiment_score),
                             lag_pct = lag(pct))
df_class <- df_1 %>% filter(date != '2016-03-10')


train <- df_class %>% filter(date < as.Date('2016-05-31'))%>% select(buy,lag_pct,lag_sen)
test <- df_class %>% filter(date >= as.Date('2016-05-31'))%>% select(buy,lag_pct,lag_sen)

#train_x <- train %>% select(lag_pct,lag_sen)
#train_y <- train$buy

#test_x <- test %>% select(lag_pct,lag_sen)
#test_y <- test$buy

f1 <- as.formula(buy ~ .)
trees <- randomForest(f1, train,
                            ntree=200,
                            do.trace=F)
yhat.test.tree <- predict(trees, test)
yhat.train.tree <- predict(trees, train)
confusionMatrix(yhat.test.tree,test$buy)
confusionMatrix(yhat.train.tree,train$buy)


fit_boost <- gbm(f1,
                  data = train,
                            distribution = "bernoulli",
                            n.trees = 5000,
                            interaction.depth = 2,
                            shrinkage = 0.001)

pred = predict.gbm(object = fit_boost,
                   newdata = test,
                   n.trees = 5000,
                   type = "response")

confusionMatrix(round(pred),test$buy)
confusionMatrix(yhat.train.tree,train$buy)
>>>>>>> 44ecb36b441dd670b15dc5a16980cd266d259211
