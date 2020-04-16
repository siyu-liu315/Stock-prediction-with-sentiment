library(tidyverse)
library(caret)
library(randomForest)
library(readxl)
library(tidytext)
library(gbm)

# ### target company for their tweets
# ### can add more
# 
# stock <- c('AAL','AAPL',"ADBE","ADP","ADSK","AKAM",
#            "ALXN","AMAT","AMGN","AMZN","ATVI","AVGO")
# 
# #prepare for the sentiment analysis
# df2 <- df1 %>% select(`Tweet content`,Date,RTs,Favs,Followers,Symbols)%>% 
#   separate(Symbols,into = paste('v',1:28))
# df3 <- df2 %>% pivot_longer(cols = `v 1`:`v 28`,
#                             names_to = 'pos',
#                             values_to = 'symbol',
#                             values_drop_na = T) %>% select(-pos) %>% 
#   filter(symbol %in% stock)
# 
# df3$index <- seq.int(nrow(df3))
# 
# ### runing sentiment for each company:
# ### use dictionary: 
# token = df3 %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "tweets",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# 
# token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(df3)->sentiment_data
# 
# sentiment_data$Followers <- as.numeric(sentiment_data$Followers)
# sentiment_data <- sentiment_data %>% mutate(sentiment_score = polarity * Followers/1000)
# 
# group <- sentiment_data %>% group_by(symbol,Date) %>% 
#   summarise(sum = sum(sentiment_score,na.rm = T)) %>% ungroup()



## sentiment analysis
### split the rtweet data by 
stock <- c('AAL','AAPL',"ADBE","ADP","ADSK","AKAM",
           "ALXN","AMAT","AMGN","AMZN","ATVI","AVGO",'')

#prepare for the sentiment analysis
df2 <- df1 %>% select(`Tweet content`,Date,RTs,Favs,Followers,Symbols)%>% 
  separate(Symbols,into = paste('v',1:28))
df3 <- df2 %>% pivot_longer(cols = `v 1`:`v 28`,
                            names_to = 'pos',
                            values_to = 'symbol',
                            values_drop_na = T) %>% select(-pos) %>% 
  filter(symbol %in% stock)

df3$index <- seq.int(nrow(df3))

### runing sentiment for each company:
token = df3 %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "tweets",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print

token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(df3)->sentiment_data

sentiment_data$Followers <- as.numeric(sentiment_data$Followers)
sentiment_data <- sentiment_data %>% mutate(sentiment_score = polarity * Followers/1000)

group <- sentiment_data %>% group_by(symbol,Date) %>% 
  summarise(sum = sum(sentiment_score,na.rm = T)) %>% ungroup()



### read files include finance data
a <- read.csv('a_group.csv')
a$date <- as.Date(as.character(a$date),format = '%m/%d/%Y')

a_finance <- a %>% select(date, TICKER,PRC,VOL) %>% 
  mutate(pct = (PRC-lag(PRC))/lag(PRC),
         buy = as.numeric(pct > 0 ))%>%
           filter(date >= as.Date('2016-03-10') & date <= as.Date('2016-06-15'))

# EDA
View(a)
ggplot(data = a_finance, aes(x = as.factor(date), y = pct, fill = TICKER)) +
  geom_col(position = "dodge")+
  facet_wrap(~TICKER) +
  labs(title = "Percent Change of A-group Stock Price in Selected Period", x = "Date", y ="Percent Change")


##combind finance data with sentiment data

names(group) = c('TICKER','date','sentiment_score')
group$date = as.Date(group$date)
a_finance$buy <- as.factor(a_finance$buy)
df_cla <- left_join(a_finance,group,by =c('date','TICKER'))
df_cla$sentiment_score[is.na(df_cla$sentiment_score)] <-0


### lag sentiment data
df_1 <- df_cla %>% select(date,TICKER,pct,buy,sentiment_score) %>% 
  arrange(TICKER) %>% mutate(lag_sen = lag(sentiment_score),
                             lag_pct = lag(pct))
df_class <- df_1 %>% filter(date != '2016-03-10')
df_class <-  df_class %>% select(date,buy,lag_pct,lag_sen) 


##function return overall acurracy
get_accuracy<- function(train, test){
  f1 <- as.formula(buy ~ .)
  trees <- randomForest(f1, train,
                        ntree=200,
                        do.trace=F)
  yhat.test.tree <- predict(trees, test)
  yhat.train.tree <- predict(trees, train)
  a <- confusionMatrix(yhat.test.tree,test$buy)
  return(a$overall)
}

### model
train <- df_class %>% filter(date < as.Date('2016-05-31')) %>% select(-date)
test <- df_class %>% filter(date >= as.Date('2016-05-31')) %>% select(-date)

lst <- list()

for(n in seq_len(20)){
  column_name = paste('lag',n,sep = '_')
  df_class[[column_name]] <- lag(df_class$lag_sen,n)
  date_remove <- df_class$date[is.na(df_class[[column_name]])]
  df_class <- df_class %>% filter(date != date_remove)
  train <- df_class %>% filter(date < as.Date('2016-05-31')) %>% select(-date)
  test <- df_class %>% filter(date >= as.Date('2016-05-31')) %>% select(-date)
  lst[n] <- get_accuracy(train, test)
}
###seems windows of 7 would have highest accuracy.


#train_x <- train %>% select(lag_pct,lag_sen)
#train_y <- train$buy

#test_x <- test %>% select(lag_pct,lag_sen)
#test_y <- test$buy

### random forest



### boosting 
## but not defined as a classification 
fit_boost <- gbm(f1,
                  data = train,
                            distribution = "gaussian",
                            n.trees = 100,
                            interaction.depth = 2,
                            shrinkage = 0.001)

pred = predict.gbm(object = fit_boost,
                   newdata = test,
                   n.trees = 100,
                   type = "response")

confusionMatrix(round(pred),test$buy)
confusionMatrix(yhat.train.tree,train$buy)

