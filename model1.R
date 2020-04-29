library(tidyverse)
library(caret)
library(randomForest)
library(readxl)
library(tidytext)
library(gbm)

stock <- c("AAL","AAPL","ADBE","ADP","ADSK","AKAM",
           "ALXN","AMAT","AMGN","AMZN","ATVI","AVGO","BBBY",
           "BIDU","BMRN","CA","CELG","CERN","CHKP","CHTR","CMCSA","COST","CSCO",
           "CSX","CTRP","CTSH","DISCA","DISCK","DISH","DLTR","EA","EBAY","ENDP","ESRX",
           "EXPE","FAST","FB","FISV","FOX","FOXA","GILD","GOOG","GOOGL","HSIC","ILMN",
           "INCT","INCY","INTU","ISRG","JD","KHC","LBTYA","LBTYK","LlTC","LMCA","LMCK",
           "LRCX","LVNTA","MAR","MAT","MDLZ","MNST","MSFT","MU","MXIM","MYL","NCLH",
           "NFLX","NTAP","NTES","NVDA","NXPI","ORLY","PAYX","PCAR","PCLN","PYPL","QCOM",
           "QVCA","REGN","ROST","SBAC","SBUX","SNDK","SRCL","STX","SWKS","SYMC","TMUS",
           "TRIP","TSCO","TSLA","TXN","ULTA","VIAB","VOD","VRSK","VRTX","WBA","WDC","WFM","XLNX","YHOO")

#prepare for the sentiment analysis
temp <- df1 %>% select(`Tweet content`,Date,RTs,Favs,Followers,Symbols)%>% 
  separate(Symbols,into = paste('v',1:28))
df1_clean <- temp %>% pivot_longer(cols = `v 1`:`v 28`,
                            names_to = 'pos',
                            values_to = 'symbol',
                            values_adrop_na = T) %>% select(-pos) %>% 
filter(symbol %in% stock)

temp2 <- df2 %>% select(`Tweet content`,Date,RTs,Favs,Followers,Symbols)%>% 
  separate(Symbols,into = paste('v',1:28))
df2_clean <- temp2 %>% pivot_longer(cols = `v 1`:`v 28`,
                                   names_to = 'pos',
                                   values_to = 'symbol',
                                   values_drop_na = T) %>% select(-pos) %>% 
  filter(symbol %in% stock)

df_content <- rbind(df1_clean,df2_clean)
# df_content <- unique(df_content)

df_content$index <- seq.int(nrow(df_content))


### runing sentiment for each company:
token = df_content %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "tweets",
                strip_punct = T) 

token <- token%>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word"))

token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(df_content)->sentiment_data

sentiment_data$Followers <- as.numeric(sentiment_data$Followers)
sentiment_data <- sentiment_data %>% mutate(sentiment_score = polarity * Followers/1000)

group <- sentiment_data %>% group_by(symbol,Date) %>% 
  summarise(sum = sum(sentiment_score,na.rm = T)) %>% ungroup()



### read files include finance data
stock_price <- read.csv('stock_price.csv')
stock_price$datadate <- as.Date(as.character(stock_price$datadate),format = '%m/%d/%Y')
names(stock_price) = c('gvkey','iid','date','tic','vol','price')


stock <- stock_price %>% select(date, tic,price,vol) %>%
  arrange(tic) %>% 
  mutate(pct = (price-lag(price))/lag(price),
         buy = as.numeric(pct < 0 ))%>%
           filter(date >= as.Date('2016-03-10') & date <= as.Date('2016-06-15'))

# # EDA
# View(a)
# ggplot(data = a_finance, aes(x = as.factor(date), y = pct, fill = TICKER)) +
#   geom_col(position = "dodge")+
#   facet_wrap(~TICKER) +
#   labs(title = "Percent Change of A-group Stock Price in Selected Period", x = "Date", y ="Percent Change")


##combind finance data with sentiment data

names(group) = c('tic','date','sentiment_score')
group$date = as.Date(group$date)
stock$buy <- as.factor(stock$buy)
df_cla <- left_join(stock,group,by =c('date','tic'))
df_cla$sentiment_score[is.na(df_cla$sentiment_score)] <-0


### lag sentiment data
df_1 <- df_cla %>% select(date,tic,pct,buy,sentiment_score) %>% 
  arrange(tic) %>% mutate(lag_sen = lag(sentiment_score),
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
  print(a)
  return(a$overall)
}

### model
# train <- df_class %>% filter(date < as.Date('2016-05-31')) %>% select(-date)
# test <- df_class %>% filter(date >= as.Date('2016-05-31')) %>% select(-date)

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



###seems windows of 7 would have highest accuracy.

df_result <- as.data.frame(1:20)
df_result$accuracy = lst
df_result$accuracy = as.numeric(df_result$accuracy)
names(df_result)[1] <- 'windows'


ggplot(df_result,aes(x = windows, y = accuracy))+
  geom_line()+
  geom_hline(yintercept = 0.5,color = 'red')+
  ggtitle('Classification Accuracy on Diffrent Windows')


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

