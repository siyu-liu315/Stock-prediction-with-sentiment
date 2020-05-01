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
                            values_to = 'symbol') %>% select(-pos) %>% 
filter(symbol %in% stock)

temp2 <- df2 %>% select(`Tweet content`,Date,RTs,Favs,Followers,Symbols)%>% 
  separate(Symbols,into = paste('v',1:28))
df2_clean <- temp2 %>% pivot_longer(cols = `v 1`:`v 28`,
                                   names_to = 'pos',
                                   values_to = 'symbol') %>% select(-pos) %>% 
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


stock <- stock_price %>% filter(tic != 'SNDK') %>% 
  select(date, tic,price,vol) %>%
  arrange(tic) %>% 
  mutate(pct = (price-lag(price))/lag(price),
         buy = as.numeric(pct > 0 ))%>%
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

##scale
df_cla$sentiment_score <- scale(df_cla$sentiment_score)
df_cla$sentiment_score[is.na(df_cla$sentiment_score)] <-0


### lag sentiment data
df_1 <- df_cla %>% select(date,tic,pct,buy,sentiment_score) %>% 
  arrange(tic) %>% mutate(lag_sen = lag(sentiment_score),
                             lag_pct = lag(pct))

df_class1 <- df_1 %>% filter(date != '2016-03-10')
df_class <-  df_class1 %>% select(date,buy,lag_pct,lag_sen) 


##function return overall acurracy
get_accuracy<- function(train, test){
  f1 <- as.formula(buy ~ .)
  trees <- randomForest(f1, train,
                        ntree=200,
                        do.trace=F)
  yhat.test.tree <- predict(trees, test)
  yhat.train.tree <- predict(trees, train)
  a <- confusionMatrix(yhat.test.tree,test$buy)
  print(a$overall)
  return(a)
}

get_detail <- function(a, n){
  if (n == 'overall'){
    return (a$overall)
  }
  if (n == 'positive'){
    return(a$byClass[3])
  }
  if(n == 'negative'){
    return(a$byClass[4])
  }
}


### model
# train <- df_class %>% filter(date < as.Date('2016-05-31')) %>% select(-date)
# test <- df_class %>% filter(date >= as.Date('2016-05-31')) %>% select(-date)


df_result <- as.data.frame(1:20)

for(n in seq(0,19)){
  column_name = paste('lag',n+1,sep = '_')
  df_class[[column_name]] <- lag(df_class$lag_sen,1)
  date_remove <- df_class$date[is.na(df_class[[column_name]])]
  print(date_remove)
  df_class <- df_class %>% filter(date != date_remove)
  diff = (max(df_class$date)-min(df_class$date))*0.85
  thre <- diff +  min(df_class$date)
  print(thre)
  train <- df_class %>% filter(date <= thre) %>% select(-date)
  test <- df_class %>% filter(date > thre) %>% select(-date)
  print(nrow(train))
  print(nrow(test))
  a <- get_accuracy(train, test)
  df_result$overall[n+1] <- get_detail(a,'overall')
  df_result$positive[n+1] <- get_detail(a,'positive')
  df_result$negative[n+1] <- get_detail(a,'negative')
  lst[n]
}


###seems windows of 7 would have highest accuracy.
# 
# df_result <- as.data.frame(1:20)
# df_result$accuracy = lst
# df_result$accuracy = as.numeric(df_result$accuracy)
names(df_result)[1] <- 'windows'
#names(df_result)[4] <- 'negative'
#names(df_result)[5] <- 'overall'


ggplot(df_result)+
  geom_line(aes(x = windows,y = positive))+
  geom_line(aes(x = windows,y = negative))+
  geom_line(aes(x = windows,y = overall))+
  geom_hline(yintercept = 0.5,color = 'red')+
  ggtitle('Classification Accuracy on Diffrent Windows')


#train_x <- train %>% select(lag_pct,lag_sen)
#train_y <- train$buy

#test_x <- test %>% select(lag_pct,lag_sen)
#test_y <- test$buy

### random forest



# ### boosting 
# ## but not defined as a classification 
# fit_boost <- gbm(f1,
#                   data = train,
#                             distribution = "gaussian",
#                             n.trees = 100,
#                             interaction.depth = 2,
#                             shrinkage = 0.001)
# 
# pred = predict.gbm(object = fit_boost,
#                    newdata = test,
#                    n.trees = 100,
#                    type = "response")
# 
# confusionMatrix(round(pred),test$buy)
# confusionMatrix(yhat.train.tree,train$buy)


###for y_hat

df_1 <- df_cla %>% select(date,tic,pct,buy,sentiment_score) %>% 
  arrange(tic) %>% mutate(lag_sen = lag(sentiment_score),
                          lag_pct = lag(pct))

df_class1 <- df_1 %>% filter(date != '2016-03-10')
df_class <-  df_class1 %>% select(date,buy,lag_pct,lag_sen) 


for (n in 1:7){
column_name = paste('lag',n+1,sep = '_')
df_class[[column_name]] <- lag(df_class$lag_sen,n)
}


date_remove <- df_class$date[is.na(df_class[[column_name]])]
df_filterday <- df_class %>% filter(!date %in% date_remove)

train <- df_filterday %>% filter(date < as.Date('2016-05-31')) %>% select(-date)
test <- df_filterday %>% filter(date >= as.Date('2016-05-31')) %>% select(-date)

f2 <- as.formula(buy ~ .)
trees <- randomForest(f2, train,
                        ntree=200,
                        do.trace=F)
yhat.test.tree <- predict(trees, test)
yhat.train.tree <- predict(trees, train)

confusionMatrix(yhat.test.tree,test$buy)

test <- df_filterday %>% filter(date >= as.Date('2016-05-31'))
test$yhat <- yhat.test.tree


label <- df_class1  %>% filter(date >= '2016-05-31' )%>% select(tic)
output <- cbind(label,test)
output <- output %>% select(tic,date,buy, yhat)

write.csv(output,'label_output2')


a <- read.csv('label_output')

