service = read.csv('service.csv')
Tic <- unique(service$tic)


temp_service <- df1 %>% select(`Tweet content`,Date,RTs,Favs,Followers,Symbols)%>% 
  separate(Symbols,into = paste('v',1:28))
df1_clean <- temp_service %>% pivot_longer(cols = `v 1`:`v 28`,
                                   names_to = 'pos',
                                   values_to = 'symbol',
                                   values_drop_na = T) %>% select(-pos) %>% filter(symbol %in% Tic)

temp2_service <- df2 %>% select(`Tweet content`,Date,RTs,Favs,Followers,Symbols)%>% 
  separate(Symbols,into = paste('v',1:28))
df2_clean <- temp2_service %>% pivot_longer(cols = `v 1`:`v 28`,
                                    names_to = 'pos',
                                    values_to = 'symbol',
                                    values_drop_na = T) %>% select(-pos) %>%filter(symbol %in% Tic)

df_content <- rbind(df1_clean,df2_clean)

df_content$index <- seq.int(nrow(df_content))


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

##stock price
stock_price <- read.csv('stock_price.csv')
stock_price$datadate <- as.Date(as.character(stock_price$datadate),format = '%m/%d/%Y')
names(stock_price) = c('gvkey','iid','date','tic','vol','price')


stock <- stock_price %>% select(date, tic,price,vol) %>%
  arrange(tic) %>% 
  mutate(pct = (price-lag(price))/lag(price),
         buy = as.numeric(pct > 0  ))%>%
  filter(date >= as.Date('2016-03-10') & date <= as.Date('2016-06-15'))%>% 
  filter(tic %in% Tic)

##combine
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


 ## modelling
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
