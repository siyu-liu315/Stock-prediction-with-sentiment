library(tidyverse)

a <- read.csv('a_group.csv')
a$date <- as.Date(as.character(a$date),format = '%m/%d/%Y')

a_finance <- a %>% select(date, TICKER,PRC,VOL) %>% 
  mutate(pct = (PRC-lag(PRC))/lag(PRC),
         buy = as.numeric(pct > 0 )) %>%  
  filter(date >= as.Date('2016-03-10') & date <= as.Date('2016-06-15'))

names(a_sentiment) = c('date','sentiment_score','TICKER')

df_cla <- merge(a_finance,a_sentiment,by =c('date','TICKER'))             

df_cla$sentiment_score[is.na(df_cla$sentiment_score)] <-0 

Trainse