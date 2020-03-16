library(tidyverse)

a <- read.csv('a_group.csv')
a$date <- as.Date(as.character(a$date),format = '%m/%d/%Y')

a <- a %>% select(date, TICKER,PRC,VOL) %>% 
  mutate(pct = (PRC-lag(PRC))/lag(PRC),
         buy = as.numeric(pct > 0 )) %>%  
  filter(date > as.Date('2016-03-15') & date < as.Date('2016-06-15'))


             