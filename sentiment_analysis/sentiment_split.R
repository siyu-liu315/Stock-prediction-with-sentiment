library(tidyverse)
library(readxl)
library(tidytext)


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


### put the sentiment back to original data
# aal_data = aal_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(aal)
# aal_data$Followers = as.numeric(aal_data$Followers)
# aal_data = aal_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "AAL")

# aal <- filter(df1, grepl("AAL", Symbols))
# aal$index <- seq.int(nrow(aal))
# 
# aapl <- filter(df1, grepl("AAPL", Symbols))
# aapl$index <- seq.int(nrow(aapl))
# 
# adbe <- filter(df1, grepl("ADBE", Symbols))
# adbe$index <- seq.int(nrow(adbe))
# 
# adp <- filter(df1, grepl("ADP", Symbols))
# adp$index <- seq.int(nrow(adp))
# 
# adsk <- filter(df1, grepl("ADSK", Symbols))
# adsk$index <- seq.int(nrow(adsk))
# 
# akam <- filter(df1, grepl("AKAM", Symbols))
# akam$index <- seq.int(nrow(akam))
# 
# alxn <- filter(df1, grepl("ALXN", Symbols))
# alxn$index <- seq.int(nrow(alxn))
# 
# amat <- filter(df1, grepl("AMAT", Symbols))
# amat$index <- seq.int(nrow(amat))
# 
# amgn <- filter(df1, grepl("AMGN", Symbols))
# amgn$index <- seq.int(nrow(amgn))
# 
# amzn <- filter(df1, grepl("AMZN", Symbols))
# amzn$index <- seq.int(nrow(amzn))
# 
# atvi <- filter(df1, grepl("ATVI", Symbols))
# atvi$index <- seq.int(nrow(atvi))
# 
# avgo <- filter(df1, grepl("AVGO", Symbols))
# avgo$index <- seq.int(nrow(avgo))


### runing sentiment for each company:
## aal
# aal_token = aal %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# aal_data = aal_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(aal)
# aal_data$Followers = as.numeric(aal_data$Followers)
# aal_data = aal_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "AAL")
# 
# 
# ## aapl
# aapl_token = aapl %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# aapl_data = aapl_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(aapl)
# aapl_data$Followers = as.numeric(aapl_data$Followers)
# aapl_data = aapl_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "AAPL")
# 
# ## adbe
# adbe_token = adbe %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# adbe_data = adbe_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(adbe)
# adbe_data$Followers = as.numeric(adbe_data$Followers)
# adbe_data = adbe_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "ADBE")
# 
# ## adp
# adp_token = adp %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# adp_data = adp_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(adp)
# adp_data$Followers = as.numeric(adp_data$Followers)
# adp_data = adp_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "ADP")
# 
# ### adsk
# 
# adsk_token = adsk %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# adsk_data = adsk_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(adsk)
# adsk_data$Followers = as.numeric(adsk_data$Followers)
# adsk_data = adsk_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "ADSK")
# 
# 
# ### akam
# akam_token = akam %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# akam_data = akam_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(akam)
# akam_data$Followers = as.numeric(akam_data$Followers)
# akam_data = akam_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "AKAM")
# 
# ### alxn sentiment
# 
# alxn_token = alxn %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# alxn_data = alxn_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(alxn)
# alxn_data$Followers = as.numeric(alxn_data$Followers)
# alxn_data = alxn_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "ALXN")
# 
# ### amat sentiment
# 
# amat_token = amat %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# amat_data = amat_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(amat)
# amat_data$Followers = as.numeric(amat_data$Followers)
# amat_data = amat_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "AMAT")
# ### amgn sentiment
# 
# amgn_token = amgn %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# amgn_data = amgn_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(amgn)
# amgn_data$Followers = as.numeric(amgn_data$Followers)
# amgn_data = amgn_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "AMGN")
# 
# ### amzn sentiment
# 
# amzn_token = amzn %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# amzn_data = amzn_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(amzn)
# amzn_data$Followers = as.numeric(amzn_data$Followers)
# amzn_data = amzn_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "AMZN")
# 
# ### atvi sentiment
# 
# atvi_token = atvi %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# atvi_data = atvi_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(atvi)
# atvi_data$Followers = as.numeric(atvi_data$Followers)
# atvi_data = atvi_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "ATVI")
# 
# ### avgo sentiment
# 
# avgo_token = avgo %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# avgo_data = avgo_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(avgo) 
# avgo_data$Followers = as.numeric(avgo_data$Followers)
# avgo_data = avgo_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "AVGO")
# 
# 
# a %>% filter(TICKER == "AVGO")
# avgo_data
# 
# a_sentiment <- rbind(aal_data,aapl_data,adbe_data,adp_data,adsk_data,akam_data,alxn_data,
#       amat_data,amgn_data,amzn_data,atvi_data,avgo_data)
### avgo sentiment

# avgo_token = avgo %>% 
#   select(index, `Tweet content`) %>% 
#   unnest_tokens(token, `Tweet content`, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# avgo_data = avgo_token %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity = mean(value)) %>% 
#   inner_join(avgo) 
# avgo_data$Followers = as.numeric(avgo_data$Followers)
# avgo_data = avgo_data %>% mutate(sentiment_score = polarity * Followers) %>% select(Date,sentiment_score) %>% 
#   group_by(Date) %>% summarise(sentiment_score = mean(sentiment_score)) %>% mutate(symbols = "AVGO")
# 
# 
# a %>% filter(TICKER == "AVGO")
# avgo_data
# 
# a_sentiment <- rbind(aal_data,aapl_data,adbe_data,adp_data,adsk_data,akam_data,alxn_data,
#       amat_data,amgn_data,amzn_data,atvi_data,avgo_data)
