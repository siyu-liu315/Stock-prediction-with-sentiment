names(df1)
try <- head(df1)


library(tidyverse)
library(tidyverse)
library(tidyverse)
library(readxl)
## new packages for us
# install.packages("rvest")
library(rvest)
# install.packages("tidytext")
library(tidytext)
# install.packages("wordcloud)
library(wordcloud)
# install.packages("quanteda")
library(quanteda)
# devtools::install_github("quanteda/quanteda.corpora")
library(quanteda.corpora)
names(df1)

### split the rtweet data by 

aal <- filter(df1, grepl("AAL", Symbols))
aal$index <- seq.int(nrow(aal))

aapl <- filter(df1, grepl("AAPL", Symbols))
aapl$index <- seq.int(nrow(aapl))

adbe <- filter(df1, grepl("ADBE", Symbols))
adbe$index <- seq.int(nrow(adbe))

adp <- filter(df1, grepl("ADP", Symbols))
adp$index <- seq.int(nrow(adp))

adsk <- filter(df1, grepl("ADSK", Symbols))
adsk$index <- seq.int(nrow(adsk))

akam <- filter(df1, grepl("AKAM", Symbols))
akam$index <- seq.int(nrow(akam))

alxn <- filter(df1, grepl("ALXN", Symbols))
alxn$index <- seq.int(nrow(alxn))

amat <- filter(df1, grepl("AMAT", Symbols))
amat$index <- seq.int(nrow(amat))

amgn <- filter(df1, grepl("AMGN", Symbols))
amgn$index <- seq.int(nrow(amgn))

amzn <- filter(df1, grepl("AMZN", Symbols))
amzn$index <- seq.int(nrow(amzn))

atvi <- filter(df1, grepl("ATVI", Symbols))
atvi$index <- seq.int(nrow(atvi))

avgo <- filter(df1, grepl("AVGO", Symbols))
avgo$index <- seq.int(nrow(avgo))


### alxn sentiment

alxn_token = alxn %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
alxn_data = alxn_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(alxn)

### amat sentiment

amat_token = amat %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
amat_data = amat_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(amat)

### amgn sentiment

amgn_token = amgn %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
amgn_data = amgn_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(amgn)


### amzn sentiment

amzn_token = amzn %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
amzn_data = amzn_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(amzn)

### atvi sentiment

atvi_token = atvi %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
atvi_data = atvi_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(atvi)


### avgo sentiment

avgo_token = avgo %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
avgo_data = avgo_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(avgo)





