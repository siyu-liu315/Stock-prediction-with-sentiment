names(df1)
try <- head(df1)
View(try)

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
aal$index <- seq.int(nrow(aal))

amzn <- filter(df1, grepl("AMZN", Symbols))
amzn$index <- seq.int(nrow(amzn))

atvi <- filter(df1, grepl("ATVI", Symbols))
atvi$index <- seq.int(nrow(atvi))

avgo <- filter(df1, grepl("AVGO", Symbols))
avgo$index <- seq.int(nrow(avgo))

### runing sentiment for each company:
## aal
aal_token = aal %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
affin_aal = aal_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(aal)

## aapl
aapl_token = aapl %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
affin_aapl = aapl_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(aapl)

## adbe
adbe_token = adbe %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
affin_adbe = adbe_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(adbe)

## adp
adp_token = adp %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
affin_adp = adp_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(adp)

### adsk
adsk_token = adsk %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
affin_adsk = adsk_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(adsk)


### akam
akam_token = akam %>% 
  select(index, `Tweet content`) %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
affin_akam = akam_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(akam)


