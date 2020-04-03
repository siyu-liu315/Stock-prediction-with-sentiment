##############################################################################
## Getting started with Text Analytics
##
## Learning goals:
##                 - text as a datasource
##                 - cleaning text
##                 - basic eda
##                 - Doc Term Matrix representation by hand
##                 - how text can be used in ML
##############################################################################

options(stringsAsFactors = FALSE)
options(digits = 3)
options(scipen = 999)  ## remove scientific notation - but need width

## load the packages
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
library(quanteda.corpora) ## for datasets 


############################################### Introduction to text

## lets start simple, bring in text101
data = read_csv("Combined_News_DJIA.csv")
glimpse(data)


data$index = seq(1:1989)
data = data %>% select(index, Top1)

glimpse(data)

data %>% arrange(index)
nrow(basic)
dim(data)

### doing top1
nfl_bigrams = data %>% 
  select(index, Top1) %>% 
  unnest_tokens(word, Top1, token="ngrams", n=5)

nfl_bigrams %>% 
  count(word, sort=T) %>% 
  print(n=20)

nfl_tokens = data %>% 
  unnest_tokens(word, Top1, strip_punct=T) %>% 
  anti_join(get_stopwords())

nfl_tokens %>% 
  count(word, sort=T) %>% 
  print(n=25)

nfl_sent = nfl_tokens %>% 
  inner_join(get_sentiments("afinn"))
sample_n(nfl_sent, 5)

ggplot(data = nfl_sent,aes(x=word, y=value)) +
  geom_col()


# ### Top2
# sent_token1 = basic %>% 
#   select(index, Top2) %>% 
#   unnest_tokens(token, Top2, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# affin_data1 = sent_token1 %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity2 = mean(value)) %>% 
#   inner_join(affin_data1)
# ### Top3
# sent_token1 = basic %>% 
#   select(index, Top3) %>% 
#   unnest_tokens(token, Top3, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# affin_data1 = sent_token1 %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity3 = mean(value)) %>% 
#   inner_join(affin_data1)
# 
# ### Top4
# sent_token1 = basic %>% 
#   select(index, Top4) %>% 
#   unnest_tokens(token, Top4, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# affin_data1 = sent_token1 %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity4 = mean(value)) %>% 
#   inner_join(affin_data1)
# 
# ### Top5
# sent_token1 = basic %>% 
#   select(index, Top5) %>% 
#   unnest_tokens(token, Top5, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# affin_data1 = sent_token1 %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity5 = mean(value)) %>% 
#   inner_join(affin_data1)
# 
# ### Top6
# sent_token1 = basic %>% 
#   select(index, Top6) %>% 
#   unnest_tokens(token, Top6, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# affin_data1 = sent_token1 %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity6 = mean(value)) %>% 
#   inner_join(affin_data1)
# 
# ### Top7
# sent_token1 = basic %>% 
#   select(index, Top7) %>% 
#   unnest_tokens(token, Top7, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# affin_data1 = sent_token1 %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity7 = mean(value)) %>% 
#   inner_join(affin_data1)
# 
# ### Top8
# sent_token1 = basic %>% 
#   select(index, Top8) %>% 
#   unnest_tokens(token, Top8, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# affin_data1 = sent_token1 %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity8 = mean(value)) %>% 
#   inner_join(affin_data1)
# 
# ### Top9
# sent_token1 = basic %>% 
#   select(index, Top9) %>% 
#   unnest_tokens(token, Top9, token = "words",
#                 strip_punct = T) %>% 
#   anti_join(get_stopwords(),
#             by = c("token" ="word")) %>% 
#   inner_join(get_sentiments("afinn"),
#              by = c("token" ="word")) %>% print
# ### put the sentiment back to original data
# affin_data1 = sent_token1 %>% select(index, value) %>% 
#   group_by(index) %>% 
#   summarise(polarity9 = mean(value)) %>% 
#   inner_join(affin_data1)

### trying to use reapeat loop sentiment each top
# x <- 3
# repeat {
#   sent_token1 = basic[,x] %>% 
#     unnest_tokens(token, x, token = "words",
#                   strip_punct = T) %>% 
#     anti_join(get_stopwords(),
#               by = c("token" ="word")) %>% 
#     inner_join(get_sentiments("afinn"),
#                by = c("token" ="word")) %>% mutate(index = seq(1:1989))
#   ### put the sentiment back to original data
#   affin_data1 = sent_token1 %>% select(index, value) %>% 
#     group_by(index) %>% 
#     summarise(polarity1 = mean(value)) %>% 
#     inner_join(basic)
#   if (x == basic[,27]){
#     break
#   }
# }
