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
basic = read_csv("Combined_News_DJIA.csv")
glimpse(basic)
View(basic)

basic$index = seq(1:1989)


### doing one by one
sent_token1 = basic %>% 
  select(index, Top1) %>% 
  unnest_tokens(token, Top1, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word")) %>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word")) %>% print
### put the sentiment back to original data
affin_data1 = sent_token1 %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(basic)
