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

basic = read_csv("Combined_News_DJIA.csv")
glimpse(basic)


basic$element_id = seq(1:1989)
glimpse(affin_data1)

dim(basic)

# ### Trying repeat loop
# x <- 3
# repeat {
#   aa = get_sentences(basic[,x])
#   bb = sentiment(aa)
#   head(bb)
#   
#   tem3 = bb %>% 
#     group_by(element_id) %>% 
#     summarise(x = mean(sentiment))
#   nrow(tem3) == nrow(basic)
#   if (x == basic[,27]){
#     break
#   }
# }


### Top1
tem = get_sentences(basic$Top1)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity1 = mean(sentiment))
nrow(tem3) == nrow(basic)

basic = inner_join(tem3,basic)

### Top2
tem = get_sentences(basic$Top2)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity2 = mean(sentiment))
nrow(tem3) == nrow(basic)

basic = inner_join(tem3,basic)

### Top3
tem = get_sentences(basic$Top3)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity3 = mean(sentiment))
nrow(tem3) == nrow(basic)

basic = inner_join(tem3,basic)

### Top4
tem = get_sentences(basic$Top4)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity4 = mean(sentiment))
nrow(tem3) == nrow(basic)

basic = inner_join(tem3,basic)

### Top5
tem = get_sentences(basic$Top5)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity5 = mean(sentiment))
nrow(tem3) == nrow(basic)

basic = inner_join(tem3,basic)

### Top6
tem = get_sentences(basic$Top6)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity6 = mean(sentiment))
nrow(tem3) == nrow(basic)

basic = inner_join(tem3,basic)

### Top7
tem = get_sentences(basic$Top7)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity7 = mean(sentiment))
nrow(tem3) == nrow(basic)

basic = inner_join(tem3,basic)

### Top8
tem = get_sentences(basic$Top8)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity8 = mean(sentiment))
nrow(tem3) == nrow(basic)

basic = inner_join(tem3,basic)

### Top9
tem = get_sentences(basic$Top9)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity9 = mean(sentiment))
nrow(tem3) == nrow(basic)

basic = inner_join(tem3,basic)

### Top10
tem = get_sentences(basic$Top10)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity10 = mean(sentiment))
nrow(tem3) == nrow(basic)

basic = inner_join(tem3,basic)

### Top11
tem = get_sentences(basic$Top11)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity11 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top12
tem = get_sentences(basic$Top12)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity12 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top13
tem = get_sentences(basic$Top13)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity13 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top14
tem = get_sentences(basic$Top14)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity14 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top15
tem = get_sentences(basic$Top15)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity15 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top16
tem = get_sentences(basic$Top16)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity16 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top17
tem = get_sentences(basic$Top17)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity17 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top18
tem = get_sentences(basic$Top18)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity18 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top19
tem = get_sentences(basic$Top19)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity19 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top20
tem = get_sentences(basic$Top20)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity20 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top21
tem = get_sentences(basic$Top21)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity21 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top22
tem = get_sentences(basic$Top22)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity22 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top23
tem = get_sentences(basic$Top23)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity23 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top24
tem = get_sentences(basic$Top24)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity24 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### Top25
tem = get_sentences(basic$Top25)
tem2 = sentiment(tem)
head(tem2)

tem3 = tem2 %>% 
  group_by(element_id) %>% 
  summarise(polarity25 = mean(sentiment))
nrow(tem3) == nrow(basic)
basic = inner_join(tem3,basic)

### average the sentiment, but most sentiment score are -
setiment_data = basic %>% select(element_id, polarity25:polarity1) 
setiment_data$average = rowMeans(setiment_data[,2:26])

### clean data by Excel and saved as 2015-sentiments

final = read_xlsx("2015-sentiment.xlsx")
final$Date = as.Date(final$Date)

ggplot(data = final, aes(x=Date, y=average))+
  geom_line()

ggplot(data = final, aes(x=Date, y=Close))+
  geom_line()
