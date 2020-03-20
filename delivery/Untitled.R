## load the packages
library(tidyverse)
library(readxl)
library(plotly)


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

data888 = readxl::read_excel("~/Desktop/aa.xlsx")
glimpse(data888)
index = (1:length(data888$`Tweet content`))
data888$index = index

aa_token= data888 %>% 
  select(`Tweet content`, index)  %>% 
  unnest_tokens(token, `Tweet content`, token = "words",
                strip_punct = T) %>% 
  anti_join(get_stopwords(),
            by = c("token" ="word"))%>% 
  inner_join(get_sentiments("afinn"),
             by = c("token" ="word"))
sample_n(aa_token, 5)

aa_final = aa_token %>% select(index, value) %>% 
  group_by(index) %>% 
  summarise(polarity = mean(value)) %>% 
  inner_join(data888)
ggplot(aa_final, aes(Date, polarity)) + 
  geom_line() +
  geom_smooth()
