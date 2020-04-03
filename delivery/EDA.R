library(DataExplorer)
library(skimr)
library(tidyverse)
library(amelie)
install.packages("quantmod")
install.packages("openair")
library(openair) 

library(quantmod)
install.packages("amelie")

sp500 <- read.csv(file = "^GSPC.csv") 

View(stock)
plot_histogram(sp500)
summary(sp500)

#### split sp500 information by year
split <- split(sp500, format(as.Date(sp500$Date), "%Y"))

ggplot(data = split$`2011`, aes(x = Date, y = Close)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2011 S&P500 Price")

ggplot(data = split$`2012`, aes(x = Date, y = Close)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2012 S&P500 Price")

ggplot(data = split$`2013`, aes(x = Date, y = Close)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2013 S&P500 Price")

ggplot(data = split$`2014`, aes(x = Date, y = Close)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2014 S&P500 Price")

ggplot(data = split$`2015`, aes(x = Date, y = Close)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2015 S&P500 Price")
ggplot(data = split$`2016`, aes(x = Date, y = Close)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2016 S&P500 Price")

ggplot(data = split$`2017`, aes(x = Date, y = Close)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2017 S&P500 Price")

ggplot(data = split$`2018`, aes(x = Date, y = Close)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2018 S&P500 Price")

ggplot(data = split$`2019`, aes(x = Date, y = Close)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2019 S&P500 Price")






