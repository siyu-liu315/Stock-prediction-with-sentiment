library(ggploty)
install.packages("forecast")
install.packages("ggfortify")
install.packages("docstring")
install.packages("here")
install.packages("packrat")
library(packrat)
library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
library(tidyverse)
library(factoextra)
install.packages("factorEx")
library(factorEx)
final1 = read_xlsx("2015-sentiment.xlsx")
final1$average = final$average * 13000

a = ggplot(data = final1, aes(x=Date, y=average))+
  geom_line()
ggploty

data = read.csv("S&P500Index.csv")
glimpse(tem)

NEW = transform(data, date = as.Date(as.character(data$caldt), "%Y%m%d"))

write.csv(NEW, file = "S&P 500 with correct date.csv")

glimpse(NEW)

plot_decompose
