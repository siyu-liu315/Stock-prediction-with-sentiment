options(stringsAsFactors = F)

library(readxl)
library(tidyverse)
library(lubridate)
library(prophet)
library(corrplot)
library(skimr)
library(openair)
sp500 <- read.csv(file = "^GSPC.csv")
df <- read.csv('S&P500Index.csv')
index <- read.csv('marketindexinfo.csv')

###modeling
sp500$Date = as.Date(sp500$Date)
### first perdiction with July 
sp500 %>% select(Date, Close) %>% 
  rename(ds = Date, y = Close)-> close

m <-  prophet()
m <- fit.prophet(m, close)

future60 = make_future_dataframe(m, 60)

# fit the model to future observations
forecast = predict(m, future60)
## delete predictions for weekends 

dyplot.prophet(m, forecast)
future60_yhat = forecast %>% select(ds, yhat) %>% tail(60)
View(future60_yhat)
