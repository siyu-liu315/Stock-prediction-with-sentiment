options(stringsAsFactors = F)

library(readxl)
library(tidyverse)
library(lubridate)
library(prophet)
library(corrplot)
library(skimr)
library(openair)

sp500 <- read.csv('timeseries_analysis/S&P500Index.csv')
nyse <- read.csv('timeseries_analysis/NYSE.csv')
nasdaq <- read.csv('timeseries_analysis/NASDAQ.csv')
DJ <- read.csv('timeseries_analysis/NASDAQ.csv')

##int_to_date <- function(df,int){
##  df <- transform(df, date = as.Date(as.character(int), "%Y%m%d"))
##}
##int_to_date(sp500,sp500$caldt)


sp500 <- transform(sp500,date = as.Date(as.character(sp500$caldt), "%Y%m%d"))[-1]

###modeling
nyse$Date = as.Date(nyse$Date)
nasdaq$Date = as.Date(nasdaq$Date)
DJ$Date = as.Data(DJ$Date)

### first perdiction with July 
sp500 %>% select(date, spindx) %>% 
  rename(ds = date, y = spindx)-> sp500df

nyse %>% select(Date, Close) %>% 
  rename(ds = Date, y = Close)-> nysedf

sp <-  prophet()
sp <- fit.prophet(sp, sp500df)
nysem <- prophet()
nysem <- fit.prophet(nysem,nysedf) 

future60 = make_future_dataframe(sp, 60)
future60nyse <- make_future_dataframe(nysem,60)


# fit the model to future observations
forecast = predict(sp, future60)
forecast = predict(nysem, future60nyse)
## delete predictions for weekends 

dyplot.prophet(sp, forecast)
dyplot.prophet(nysem, forecast)


future60_yhat = forecast %>% select(ds, yhat) %>% tail(60)
View(future60_yhat)


