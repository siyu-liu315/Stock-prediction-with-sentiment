library(zoo)
library(openair)
library(ggplot2)
sp500 <- read.csv(file = "S&P500Index.csv")
View(sp500)


##### transfomr numbr to date
sp <- transform(sp500, date = as.Date(as.character(sp500$caldt), "%Y%m%d"))
View(sp)

### Overview
ggplot(data = sp, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1990 - 2019 S&P500 Price")

split <- split(sp, format(as.Date(sp$date), "%Y"))
split
View(split)

ggplot(data = split$`1990`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1990 S&P500 Price")

ggplot(data = split$`1991`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1991 S&P500 Price")

ggplot(data = split$`1992`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1992 S&P500 Price")

ggplot(data = split$`1993`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1993 S&P500 Price")

ggplot(data = split$`1994`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1994 S&P500 Price")

ggplot(data = split$`1995`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1995 S&P500 Price")

ggplot(data = split$`1995`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1995 S&P500 Price")

ggplot(data = split$`1995`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1995 S&P500 Price")


ggplot(data = split$`1996`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1996 S&P500 Price")

ggplot(data = split$`1997`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1997 S&P500 Price")

ggplot(data = split$`1998`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1998 S&P500 Price")

ggplot(data = split$`1999`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "1999 S&P500 Price")

ggplot(data = split$`2000`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2000 S&P500 Price")

ggplot(data = split$`2001`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2001 S&P500 Price")

ggplot(data = split$`2002`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2002 S&P500 Price")

ggplot(data = split$`2003`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2003 S&P500 Price")

ggplot(data = split$`2004`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2004 S&P500 Price")

ggplot(data = split$`2005`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2002 S&P500 Price")

ggplot(data = split$`2005`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2002 S&P500 Price")

ggplot(data = split$`2006`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2006 S&P500 Price")

ggplot(data = split$`2007`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2007 S&P500 Price")

ggplot(data = split$`2008`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2008 S&P500 Price")

ggplot(data = split$`2009`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2009 S&P500 Price")

ggplot(data = split$`2010`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2010 S&P500 Price")

ggplot(data = split$`2011`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2011 S&P500 Price")

ggplot(data = split$`2012`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2012 S&P500 Price")

ggplot(data = split$`2013`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2013 S&P500 Price")

ggplot(data = split$`2014`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2014 S&P500 Price")

ggplot(data = split$`2015`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2015 S&P500 Price")

ggplot(data = split$`2016`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2016 S&P500 Price")

ggplot(data = split$`2017`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2017 S&P500 Price")

ggplot(data = split$`2018`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2018 S&P500 Price")

ggplot(data = split$`2019`, aes(x = date, y = spindx)) +
  geom_line(color = "#FC4E07") +
  labs(title = "2019 S&P500 Price")
