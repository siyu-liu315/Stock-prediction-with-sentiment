names(df1)
try <- head(df1)
View(try)

library(tidyverse)

names(df1)

### split the rtweet data by 

aal <- filter(df1, grepl("AAL", Symbols))
aapl <- filter(df1, grepl("AAPL", Symbols))
adbe <- filter(df1, grepl("ADBE", Symbols))
adp <- filter(df1, grepl("ADP", Symbols))
adsk <- filter(df1, grepl("ADSK", Symbols))
akam <- filter(df1, grepl("AKAM", Symbols))
alxn <- filter(df1, grepl("ALXN", Symbols))
amat <- filter(df1, grepl("AMAT", Symbols))
amgn <- filter(df1, grepl("AMGN", Symbols))
amzn <- filter(df1, grepl("AMZN", Symbols))
atvi <- filter(df1, grepl("ATVI", Symbols))
avgo <- filter(df1, grepl("AVGO", Symbols))
