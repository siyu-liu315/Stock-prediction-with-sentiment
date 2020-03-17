names(df1)
try <- head(df1)
View(try)

library(tidyverse)

names(df1)

### split the rtweet data by 

aal <- filter(df1, grepl("AAL", Symbols))
aal$index <- seq.int(nrow(aal))

aapl <- filter(df1, grepl("AAPL", Symbols))
aapl$index <- seq.int(nrow(aapl))

adbe <- filter(df1, grepl("ADBE", Symbols))
adbe$index <- seq.int(nrow(adbe))

adp <- filter(df1, grepl("ADP", Symbols))
adp$index <- seq.int(nrow(adp))

adsk <- filter(df1, grepl("ADSK", Symbols))
adsk$index <- seq.int(nrow(adsk))

akam <- filter(df1, grepl("AKAM", Symbols))
akam$index <- seq.int(nrow(akam))

alxn <- filter(df1, grepl("ALXN", Symbols))
alxn$index <- seq.int(nrow(alxn))

amat <- filter(df1, grepl("AMAT", Symbols))
amat$index <- seq.int(nrow(amat))

amgn <- filter(df1, grepl("AMGN", Symbols))
aal$index <- seq.int(nrow(aal))

amzn <- filter(df1, grepl("AMZN", Symbols))
amzn$index <- seq.int(nrow(amzn))

atvi <- filter(df1, grepl("ATVI", Symbols))
atvi$index <- seq.int(nrow(atvi))

avgo <- filter(df1, grepl("AVGO", Symbols))
avgo$index <- seq.int(nrow(avgo))



