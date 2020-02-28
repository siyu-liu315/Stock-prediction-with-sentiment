library(openair)
library(ggplot2)
data <- read.csv("AAL.csv")
names(data)

ggplot() +
  geom_line(data = data, aes(x = Date,  y = Low, group = 1), color = "red") +
  geom_line(data = data, aes(x = Date, y = High, group =1), color = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "American Airline Stock Price Trend in Selected period", 
       subtitle = "Blue - High Price, Red - Low Price")
  
