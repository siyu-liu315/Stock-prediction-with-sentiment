data <- read_csv("portfolio_data.csv")
data <- data[-c(1)]
data <- data %>% select(X1_1, everything())
# Deleting any prices from around the Corona Virus
data <- data[-c(200:505)]
data <- data[-c(2:130)]


## Split data for portfolio and for selecting model dropping ticker symbol and the tag 
smp_size <- floor(0.5 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
model_data <- data[train_ind, ]
model_data <- model_data[-c(1)]

###


port_data <- data[-train_ind, ]


## Splt into Train Test
smp_size <- floor(0.75 * nrow(model_data))
train_ind <- sample(seq_len(nrow(model_data)), size = smp_size)
train <- model_data[train_ind, ]
test <- model_data[-train_ind, ]
test_real <- as.vector(test[[c(1)]])

data_long <- pivot_longer(data,cols = -X1_1,names_to = 'date',values_to = 'price' )

data <- data %>% select(X1_1, everything())
# Deleting any prices from around the Corona Virus

