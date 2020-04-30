data <- read_csv("portfolio/portfolio_data.csv")
data[2:104] <- scale(data[2:104])
data <- as.data.frame(t(data))
names(data) <- as.matrix(data[1, ])
data <- data[-1,]
data <- na.omit(data)
data <- data[,order(ncol(data):1)]

View(data)
## Split data for portfolio and for selecting model dropping ticker symbol and the tag

smp_size <- floor(0.5 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
model_data <- data[train_ind, ]
model_data <- model_data[-c(1)]
port_data <- data[-train_ind, ]


## Splt into Train Test
smp_size <- floor(0.75 * nrow(model_data))
train_ind <- sample(seq_len(nrow(model_data)), size = smp_size)
train <- model_data[train_ind, ]
test <- model_data[-train_ind, ]
test_real <- as.vector(test[[c(1)]])



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

