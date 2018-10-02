levels <- read.csv("levels2.csv")
levels_Canada <- as.ts(levels[,3])
total_res <- 0
count <- 0
len <- as.numeric(length(levels_Canada))-8

for (i in 1:len)
{
Canada_8Q <- levels_Canada[i:(7+i)]
Canada_train <- Canada_8Q[1:length(Canada_8Q)-1]
AR_Canada <- arima(Canada_train, order = c(1, 0, 0))
res_loop <- as.numeric(Canada_train[length(Canada_train)] - predict(AR_Canada)$pred)
total_res <- total_res + res_loop
count <- count + 1
}

RMSE <- sqrt(total_res) / count