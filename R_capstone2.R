levels <- read.csv("levels2.csv")
rw <- 8 # rolling window
days <- as.Date(levels$dd.mmm.yy, "%d-%b-%y")
days_minus_rw <- days[(rw + 1):length(days)]
# levels_Canada <- as.ts(levels[,3])
levels_Canada <- ts(levels[,3], frequency = 4)
actual_vector <- levels_Canada[(rw + 1):length(levels_Canada)]
total_res <- 0
total_AIC <- 0
count <- 0
predict_vector <- rep(0, length(levels_Canada)-rw)
len <- as.numeric(length(levels_Canada))-rw
for (i in 1:len) {
  print(i)
  Canada_rw_Q <- levels_Canada[i:(rw+i-1)]
  Canada_train <- Canada_rw_Q[1:length(Canada_rw_Q)-1]
  
  # AR_Canada <- arima(Canada_train, order = c(1, 0, 0))
  # Error in arima(Canada_train, order = c(1, 0, 0)) : 
  #   non-stationary AR part from CSS
  
  # https://stackoverflow.com/questions/7233288/non-stationary-seasonal-ar-part-from-css-error-in-r
  AR_Canada <- arima(Canada_train, order = c(1, 0, 0), method = "ML")

  res_loop <- as.numeric(Canada_train[length(Canada_train)] - predict(AR_Canada)$pred)
  predict_vector[i] <- predict(AR_Canada)$pred
  total_res <- total_res + res_loop^2

  model_AIC <- AR_Canada$aic
  total_AIC <- total_AIC + model_AIC
  
  print(total_res)
  print(model_AIC)

  count <- count + 1
}

AIC_avg <- total_AIC / count
RMSE <- sqrt(total_res) / count

df <- as.data.frame(cbind(days_minus_rw, predict_vector, actual_vector))
df$days_minus_rw <- as.Date(df$days_minus_rw, origin = "1970-01-01")

ggplot(df, aes(x = days_minus_rw, y = predict_vector)) + 
  geom_point(color = "blue") +  
  geom_line(aes(y = actual_vector, color = "red")) + 
  theme(legend.position = "none")