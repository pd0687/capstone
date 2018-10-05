levels <- read.csv("levels2.csv")
# levels_Canada <- as.ts(levels[,3])
levels_Canada <- ts(levels[,3], frequency = 4)
total_res <- 0
total_AIC <- 0 # initialize AIC
count <- 0
len <- as.numeric(length(levels_Canada))-8

for (i in 1:len) {
  print(i)
  Canada_8Q <- levels_Canada[i:(7+i)]
  Canada_train <- Canada_8Q[1:length(Canada_8Q)-1]
  
  # AR_Canada <- arima(Canada_train, order = c(1, 0, 0))
  # Error in arima(Canada_train, order = c(1, 0, 0)) : 
  #   non-stationary AR part from CSS
  
  # https://stackoverflow.com/questions/7233288/non-stationary-seasonal-ar-part-from-css-error-in-r
  AR_Canada <- arima(Canada_train, order = c(1, 0, 0), method = "ML")
  
  res_loop <- as.numeric(Canada_train[length(Canada_train)] - predict(AR_Canada)$pred)
  total_res <- total_res + res_loop^2 # square the residual

  model_AIC <- AR_Canada$aic # compute AIC
  total_AIC <- total_AIC + model_AIC # aggregate AIC
  
  print(total_res)
  print(model_AIC) # print AIC

  count <- count + 1
}

AIC_avg <- total_AIC / count # average the AIC
RMSE <- sqrt(total_res) / count
