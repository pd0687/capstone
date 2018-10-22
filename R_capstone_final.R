library(ggplot2)


levels <- read.csv("levels2.csv", header = TRUE)
levels <- levels[1:nrow(levels) - 1, ] # remove last row (not quarterly)
days <- as.Date(levels$dd.mmm.yy, "%d-%b-%y") # coercing to "date"
total_res <- 0
res_loop <- 0
count <- 0 # count the number of iterations
rw <- c(4, 8, 12) # rolling window
RMSE_vector <- rep(0, length(rw) * (ncol(levels) - 3 + 1))
MAPE_vector <- rep(0, length(rw) * (ncol(levels) - 3 + 1))
df_list <- list()
ggplots <- list()


#####################################################################
####### looping over the 4-, 8-, and 12-month rolling windows #######
#####################################################################


for (i in rw) {
  
  days_minus_rw <- days[(i + 1):length(days)]
  
  predict_vector <- rep(0, nrow(levels) - i)
  
  len <- as.numeric(nrow(levels)) - i


#####################################################################
####### looping over 5 countries/regions in the levels dataset ######
#####################################################################

  
    for (j in 3:ncol(levels)) {
 
      levels_country <- ts(levels[,j], frequency = 4)
      
      actual_vector <- levels_country[(i + 1):length(levels_country)]


#####################################################################
################## looping through the time series ##################
#####################################################################


      for (k in 1:len) {
        
        country_rw_Q <- levels_country[k:(i+k-1)] / 100
        
        country_train <- country_rw_Q[1:length(country_rw_Q)-1]
        
        AR_model <- suppressWarnings(tryCatch(arima(country_train,
                    order = c(1, 0, 0), method = "ML",
                    optim.control = list(maxit = 2000)),
                    error = function(x)
                      arima(country_train, order = c(1, 0, 0),
                            method = "ML",
                            optim.control = list(maxit = 2000),
                            transform.pars = FALSE)))
      
        res_loop <- as.numeric(country_train[length(country_train)] - 
                    predict(AR_model)$pred)

        predict_vector[k] <- predict(AR_model)$pred
        
        total_res <- total_res + res_loop^2
        
        count <- count + 1
        
              }
      

      
      ij_iter <- (j - 2) + (ncol(levels) - 3 + 1) * (which(i == rw) - 1)
      RMSE_vector[ij_iter] <- sqrt(total_res / count)
      predict_vector <- predict_vector * 100
      MAPE_vector[ij_iter] <- rowMeans(abs((t(actual_vector) -
                              t(predict_vector)) /
                              t(actual_vector)) * 100)
      df <- as.data.frame(cbind(days_minus_rw, predict_vector,
            actual_vector))
      df$days_minus_rw <- as.Date(df$days_minus_rw,
            origin = "1970-01-01")
      df_list[[ij_iter]] <- df
      ggplots <- lapply(df_list,function(x) ggplot(x,
                 aes(x = days_minus_rw, y = predict_vector,
                 color = "blue")) +
                 geom_point() + 
                 geom_line(aes(y = actual_vector, color = "red")) +
                 xlab("Time") + ylab("Levels"))
      total_res <- 0 # re-initializing total_res
      res_loop <- 0 # re-initializing res_loop
      count <- 0 # re-initializing count
      
          }
  

}


#####################################################################
# couldn't dyamically create titles in main loop, but this loop does
#####################################################################


for (i in rw) {
  for (j in 3:ncol(levels)) {
    ij_iter <- (j - 2) + (ncol(levels) - 3 + 1) * (which(i == rw) - 1)
    ggplots[[ij_iter]] <- ggplots[[ij_iter]] +
                          ggtitle(paste(colnames(levels)[j]," ",
                          i,"-Month", sep = "")) +
                          theme(plot.title = element_text(hjust = 0.5))
  }
}

