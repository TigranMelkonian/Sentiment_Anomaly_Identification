
#calculates the mean of values greater than the .998 quantile of values -> provides a robust max
IQRmean <- function(data, quant = .998){
  sum = 0
  count = 0
  for (i in 1:length(data)){
    if (data[i] > quantile(data, quant)[[1]]){
      sum = sum + data[i]
      count = count +1
    }
  }
  return(sum/count)
} 

#Smothed z-score algorithm 
#The algorithm takes into consideration 3 inputs: lag(static, set to 30 days) = the lag of the moving window, 
#threshold(dynamic, is relative for each celebrity) = the z-score at which the algorithm
#signals and influence(static, set to 1 because crimson sentiment data is non-stationary, 
#i.e. (variable mean and variance over time)) = the influence (between 0 and 1) of new signals 
#on the local (within lag) mean and standard deviation.
ThresholdingAlgo <- function(y,sentiment_dates,celebrity_id,threshold,lag = 30,influence = 1) {
  signals <- rep(0,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag])
  stdFilter[lag] <- sd(y[0:lag])
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 1;
      } else {
        signals[i] <- -1;
      }
      filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i])
    stdFilter[i] <- sd(filteredY[(i-lag):i])
  }
  return(list("celebrityid" = celebrity_id,"sentiment_date"=sentiment_dates, "signals"=signals))
}
