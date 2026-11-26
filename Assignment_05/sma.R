sma <- function(data, period) {
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  num_windows <- length(data) - period + 1
  sma_values <- numeric(num_windows)
  
  for (i in 1:num_windows) {
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    sma_values[i] <- mean_value
  }
  
  return(sma_values)
}