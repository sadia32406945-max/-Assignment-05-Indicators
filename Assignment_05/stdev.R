stdev <- function(data) {
  n <- length(data)
  mean_value <- sum(data) / n
  
  diff_values <- numeric(n)
  for (i in 1:n) {
    diff_values[i] <- data[i] - mean_value
  }
  
  squared_diff <- numeric(n)
  for (i in 1:n) {
    squared_diff[i] <- diff_values[i] * diff_values[i]
  }
  
  variance <- sum(squared_diff) / length(squared_diff)
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}