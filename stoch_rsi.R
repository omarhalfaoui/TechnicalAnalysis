# Stochastic RSI (StochRSI) function
stoch_rsi <- function(data, period, k_period, d_period) {
  
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  
  # Remove NA values before finding minimum and maximum RSI
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  
  # Calculate the StochRSI
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  k_values <- (valid_rsi - min_rsi) / (max_rsi - min_rsi)
  
  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)
  
  # Calculate the %D line
  d_line <- sma(k_line, d_period)
  
  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}