library(quantmod)
library(TTR)

load_stock_data <- function(file = "portfolio.txt") {
  symbols <- readLines(file)
  stock_data <- list()
  
  for (symbol in symbols) {
    stock_data[[symbol]] <- getSymbols(
      symbol,
      src = "yahoo",
      auto.assign = FALSE
    )
  }
  
  return(stock_data)
}

stocks <- load_stock_data()
calculate_statistics <- function(stock) {
  prices <- as.numeric(Cl(stock))
  
  mode_value <- as.numeric(
    names(sort(table(prices), decreasing = TRUE)[1])
  )
  
  data.frame(
    Mean = mean(prices, na.rm = TRUE),
    Mode = mode_value,
    Median = median(prices, na.rm = TRUE),
    Standard_Deviation = sd(prices, na.rm = TRUE),
    Moving_Average_20 = as.numeric(tail(SMA(prices, n = 20), 1))
  )
}

statistics <- lapply(stocks, calculate_statistics)
statistics
# Display stock data and visualizations
for (symbol in names(stocks)) {
  print(paste("Stock Data:", symbol))
  print(head(stocks[[symbol]]))
  
  chartSeries(
    stocks[[symbol]],
    name = paste(symbol, "Stock Price"),
    theme = "white"
  )
  
  addSMA(n = 20)
}