# Simple moving average (SMA) with rolling window
moving_avg <- function(prices, win_size = 50) {
  # prices: numeric vector of prices
  # win_size: window length (e.g. 20, 60, ...)

  n <- length(prices)
  ma <- rep(NA_real_, n)

  if (win_size > n) {
    warning("Window size is larger than length of price series.")
    return(ma)
  }

  for (i in win_size:n) {
    ma[i] <- mean(prices[(i - win_size + 1):i], na.rm = TRUE)
  }

  return(ma)
}

# Moving Average Crossover signal (long / short / flat)
ma_signal <- function(price, fast = 20, slow = 60) {
  # price: numeric vector of prices (e.g. closing prices)
  # fast: window size for fast MA
  # slow: window size for slow MA

  if (fast >= slow) {
    stop("Fast window must be smaller than slow window.")
  }

  fast_ma <- moving_avg(price, win_size = fast)
  slow_ma <- moving_avg(price, win_size = slow)

  # +1 = long, -1 = short, 0 = neutral
  signal <- ifelse(fast_ma > slow_ma,  1,
                   ifelse(fast_ma < slow_ma, -1, 0))

  return(signal)
}
