# Simple daily returns from price
simple_returns <- function(price) {
  # price: numeric vector of prices
  # returns: (P_t / P_{t-1}) - 1

  ret <- c(NA_real_, price[-1] / head(price, -1) - 1)
  return(ret)
}

# Backtest for long/short strategy given price and signal
backtest_ma <- function(price, signal) {
  # price: numeric vector of prices
  # signal: numeric vector of positions (-1, 0, +1), same length as price

  if (length(price) != length(signal)) {
    stop("price and signal must have the same length.")
  }

  # 1) Compute daily simple returns
  ret <- simple_returns(price)

  # 2) Lag signal by one day (trade on next day's return)
  signal_lag <- c(NA_real_, head(signal, -1))

  # 3) Strategy returns: position * next day's return
  strat_ret <- signal_lag * ret

  # Set first observation (and any NAs) to 0 return
  strat_ret[is.na(strat_ret)] <- 0

  # 4) Equity curve starting from 1
  equity <- cumprod(1 + strat_ret)

  # 5) Return list so you can access both
  out <- list(
    returns = strat_ret,
    equity  = equity
  )

  return(out)
}
