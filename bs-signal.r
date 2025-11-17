# Moving Average Calculations
#Setting a fixed parameter makes it a default value and it can still vary if the user inputs a different value
moving_avg <- function(prices, win_size = 50){ 

    ma <- prices * NA 

    for(i in win_size:length(prices)) {
        ma[i] <- sum(prices[(i - win_size + 1):i]) / win_size
    }

    return(ma)
}

# Moving Average Signal
ma_signal <- function(price = pfe$prccd, fast = 20, slow = 60) {
  fast_ma <- moving_avg(price, win_size = fast)
  slow_ma <- moving_avg(price, win_size = slow)
  
  signal <- ifelse(fast_ma > slow_ma,  1,
                   ifelse(fast_ma < slow_ma, -1, 0))
  
  return(signal)

}






