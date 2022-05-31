library('tidyverse')
library('quantmod')
library('PerformanceAnalytics')
library('Quandl')
library('crypto')
options('getSymbols.yahoo.warning' = FALSE)

Quandl.api_key(YOUR_API_KEY)

MSFT <- getSymbols('MSFT', auto.assign = FALSE)
head(MSFT)

price <- Cl(MSFT) # close price
r <- price / Lag(price) - 1 # price change 
delta <- -0.02 # threshold 
signal <- c(0) # as the first day has no signal

for (i in 2:length(price)){
  if (r[i] < delta){
    signal[i] <- 1  # buy signal
  } else signal[i] <- 0 # do not buy 
}

# change the close prices with 0's and 1's
signal <- reclass(signal, price)
tail(signal, 20)

ret <- dailyReturn(MSFT)*Lag(signal, 1)
tail(ret, 20)

# rename the column of ret
names(ret) <- 'Simple'

# plot the cumulative return and drawdown
charts.PerformanceSummary(ret)

# how many days did we have positive return with respect to threshold?
ret_df <- as.data.frame(ret)
ret_df %>% 
  filter(index(ret) > 3000,  Simple != 0) %>%
  count()

# How much percentage would we profit from the returns over the last two years 
total_ret <- 1
col_len <- length(new_ret$perc)
col_len-365*2

new_ret <- ret_df %>%
  mutate(perc = 1 + Simple)

tail(new_ret)
total_ret <- 1
for (i in new_ret$perc[col_len-(365*2):col_len]){
    total_ret <- total_ret * i  
}
total_ret

## build the RSI filter
# build the RSI index for 14 days 
day <- 14
price <- Cl(MSFT)
RSI <- RSI(price, day)
head(RSI, 20)

# set the signal zero for the first 14 days, and 1 or 0 with respect to RSI 
# for the rest of the days 
signal <- c(0)
signal[1:day + 1] <- 0

for (i in (day + 1):(length(price))){
  if (RSI[i] < 50){
    signal[i] <- 1
  } else {signal[i] <- 0}
}
head(signal)

# replace the closing prices with the signal values
signal <- reclass(signal, Cl(MSFT))
head(signal)

# calculate returns for the days when the signal is 1 
RSI_return <-  dailyReturn(MSFT)* Lag(signal, 1)
tail(RSI_return)

# how many days did the RSI return give nonzero value?
RSI_return_df <- as.data.frame(RSI_return)
RSI_return_df %>%
  filter(daily.returns != 0) %>%
  count()
# It's 59!

# rename the display.returns column
names(RSI_return) <- 'RSI Algorithm'

# display the cumulative return
charts.PerformanceSummary(RSI_return)

# compare the performance of Simple and RSI  
ret_comb <- cbind(ret, RSI_return)
tail(ret_comb, 50)

charts.PerformanceSummary(ret_comb, main='RSI Filter vs Simple Filter Performance')

# combine the RSI dataframe with daily return
daily_ret_df <- as.data.frame(dailyReturn(MSFT))
RSI_df <- as.data.frame(RSI)
rsi_return <- merge(RSI_df, daily_ret_df, by='row.names', all=TRUE)

# find the days where the rsi is below 30
indexes <- which(rsi_return$rsi < 30) + 1 # add 1 ass we will sell one day after 

return_rsi_30 <- daily_ret_df$daily.returns[indexes]
head(return_rsi_30)

# find the cumulative profit
cum_ret_30 <- 1
for (r in return_rsi_30[30:59]){
  r <- 1 + r
  cum_ret_30 <- cum_ret_30 * r
}

cum_ret_30 # equals to 1.29

charts.PerformanceSummary(return_rsi_30)

str(return_rsi_30)
