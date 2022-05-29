library('tidyverse')
library('quantmod')
library('TTR')
library('PerformanceAnalytics')
library('RColorBrewer')
library('tseries')
library('Quandl')
library('lubridate')

Quandl.api_key(YOUR_API_KEY)

# Pull stock information from Microsoft
MSFT <- getSymbols('MSFT', auto.assign = FALSE)
head(MSFT)

# Check how many days we have in the dataframe
dim(MSFT)

# What is the object type of MSFT
str(MSFT)

# Convert the xts object into a dataframe so that we can transform it more easily
MSFT <- as.data.frame(MSFT)
str(MSFT)

# What are the starting and the end date of the dataset?
index(MSFT)[1]
index(MSFT)[nrow(MSFT)]

# Get the average values for the open and the close values for the whole range
MSFT %>%
  summarise(avg_open = mean(MSFT.Open, na.rm =T), avg_close = mean(MSFT.Close, na.rm =T) )

# What are the lowest and the highest value of MSFT in the dataframe?
MSFT %>%
  filter(MSFT.Low == min(MSFT.Low, na.rm = T) | MSFT.High == max(MSFT.High, na.rm = T)) %>%
  select(MSFT.Low, MSFT.High)

# What percentage did MSFT changed from the beginning to the end date?
(MSFT[nrow(MSFT), "MSFT.Close"] - MSFT[1, "MSFT.Close"]) / MSFT[1, "MSFT.Close"] * 100
# It's is 815% percent!

# How many days were the close value higher than the open value?
nrow(filter(MSFT, MSFT.Close > MSFT.Open))

# Convert MSFT back into xst and in addition, pull TSLA information and combine it with MSFT daily returns
MSFT <- as.xts(MSFT)
TSLA <- getSymbols('TSLA', auto.assign = F)

msft_daily_returns <- dailyReturn(MSFT$MSFT.Adjusted)
head(msft_daily_returns)

# Display the daily returns of MSFT
plot(msft_daily_returns, xlab='date', ylab='daily return')

# Now, combine the daily returns of both MSFT and TSLA. Note that as the TSLA's stock values start later, we need to keep all = FALSE.
tsla_daily_returns <- dailyReturn(TSLA$TSLA.Adjusted)
comb <- merge(msft_daily_returns, tsla_daily_returns, all = FALSE)
head(comb)

# rename the columns of comb
names(comb)[1] <- 'MSFT.daily.returns'
names(comb)[2] <- 'TSLA.daily.returns'

# show the performance summary
charts.PerformanceSummary(comb, main = 'MSFT vs TSLA')

# Find the sharp ratio for MSFT and TSLA
table.AnnualizedReturns(comb, scale = 252, Rf = .004 / 252)
