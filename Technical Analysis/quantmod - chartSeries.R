library('tidyverse')
library('quantmod')
library('TTR')
library('PerformanceAnalytics')
library('RColorBrewer')
library('tseries')
library('Quandl')
library('lubridate')

Quandl.api_key(YOUR_API_KEY)

#Pull stock information for MSFT and TSLA
MSFT <- getSymbols('MSFT', auto.assign = FALSE)
TSLA <- getSymbols('TSLA', auto.assign = FALSE)

# Get the daily returns for TSLA
tsla_daily_returns <- dailyReturn(TSLA$TSLA.Adjusted)
head(tsla_daily_returns)

plot(tsla_daily_returns)

# Create a line chart for TSLA starting from 2021-05-29
chartSeries(TSLA,
            type='candlesticks',
            subset = '2021-05-29::',
            theme = chartTheme('white'),
            )

# Now, add bollinger bands, RSI, and exponential moving average to the chart
chartSeries(TSLA,
            subset = '2021::',
            TA = c(addBBands(n = 20, sd=2 ), addRSI(), addEMA(n = 20)),
            theme = chartTheme('white')
            )

# Then add the open to close price change
addTA(OpCl(TSLA),col='red', type='h')
