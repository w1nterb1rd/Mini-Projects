#### TIME SERIES ####

#trend -> a given variable fluctuation as trend, eg air purifiers in Delhi
#seasonal -> variable fluctuating due to season, eg sale of AC, short term
#cyclical -> variable that remains same for long term (2-10 years), eg effect of GST on Indian economy
#residuals -> irregularities, variables that show no regular dependence on time
#first bifurcate given data into trend, seasonality, residuals
#perform tests on residuals

getwd()
candyshop <- read.csv("candy_production.csv")
str(candyshop)
table(is.na(candyshop))
dim(candyshop)

install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

install.packages("lubridate")
library(lubridate)
candyshop$observation_date <- mdy(candyshop$observation_date)
str(candyshop)
View(candyshop)
#forecast sales for sep, oct and nov

candyts <- ts(candyshop$candy_production, c(1972,1), c(2017,8), 12)
#coverting it into ts function
# (1972,1), c(2017,8) <- data starts from jan 1972 and ends at aug 2017
# 12 is frequency, that is, the data is monthly data 
candyts
str(candyts)
head(candyts)
sum(is.na(candyts))
frequency(candyts)

plot(candyts, main = "Candy_Production")

#decomposing the plot to know trend, seasonal and irrugularity contribution
decomp <- stl(candyts, s.window = 12, t.window = 12)
# s.window extracts seasonal contribution
# t.window extracts trend contribution
# baaki remainder extracts irregular component
head(decomp$time.series)
plot(stl(candyts, s.window = 12, t.window = 12))
#first graph -> ts graph
#second graph -> seasonality
#third graph -> trend
#fourth graph -> residuals

## checking if time-series is stationary
##Stationarity means :-
# mean, variance, autocorrelation independent of time, i.e., statistical properties remain constant  ## har month ka mean each year same hai
# stationary means pattern of change is same for all years
## separating the irregular component
candyts_residuals <- decomp$time.series[,3]
#[,3] -> all rows under column 3 (residuals)
candyts_residuals

adf.test(candyts_residuals) #test for stationarity
# p value 0.01 < 0.05, null rejected, residuals series is accepted
# h0 : residuals series is not stationary
# hA : residuals series is stationary
# lag order means kitne periods back jaake compare kiya hai, here, 8 months back jaake compare kiya

## Arima Model (p,d,q)
# Yt = B0 + B1Yt-1 + B2Yt-2 + ... + BpYt-p + Error #AR
# q -> MA, moving average, errors ka auto regression  Yt = (theta0) + (theta1)Et-1 + (theta2)Et-2 + ... + (thetap)Et-p
# p -> AR, auto regression on Y
# d -> I,lag order to make series stationary, 0 if stationary
# then club them together
# Examples:-
# if p=2, AR Yt = B0 + B1Yt-1 +B2Yt-2 + Error
# if q=3, MA Yt = (theta0) + (theta1)Et-1 + (theta2)Et-2 + (theta3)Et-3 + Error

#correlograms
acf(candyts_residuals, main = 'Acf plot')      #x axis lag, y axis autocorrelation
pacf(candyts_residuals, main = 'Pacf plot')    # x axis lag, y axis partial autocorrelation
# sticks under blue line (close to 0) are insignificant, sticks crossing blue line are significant
# acf gives value of q (is like a sin curve. q=2)
# conditions of acf: significant till q lags (here, 2)
# pacf gives value of p (here, 0 se neeche wala portion dekho, right side se, after 2 sticks ek significant stick aa rahi, to p = 2)

Model <- arima(candyts, order = c(2,0,2))
Model
accuracy(Model)

## Forecasting ##
forecast_candyts <- forecast(Model, level = c(95), h=3)
forecast_candyts
# c(95) <- confidence level 95%
plot(forecast_candyts)
# blue dots are the forecasted values

# Validations
 hist(Model$residuals)
qqnorm(Model$residuals)
# straight line, normally distributed

 Box.test(Model$residuals)
# good fit -> p(0.05) < (0.9), null accepted
# h0: autocorrelations is 0/ normally distributed/ good fit
# ha: otherwise