library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(tidyr)
library(dplyr)
library(imputeTS)
library(tidyverse)
library(tsibble)
library(lubridate)

PM_2_5_Raleigh2$Date=as.Date(PM_2_5_Raleigh2$Date,format='%m/%d/%Y')
ts <- seq.POSIXt(as.POSIXct("2014-01-01",'%m/%d/%y'), as.POSIXct("2018-12-31",'%m/%d/%y'), by="day")
df <- data.frame(Date=ts)
df$Date=as.Date(df$Date,format='%m/%d/%Y')
pm <- full_join(df,PM_2_5_Raleigh2)

hw=pm %>% group_by(month=floor_date(Date, "month")) %>%
  summarize(amount=mean(Daily.Mean.PM2.5.Concentration, na.rm=TRUE))


# Creation of Time Series Data Object #
ts <- ts(hw$amount, start = 2014, frequency =12)

# Time Series Decomposition ...STL#
decomp_stl <- stl(ts, s.window = 7)
plot(decomp_stl)

# Building a Single Exponential Smoothing Model - Steel Data #
SES.Steel <- ses(ts, initial = "optimal", h = 24)
summary(SES.Steel)

plot(SES.Steel, main = "US Steel Shipments with Simple ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
abline(v = 1992, col = "red", lty = "dashed")
round(accuracy(SES.Steel),2)

autoplot(SES.Steel)+
  autolayer(fitted(SES.Steel),series="Fitted")+ylab("US Steel Shipments with Simple ESM Forecast")

# Building a Linear Exponential Smoothing Model - Steel Data #
LES.Steel <- holt(ts, initial = "optimal", h = 24)
summary(LES.Steel)

plot(LES.Steel, main = "US Steel Shipments with Linear ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
abline(v = 1992, col = "red", lty = "dashed")

autoplot(LES.Steel)+
  autolayer(fitted(LES.Steel),series="Fitted")+ylab("US Steel Shipments with Holt ESM Forecast")

LDES.Steel <- holt(ts, initial = "optimal", h = 24, damped = TRUE)
summary(LDES.Steel)

plot(LDES.Steel, main = "US Steel Shipments with Linear Damped ESM Forecast", xlab = "Date", ylab = "Shipments (Thousands of Net Tons)")
abline(v = 1992, col = "red", lty = "dashed")

autoplot(LDES.Steel)+
  autolayer(fitted(LDES.Steel),series="Fitted")+ylab("US Steel Shipments")


# Building a Linear Exponential Smoothing Model - US Airlines Data #
LES.USAir <- holt(ts, initial = "optimal", h = 24)
summary(LES.USAir)

plot(LES.USAir, main = "US Airline Passengers with Linear ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")

LDES.USAir <- holt(ts, initial = "optimal", h = 24, damped = TRUE)
summary(LDES.USAir)

plot(LDES.USAir, main = "US Airline Passengers with Linear Damped ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")

# Building a Holt-Winters ESM - US Airlines Data #
HWES.USAir <- hw(ts, seasonal = "additive")
summary(HWES.USAir)

plot(HWES.USAir, main = "US Airline Passengers with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")


autoplot(HWES.USAir)+
  autolayer(fitted(HWES.USAir),series="Fitted")+ylab("Airlines Passengers")


HWES.USAir <- hw(ts, seasonal = "multiplicative")
summary(HWES.USAir)

plot(HWES.USAir, main = "US Airline Passengers with Holt-Winters ESM Forecast", xlab = "Date", ylab = "Passengers (Thousands)")
abline(v = 2008.25, col = "red", lty = "dashed")


autoplot(HWES.USAir)+
  autolayer(fitted(HWES.USAir),series="Fitted")+ylab("Airlines Passengers")


#####Using a holdout data set
training=subset(ts,end=length(ts)-6)
test=subset(ts,start=length(ts)-5)
HWES.USAir.train <- hw(training, seasonal = "multiplicative",initial='optimal')
test.results=forecast(HWES.USAir.train,h=12)

error=test-test.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(test))
