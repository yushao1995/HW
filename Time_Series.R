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


training=subset(ts,end=length(ts)-6)
test=subset(ts,start=length(ts)-5)


SES <- ses(training, initial = "optimal", h = 6)
test.results=forecast(SES,h=6)
error=test-test.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(test))
print(MAPE)

# Building a Linear Exponential Smoothing Model
LES <- holt(training, initial = "optimal", h = 6)
test.results=forecast(LES,h=6)
error=test-test.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(test))
print(MAPE)


LDES <- holt(training, initial = "optimal", h = 6, damped = TRUE)
test.results=forecast(LDES,h=6)
error=test-test.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(test))
print(MAPE)


# Building a Holt-Winters ESM
HWES <- hw(training, seasonal = "additive", h=6)
test.results=forecast(HWES,h=6)
error=test-test.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(test))
print(MAPE)



