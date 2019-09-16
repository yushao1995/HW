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
library(lubridate)
library(tidyverse)
library(tsibble)
library(lubridate)

# Creation of Time Series Data Object #
PM_2_5_Raleigh2$Date=as.Date(PM_2_5_Raleigh2$Date,format='%m/%d/%Y')
ts <- seq.POSIXt(as.POSIXct("2014-01-01",'%m/%d/%y'), as.POSIXct("2018-12-31",'%m/%d/%y'), by="day")
df <- data.frame(Date=ts)
df$Date=as.Date(df$Date,format='%m/%d/%Y')
pm <- full_join(df,PM_2_5_Raleigh2)

hw=pm %>% group_by(month=floor_date(Date, "month")) %>%
  summarize(amount=mean(Daily.Mean.PM2.5.Concentration, na.rm=TRUE))

ts <- ts(hw$amount, start = 2014, frequency =12)




# Time Series Decomposition

#stl
decomp_stl <- stl(ts, s.window = 7)


# Ploting
#get each element
seasonal   <- decomp_stl$time.series[,1]
trend	   <- decomp_stl$time.series[,2]
remainder  <- decomp_stl$time.series[,3]

trend=subset(trend,end=length(trend)-6)
remainder=subset(remainder,end=length(remainder)-6)

plot(training,
     main="Particulate Matter with Trend",
     ylab="Particulate Matter")
lines(trend,col="blue")
legend(x=2017.3,y=13,c("Actual","Trend"),cex=0.7,col=c("black","blue"),pch=c(1,1))


plot(test, ylab="Particulate Matter",main="Particulate Matter with Predicted Values")
#second use line to add another line on plot
lines(predict,col="darkturquoise")
legend(x=2018.52,y=12.2,c("Actual","Predicted"),cex=0.7,col=c("black","darkturquoise"),pch=c(1,1))





# Modeling
training=subset(ts,end=length(ts)-6)
test=subset(ts,start=length(ts)-5)


SES <- ses(training, initial = "optimal", h = 6)
test.results=forecast(SES,h=6)
error=test-test.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(test))
print(MAPE)

predict=SES[["mean"]]

plot(test, ylab="Particulate Matter",main="Particulate Matter with Predicted Values")
#second use line to add another line on plot
lines(predict,col="darkturquoise")
legend(x=2018.52,y=12.2,c("Actual","Predicted"),cex=0.7,col=c("black","darkturquoise"),pch=c(1,1))



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



