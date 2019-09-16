library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(imputeTS)
library(lubridate)
library(tidyverse)
library(tseries)


PM_2_5_Raleigh2$Date=as.Date(PM_2_5_Raleigh2$Date,format='%m/%d/%Y')
ts <- seq.POSIXt(as.POSIXct("2014-01-01",'%m/%d/%y'), as.POSIXct("2018-12-31",'%m/%d/%y'), by="day")
df <- data.frame(Date=ts)
df$Date=as.Date(df$Date,format='%m/%d/%Y')
pm <- full_join(df,PM_2_5_Raleigh2)

hw=pm %>% group_by(month=floor_date(Date, "month")) %>%
  summarize(amount=mean(Daily.Mean.PM2.5.Concentration, na.rm=TRUE))

# Creation of Time Series Data Object #
ts <- ts(hw$amount, start = 2014, frequency =12)
ts=subset(ts,end=length(ts)-6)
#Check the stationarity of the average monthly PM2.5 levels including trend and/or
#random walks; the analysts recommend using the Augmented Dickey-Fuller tests up 
#to lag 2 tests for the results, however, you are welcome to suggest other techniques
#as long as the reasons are clearly stated and supported.


# Augmented Dickey-Fuller Testing #
ADF.Pvalues <- rep(NA, 3)
for(i in 0:2){
  ADF.Pvalues[i+1] <- adf.test(ts, alternative = "stationary", k = i)$p.value
}

#What strategies (if any) should the client take to make the data stationary 
#(the EPA would like to consider only non-seasonal options at this time)?


#Does the stationary time series exhibit white noise? Provide evidence on whether 
#it does or does not have white noise and the implications for future ARIMA modeling.

# Using Original Data
White.LB <- rep(NA, 10) 
for(i in 1:10){
  White.LB[i] <- Box.test(ts, lag=i, type="Lj", fitdf=2)$p.value
}
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags")
abline(h=0.01, lty="dashed", col="black") 
abline(h=0.05, lty="dashed", col="black")


# Using Residual
SES <- ses(ts, initial = "optimal", h = 6)
test.results=forecast(SES,h=6)
residuals=test.results$residuals

White.LB <- rep(NA, 10) 
for(i in 1:10){
  White.LB[i] <- Box.test(residuals, lag=i, type="Lj", fitdf=2)$p.value
}
barplot(White.LB, main="Ljung-Box Test P-values", ylab="Probabilities", xlab="Lags")
abline(h=0.01, lty="dashed", col="black") 
abline(h=0.05, lty="dashed", col="black")





