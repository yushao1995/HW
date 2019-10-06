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
library(astsa)

# Creation of Time Series Data Object #
PM_2_5_Raleigh2$Date=as.Date(PM_2_5_Raleigh2$Date,format='%m/%d/%Y')
ts <- seq.POSIXt(as.POSIXct("2014-01-01",'%m/%d/%y'), as.POSIXct("2018-12-31",'%m/%d/%y'), by="day")
df <- data.frame(Date=ts)
df$Date=as.Date(df$Date,format='%m/%d/%Y')
pm <- full_join(df,PM_2_5_Raleigh2)

hw=pm %>% group_by(month=floor_date(Date, "month")) %>%
  summarize(amount=mean(Daily.Mean.PM2.5.Concentration, na.rm=TRUE))

ts <- ts(hw$amount, start = 2014, frequency =12)


decomp_stl <- stl(ts, s.window = 7)
plot(decomp_stl)


# Tell whether has seasonal random walk and needs seasonal difference
nsdiffs(ts)
nsdiffs(ts,test="ch")

# Fit Dummy
Q = factor(cycle(ts))
fit=lm(ts~Q)
ts_s=predict(fit, type = "response")
ts_s <- ts(ts_s, start = 2014, frequency =12)

decomp_stl <- stl(ts_a, s.window = 7)
plot(decomp_stl)

trend	   <- decomp_stl$time.series[,2]
trend
ts_a=ts-ts_a-trend

decomp_stl <- stl(ts_a, s.window = 7)
plot(decomp_stl)



#Trend edit version
ts_1=subset(ts,end=length(ts)-17)
ts_2=subset(ts,start=length(ts)-16)

fit=lm(ts_1~time(ts_1))
ts_1=predict(fit, type = "response")
fit=lm(ts_2~time(ts_2))
ts_2=predict(fit, type = "response")
ts_t=c(ts_1, ts_2)
ts_t <- ts(ts_t, start = 2014, frequency =12)

ts_a=ts-ts_t-ts_s

decomp_stl <- stl(ts_a, s.window = 7)
plot(decomp_stl)


#auto selection
auto.arima(ts_a)
auto.arima(ts)

#sarima function
mod1<-sarima(ts_a, 1,0,0,0,0,1,12)
mod1<-sarima(ts, 1,1,1,1,0,0,12)

#double check with the code provided by professor
modle <- Arima(ts_a, order=c(1,0,0), seasonal=c(0,0,1),method="ML",)
summary(modle)

Acf(ts_a, main = "")$acf
Pacf(ts_a, main = "")$acf
Acf(modle$residuals, main = "")$acf
Pacf(modle$residuals, main = "")$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(modle$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")



#
modle <- Arima(ts, order=c(1,1,1), seasonal=c(1,0,0),method="ML",)
summary(modle)

Acf(ts_a, main = "")$acf
Pacf(ts_a, main = "")$acf
Acf(modle$residuals, main = "")$acf
Pacf(modle$residuals, main = "")$acf

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(modle$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")




