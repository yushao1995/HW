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
library(devtools)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(tseries)
library(urca)


# Creation of Time Series Data Object #
PM_2_5_Raleigh2$Date=as.Date(PM_2_5_Raleigh2$Date,format='%m/%d/%Y')
ts <- seq.POSIXt(as.POSIXct("2014-01-01",'%m/%d/%y'), as.POSIXct("2018-12-31",'%m/%d/%y'), by="day")
df <- data.frame(Date=ts)
df$Date=as.Date(df$Date,format='%m/%d/%Y')
pm <- full_join(df,PM_2_5_Raleigh2)

hw=pm %>% group_by(month=floor_date(Date, "month")) %>%
  summarize(amount=mean(Daily.Mean.PM2.5.Concentration, na.rm=TRUE))


# ts: Basic ts object #
ts <- ts(hw$amount, start = 2014, frequency =12)


# training: training set #
# test: validation set #
training=subset(ts,end=length(ts)-6)
test=subset(ts,start=length(ts)-5)

# Use STL decomposition to find patterns #
decomp_stl <- stl(training, s.window = 7)
plot(decomp_stl)
# Find both season and trend component 



# Tell whether has seasonal random walk and needs seasonal difference #
nsdiffs(training)
nsdiffs(training,test="ch")
# NO

# Fit Dummy #
month <- seasonaldummy(training)
model  <- tslm(training ~ month)
tsdisplay(residuals(model))
ts_r=residuals(model)

# Testing stationary #
adf.test(ts_r, alternative = "stationary", k=0)
adf.test(ts_r, alternative = "stationary", k=1)
adf.test(ts_r, alternative = "stationary", k=2)

# Fit Trend #
model  <- tslm(training ~ trend+month, lambda=1)
tsdisplay(residuals(model))
ts_r=residuals(model)
forcast_ts <- forecast(model,data.frame(month=I(seasonaldummy(training,6))))$mean

# Auto select #
auto.arima(ts_r)

# Results #
mod1<-sarima(ts_r, 1,0,0,1,0,0,12)

# Forcast #
mod1 <- Arima(ts_r, order=c(1,0,0), seasonal=c(1,0,0), method="ML")
forcast_r=forecast(mod1, h = 6)$mean
forcast=forcast_ts+forcast_r

# visualization
plot(test)
plot(forcast)
df=data.frame(Actual=as.matrix(test), date=as.yearmon(time(test)))
df$Predicted=as.matrix(forcast)



ggplot()+
  geom_line(data=df,aes(y=Actual,x= date,color="Actual"),size=1 )+
  geom_line(data=df,aes(y=Predicted,x= date,color="Predicted"),size=1)+ 
  scale_color_manual(values = c("Predicted" = "goldenrod1", "Actual" = "darkcyan"))+
  scale_x_yearmon()+
  labs(title =  'Prediction versus Actual in Validation Set',y = "Monthly Average PM2.5",x = "Year")+
  theme_minimal(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust= 0.5),legend.title=element_blank(),legend.position="bottom")
