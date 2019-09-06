library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)

sum(is.na(PM_2_5_Raleigh2$Daily.Mean.PM2.5.Concentration))
summary(PM_2_5_Raleigh2)

pm=zoo(PM_2_5_Raleigh2$Daily.Mean.PM2.5.Concentration, seq(from = as.Date("2014-01-01"), to = as.Date("2018-12-31"), by = 1))
plot(pm)

pm_month <- aggregate(pm, as.yearmon, mean)
plot(pm_month)


ts_beer = ts(pm_month, frequency = 12)
stl_beer = stl(ts_beer, "periodic")
seasonal_stl_beer   <- stl_beer$time.series[,1]
trend_stl_beer     <- stl_beer$time.series[,2]
random_stl_beer  <- stl_beer$time.series[,3]

plot(ts_beer)
plot(as.ts(seasonal_stl_beer))
plot(trend_stl_beer)
plot(random_stl_beer)
plot(stl_beer)