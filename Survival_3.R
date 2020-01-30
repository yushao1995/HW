library(survival)
library(MASS)
library(survminer)
library(visreg)
library(ggplot2)
library(dplyr)
library(tidyr)
library(doMC)      
registerDoMC()  


hurricane <- read_sas("Desktop/Homework1_SA/hurricane.sas7bdat", NULL) %>%
  mutate(subject=1:n())

hurricane <- hurricane %>%
  mutate(subject=1:n())



for(i in 1:11){
  print(i)
  cola <- paste('sum', i, sep= '')
  hurricane[[cola]] <- 0
}


for(i in 12:48){
  print(i)
  cola <- paste('sum', i, sep= '')
  l=i-3
  u=i+8
  hurricane[[cola]] <- rowSums(hurricane[,l:u],na.rm = T)
  hurricane[[cola]][hurricane[[cola]]!=12]=0
  hurricane[[cola]][hurricane[[cola]]==12]=1
}


drops <- c()
for(i in 1:48){
  print(i)
  cola <- paste('h', i, sep= '')
  drops <- c(drops, cola)}


hurricane=hurricane[ , !(names(hurricane) %in% drops)]
hurricane=hurricane %>% mutate(motors = (reason == 2))
for(h in 1:47){
  for(i in 1:48){
    if (h<i)
      cola <- paste('sum', i, sep= '')
    hurricane[hurricane$hour == h,][[cola]]=NA
  }
}


motors <- hurricane %>%
  gather(hour_, h, sum1:sum48) %>%
  filter(!is.na(h)) %>% 
  group_by(subject) %>%
  mutate(
    start  = row_number()-1,
    stop   = row_number()) %>%
  arrange(subject, stop)


motors <- motors %>%
  group_by(subject) %>%
  mutate(h_1 = lag(h, default=0)) %>%
  ungroup()


motors <- motors %>%
  group_by(subject) %>%
  mutate(diff = h-h_1) %>%
  ungroup()


motors[motors$start == 0,][['diff']]=1

motors=motors[motors$diff != 0,]


motors <- motors %>%
  group_by(subject) %>%
  mutate(lead_stop = lead(start, default=NA)) %>%
  ungroup()

motors <- motors %>%
  group_by(subject) %>%
  mutate(stop = lead_stop) %>%
  ungroup()

motors$stop[is.na(motors$stop)]<- motors$hour[is.na(motors$stop)]

motors[(motors$reason==2)&(motors$hour!=motors$stop),][['motors']]=FALSE


motors.ph <- coxph(Surv(start, stop, motors) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo+h, data = motors)
summary(motors.ph)



# Proportional Hazard Test - Schoenfeld Residuals #
ph.zph <- cox.zph(motors.ph, transform = "km")
ph.zph

ph.zph <- cox.zph(motors.ph, transform = "identity")
ph.zph

ph.zph <- cox.zph(motors.ph, transform = "log")
ph.zph


ph.tdc <- coxph(Surv(start, stop, motors) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo+h, data = motors)
  
  
  coxph(Surv(week, arrest == 1) ~ fin + race + wexp + mar + paro + age + tt(age), data = recid,
                      tt = function(x, time, ...){x*log(time)})
summary(recid.ph.tdc)

concordance(motors.ph)



