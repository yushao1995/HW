# Needed Libraries for Analysis #
#install.packages("survival")
#install.packages("survminer")

library(survival)
library(survminer)
library(dplyr)
library(tidyverse)

surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0)

#Survival probability across time for all pumps together
km <- survfit(surv ~ 1, data = hurricane)
summary(km)
ggsurvplot(km, data = hurricane, conf.int = TRUE, palette = "purple",
           xlab = "Week", ylab = "Survival Probability", legend = "none",
           break.y.by = 0.1)


#Survival probability across time for pumps broken down by failure type
survdiff(surv ~ reason, rho = 0, data = hurricane)

recid_strat <- survfit(surv ~ reason, data = hurricane)
summary(recid_strat)
ggsurvplot(recid_strat, data = hurricane, palette = c("purple", "black","red","blue","yellow"),
           xlab = "Week", ylab = "Survival Probability", break.y.by = 0.1,
           legend.title = "work experience", legend.labs = c("0", "1","2","3","4"))


#Conditional failure probabilities across time for all pumps together
km <- survfit(Surv(time = hurricane$hour, event = (hurricane$survive == 0)) ~ 1, data = hurricane)
km$hp <- km$n.event/km$n.risk
print(km$hp)

haz <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, hp = km$hp), by = "time", all = TRUE)
haz[is.na(haz) == TRUE] <- 0
print(haz)

plot(y = haz$hp, x = haz$time, main = "Hazard Probability Function", xlab = "Tenure", ylab = "Hazard Probability",type = 'l')

ggsurvplot(km, data = hurricane, fun = "cumhaz", conf.int = TRUE, palette = "purple",
           xlab = "Week", ylab = "Cumulative Hazard", legend = "none")

#Conditional failure probabilities across time for pumps broken down by failure type
#Subset and Output for hp
hurricane_0 = hurricane %>% 
  filter(reason==0)

km <- survfit(Surv(time = hurricane_0$hour, event = (hurricane_0$survive == 0)) ~ 1, data = hurricane_0)
km$hp <- km$n.event/km$n.risk
haz_0 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, hp = km$hp), by = "time", all = TRUE)
haz_0[is.na(haz_0) == TRUE] <- 0
colnames(haz_0)[colnames(haz_0)=="hp"] <- "hp_0"

hurricane_1 = hurricane %>% 
  filter(reason==1)

km <- survfit(Surv(time = hurricane_1$hour, event = (hurricane_1$survive == 0)) ~ 1, data = hurricane_1)
km$hp <- km$n.event/km$n.risk
haz_1 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, hp = km$hp), by = "time", all = TRUE)
haz_1[is.na(haz_1) == TRUE] <- 0
colnames(haz_1)[colnames(haz_1)=="hp"] <- "hp_1"

hurricane_2 = hurricane %>% 
  filter(reason==2)

km <- survfit(Surv(time = hurricane_2$hour, event = (hurricane_2$survive == 0)) ~ 1, data = hurricane_2)
km$hp <- km$n.event/km$n.risk
haz_2 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, hp = km$hp), by = "time", all = TRUE)
haz_2[is.na(haz_2) == TRUE] <- 0
colnames(haz_2)[colnames(haz_2)=="hp"] <- "hp_2"

hurricane_3 = hurricane %>% 
  filter(reason==3)

km <- survfit(Surv(time = hurricane_3$hour, event = (hurricane_3$survive == 0)) ~ 1, data = hurricane_3)
km$hp <- km$n.event/km$n.risk
haz_3 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, hp = km$hp), by = "time", all = TRUE)
haz_3[is.na(haz_3) == TRUE] <- 0
colnames(haz_3)[colnames(haz_3)=="hp"] <- "hp_3"

hurricane_4 = hurricane %>% 
  filter(reason==4)

km <- survfit(Surv(time = hurricane_4$hour, event = (hurricane_4$survive == 0)) ~ 1, data = hurricane_4)
km$hp <- km$n.event/km$n.risk
haz_4 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, hp = km$hp), by = "time", all = TRUE)
haz_4[is.na(haz_4) == TRUE] <- 0
colnames(haz_4)[colnames(haz_4)=="hp"] <- "hp_4"

haz=merge(x = haz, y = haz_0, by = "time", all.x = TRUE)
haz=merge(x = haz, y = haz_1, by = "time", all.x = TRUE)
haz=merge(x = haz, y = haz_2, by = "time", all.x = TRUE)
haz=merge(x = haz, y = haz_3, by = "time", all.x = TRUE)
haz=merge(x = haz, y = haz_4, by = "time", all.x = TRUE)

write.csv(simple_haz, file = "/Users/shao/Desktop/Homework1_SA/hp.csv",row.names=FALSE, na="")



#Subset and Output for sur

km <- survfit(Surv(time = hurricane$hour, event = (hurricane$survive == 0)) ~ 1, data = hurricane)
sur <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, sur = km$surv), by = "time", all = TRUE)
sur=sur %>% fill(sur)
sur[is.na(sur) == TRUE] <- 1

km <- survfit(Surv(time = hurricane_0$hour, event = (hurricane_0$survive == 0)) ~ 1, data = hurricane_0)
sur_0 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, sur_0 = km$surv), by = "time", all = TRUE)
sur_0=sur_0 %>% fill(sur_0)
sur_0[is.na(sur_0) == TRUE] <- 1


km <- survfit(Surv(time = hurricane_1$hour, event = (hurricane_1$survive == 0)) ~ 1, data = hurricane_1)
sur_1 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, sur_1 = km$surv), by = "time", all = TRUE)
sur_1=sur_1 %>% fill(sur_1)
sur_1[is.na(sur_1) == TRUE] <- 1

km <- survfit(Surv(time = hurricane_2$hour, event = (hurricane_2$survive == 0)) ~ 1, data = hurricane_2)
sur_2 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, sur_2 = km$surv), by = "time", all = TRUE)
sur_2=sur_2 %>% fill(sur_2)
sur_2[is.na(sur_2) == TRUE] <- 1

km <- survfit(Surv(time = hurricane_3$hour, event = (hurricane_3$survive == 0)) ~ 1, data = hurricane_3)
sur_3 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, sur_3 = km$surv), by = "time", all = TRUE)
sur_3=sur_3 %>% fill(sur_3)
sur_3[is.na(sur_3) == TRUE] <- 1

km <- survfit(Surv(time = hurricane_4$hour, event = (hurricane_4$survive == 0)) ~ 1, data = hurricane_4)
sur_4 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = km$time, sur_4 = km$surv), by = "time", all = TRUE)
sur_4=sur_4 %>% fill(sur_4)
sur_4[is.na(sur_4) == TRUE] <- 1


sur=merge(x = sur, y = sur_0, by = "time", all.x = TRUE)
sur=merge(x = sur, y = sur_1, by = "time", all.x = TRUE)
sur=merge(x = sur, y = sur_2, by = "time", all.x = TRUE)
sur=merge(x = sur, y = sur_3, by = "time", all.x = TRUE)
sur=merge(x = sur, y = sur_4, by = "time", all.x = TRUE)


write.csv(sur, file = "/Users/shao/Desktop/Homework1_SA/sur.csv",row.names=FALSE, na="")



#Grouping and Testing
hurricane$reason_new=recode(hurricane$reason, `1` = 1L,`2` = 2L, `3` = 2L, `4` = 0L,`0` = 0L)
hurricane_new = hurricane %>% 
  filter(reason_new>0)

surv <- Surv(time = hurricane_new$hour, event = hurricane_new$survive == 0)
strat <- survfit(surv ~ reason_new, data = hurricane_new)
pairwise_survdiff(surv ~ reason_new, rho = 0, data = hurricane_new)
survdiff(surv ~ reason_new, rho = 0, data = hurricane_new)


#Subset and Output for grouping
hurricane_new_1 = hurricane_new %>% 
  filter(reason_new==1)
hurricane_new_2 = hurricane_new %>% 
  filter(reason_new==2)





simple_km <- survfit(Surv(time = hurricane_new_1$hour, event = (hurricane_new_1$survive == 0)) ~ 1, data = hurricane_new_1)
sur_1 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = simple_km$time, sur_1 = simple_km$surv), by = "time", all = TRUE)
sur_1=sur_1 %>% fill(sur_1)
sur_1[is.na(sur_1) == TRUE] <- 1

simple_km <- survfit(Surv(time = hurricane_new_2$hour, event = (hurricane_new_2$survive == 0)) ~ 1, data = hurricane_new_2)
sur_2 <- merge(data.frame(time = seq(1,48,1)), data.frame(time = simple_km$time, sur_2 = simple_km$surv), by = "time", all = TRUE)
sur_2=sur_2 %>% fill(sur_2)
sur_2[is.na(sur_2) == TRUE] <- 1


sur_1=merge(x = sur_1, y = sur_2, by = "time", all.x = TRUE)
write.csv(sur_1, file = "/Users/shao/Desktop/Homework1_SA/sur_new.csv",row.names=FALSE, na="")