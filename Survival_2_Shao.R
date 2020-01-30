#Library
library(survival)
library(survminer)
library(flexsurv)
library(dplyr)
library(tidyverse)
library(pec)

# Create Dummy                                      
hurricane=hurricane %>% mutate(flood = (reason == 1))                        

# Plot
# Accelerated Failure Time Model 
aft.w <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "weibull")
aft.e <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "exp")
aft.g <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "gamma")
aft.ll <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "llogis")
aft.ln <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "lnorm")


plot(aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Weibull Distribution")

plot(aft.e, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Exponential Distribution")

plot(aft.g, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Gamma Distribution")

plot(aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Log-Logistic Distribution")

plot(aft.ln, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Log-Normal Distribution")

# Converge Problem
aft.f <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "genf")
plot(aft.f, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Generalized F")


# Goodness-of-Fit Tests #
# The flexsurvreg() function has more distributions available than in SAS so we can perform more comparisons here. #
like.e <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "exp")$loglik
like.w <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "weibull")$loglik
like.ln <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "lnorm")$loglik
like.g <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "gamma")$loglik
like.ll <- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "llogis")$loglik

pval.e.g <- 1 - pchisq((-2*(like.e-like.g)), 2)
pval.w.g <- 1 - pchisq((-2*(like.w-like.g)), 1)
pval.ln.g <- 1 - pchisq((-2*(like.ln-like.g)), 1)

# Converge Problem
like.f<- flexsurvreg(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "genf")$loglik
pval.g.f <- 1 - pchisq((-2*(like.g-like.f)), 1)
pval.ll.f <- 1 - pchisq((-2*(like.ll-like.f)), 1)


# Test
Tests <- c('Exp vs. Gam', 'Wei vs. Gam', 'LogN vs. Gam')
P_values <- c(pval.e.g, pval.w.g, pval.ln.g)
cbind(Tests, P_values)


# Model selection
# Reference https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5233524/
model=psm(Surv(hour, flood == 1) ~ age+backup+bridgecrane+gear+trashrack+slope+elevation+servo, data = hurricane, dist = "weibull")
anova(model)
plot(anova(model),margin=c("chisq","d.f.","P"))
fastbw(model,rule="p",sls=.03)
fastbw(model,rule="p",sls=.05)

aft.w <- survreg(Surv(hour, flood == 1) ~ slope+servo, data = hurricane, dist = 'weibull')
(exp(coef(aft.w))-1)*100


# Upgrade
survprob.actual <- 1 - psurvreg(hurricane$hour,
                                mean = predict(aft.w, type = "lp"),
                                scale = aft.w$scale,
                                distribution = aft.w$dist)

new_time <-  qsurvreg(1 - survprob.actual,
                      mean = predict(aft.w, type = "lp") + coef(aft.w)['servo'],
                      scale = aft.w$scale,
                      distribution = aft.w$dist)

hurricane$new_time <- new_time
hurricane$diff <- hurricane$new_time - hurricane$hour

head(data.frame(hurricane$hour, hurricane$new_time, hurricane$diff), n = 10)

subset=hurricane %>% filter(reason==1) %>% filter(hour<48) %>% arrange(desc(diff))

subset=hurricane %>% filter(reason==1) %>% select(hour,new_time,diff) %>% arrange(desc(diff))


