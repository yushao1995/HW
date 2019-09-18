# Needed Libraries for Analysis #

library(MASS)
library(car)
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)
library(haven)
# Load Needed Data Sets #
# Replace the ... below with the file location of the data sets #
insurance_t_bin <- read_sas("~/Desktop/HW/Homework2_LR/insurance_t_bin.sas7bdat")

# Data Processing
# Missing as catagory
insurance_t_bin[is.na(insurance_t_bin)] <- "Missing"
# Quasi Seperation Recode
insurance_t_bin$CASHBK[insurance_t_bin$CASHBK==2]=1
table(insurance_t_bin[["CASHBK"]],insurance_t_bin[["INS"]])

insurance_t_bin$MMCRED[insurance_t_bin$MMCRED==5]=3
table(insurance_t_bin[["MMCRED"]],insurance_t_bin[["INS"]])


# Generalized R-squared #
# Model from Phase 2
logit.model=glm(INS ~ DDA:IRA + DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

summary(logit.model)

AIC(logit.model)
BIC(logit.model)
PseudoR2(logit.model, which = "Nagelkerke")

# Brier Score #
BrierScore(logit.model)

# Discrimination Slope #
insurance_t_bin$p_hat <- predict(logit.model, type = "response")

p1 <- insurance_t_bin$p_hat[insurance_t_bin$INS == 1]
p0 <- insurance_t_bin$p_hat[insurance_t_bin$INS == 0]
coef_discrim <- mean(p1) - mean(p0)

ggplot(insurance_t_bin, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability",
       fill = "Outcome",
       title = paste("Coefficient of Discrimination = ",
                     round(coef_discrim, 3), sep = ""))

# Rank-order Statistics #
Concordance(insurance_t_bin$INS, insurance_t_bin$p_hat)

somersD(insurance_t_bin$INS, insurance_t_bin$p_hat)

# Classification Table & Youden Index #
confusionMatrix(insurance_t_bin$INS, insurance_t_bin$p_hat, threshold = 0.5)

sens <- NULL
spec <- NULL
youden <- NULL
cutoff <- NULL

for(i in 1:49){
     cutoff = c(cutoff, i/50)
     sens <- c(sens, sensitivity(insurance_t_bin$INS, insurance_t_bin$p_hat, threshold = i/50))
     spec <- c(spec, specificity(insurance_t_bin$INS, insurance_t_bin$p_hat, threshold = i/50))
     youden <- c(youden, youdensIndex(insurance_t_bin$INS, insurance_t_bin$p_hat, threshold = i/50))
   }

ctable <- data.frame(cutoff, sens, spec, youden)

ctable
write.csv(ctable,'/Users/shao/Desktop/HW/LR_3/ctable1.csv')


# ROC Curve - InformationValue Package #
plotROC(insurance_t_bin$INS, insurance_t_bin$p_hat)
AUROC(insurance_t_bin$INS, insurance_t_bin$p_hat)

# ROC Curve - ROCR Package #
pred <- prediction(fitted(logit.model), factor(insurance_t_bin$INS))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)

performance(pred, measure = "auc")@y.values

# Precision, Recall, F1 #
prec <- NULL
reca <- NULL
f1 <- NULL
cutoff <- NULL

for(i in 1:49){
  cutoff = c(cutoff, i/50)
  reca <- c(reca, sensitivity(insurance_t_bin$INS, insurance_t_bin$p_hat, threshold = i/50))
  prec <- c(prec, precision(insurance_t_bin$INS, insurance_t_bin$p_hat, threshold = i/50))
  f1 <- c(f1, 2*((prec[i]*reca[i])/(prec[i]+reca[i])))
}

ctable <- data.frame(cutoff, reca, prec, f1)
write.csv(ctable,'/Users/shao/Desktop/HW/LR_3/ctable2.csv')
ctable

# Lift Chart #
perf <- performance(pred, measure = "lift", x.measure = "rpp")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)),
     main = "Lift Chart for Training Data")
abline(h = 1, lty = 3)

# K-S Statistics #
ks_plot(insurance_t_bin$INS, insurance_t_bin$p_hat)
ks_stat(insurance_t_bin$INS, insurance_t_bin$p_hat)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
KS <- max(perf@y.values[[1]] - perf@x.values[[1]])
cutoffAtKS <- unlist(perf@alpha.values)[which.max(perf@y.values[[1]] - perf@x.values[[1]])]
print(c(KS, cutoffAtKS))

plot(x = unlist(perf@alpha.values), y = (1-unlist(perf@y.values)),
     type = "l", main = "K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf@alpha.values), y = (1-unlist(perf@x.values)), col = "blue")
