# Needed Libraries for Analysis #
library(MASS)
library(car)
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)
library(haven)
library(dplyr)

# Load Needed Data Sets #
# Replace the ... below with the file location of the data sets #
insurance_t_bin <- read_sas("~/Desktop/HW/Homework2_LR/insurance_t_bin.sas7bdat")
insurance_v_bin <- read_sas("~/Desktop/HW/Homework2_LR/insurance_v_bin.sas7bdat")

# Data Processing
# Missing as catagory
insurance_t_bin[is.na(insurance_t_bin)] <- "Missing"
# Quasi Seperation Recode
insurance_t_bin$CASHBK[insurance_t_bin$CASHBK==2]=1
table(insurance_t_bin[["CASHBK"]],insurance_t_bin[["INS"]])

insurance_t_bin$MMCRED[insurance_t_bin$MMCRED==5]=2
insurance_t_bin$MMCRED[insurance_t_bin$MMCRED==3]=2
table(insurance_t_bin[["MMCRED"]],insurance_t_bin[["INS"]])

# Data Processing for validation dataset
# Missing as catagory
insurance_v_bin[is.na(insurance_v_bin)] <- "Missing"
# Quasi Seperation Recode
insurance_v_bin$CASHBK[insurance_v_bin$CASHBK==2]=1
table(insurance_v_bin[["CASHBK"]],insurance_v_bin[["INS"]])

## We recode one more group
insurance_v_bin$MMCRED[insurance_v_bin$MMCRED==5]=2
insurance_v_bin$MMCRED[insurance_v_bin$MMCRED==3]=2
table(insurance_v_bin[["MMCRED"]],insurance_v_bin[["INS"]])



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
  scale_fill_manual(values=c("goldenrod1","darkcyan")) +
  geom_histogram(alpha = 0.7)+
  labs(x = "Predicted Probability",
       fill = "Outcome",
       title = paste("Coefficient of Discrimination = ",
                     round(coef_discrim, 3), sep = ""))+
  theme_classic()+
  theme(plot.title=element_text(hjust=0.5))

# Rank-order Statistics #
Concordance(insurance_t_bin$INS, insurance_t_bin$p_hat)

somersD(insurance_t_bin$INS, insurance_t_bin$p_hat)


# ROC Curve - InformationValue Package #
plotROC(insurance_t_bin$INS, insurance_t_bin$p_hat)
AUROC(insurance_t_bin$INS, insurance_t_bin$p_hat)

# ROC Curve - ROCR Package #
pred <- prediction(fitted(logit.model), factor(insurance_t_bin$INS))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")


plot(perf, lwd = 3,  col = "darkcyan")
text(0.5, 0.5,"AUROC: 0.798")



performance(pred, measure = "auc")@y.values


# K-S Statistics #
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
KS <- max(perf@y.values[[1]] - perf@x.values[[1]])
cutoffAtKS <- unlist(perf@alpha.values)[which.max(perf@y.values[[1]] - perf@x.values[[1]])]
print(c(KS, cutoffAtKS))

plot(x = unlist(perf@alpha.values), y = (1-unlist(perf@y.values)),
     type = "l", main = "K-S Plot (EDF)",
     xlab = 'Estimated Probability',
     ylab = "Proportion",
     col = "goldenrod1",
     lwd=3)
lines(x = unlist(perf@alpha.values), y = (1-unlist(perf@x.values)), col = "darkcyan",lwd=3)
legend(0.75, 0.3, legend=c("0", "1"),col=c("goldenrod1","darkcyan"), lty=1,cex=1,lwd=3,title="Ourcome",text.font=4,box.lty=0)


#Find the cutoff by youden table
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
#0.3 as cutoff



########### 
## Fit Model on validation dataset
insurance_v_bin$p_hat <- predict(logit.model, insurance_v_bin)

p1 <- insurance_v_bin$p_hat[insurance_v_bin$INS == 1]
p0 <- insurance_v_bin$p_hat[insurance_v_bin$INS == 0]
coef_discrim <- mean(p1) - mean(p0)

#Confusion Matrix
confusionMatrix(insurance_v_bin$INS, insurance_v_bin$p_hat, threshold = 0.3)


#Accuracy
Test <- insurance_v_bin  %>% mutate(model_pred = 1*(p_hat > .3) + 0,
                         actual = 1*(INS == 1) + 0)

Test <- Test %>% mutate(accurate = 1*(model_pred == actual))
sum(Test$accurate)/nrow(Test)
#0.7123352



#Lift
pred <- prediction(insurance_v_bin$p_hat, factor(insurance_v_bin$INS))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

perf <- performance(pred, measure = "lift", x.measure = "rpp")

plot(perf, lwd = 3, col = "darkcyan",main = "Lift Chart for Validation Data")





