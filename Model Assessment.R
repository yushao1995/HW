###############################
#                             #
#     Logistic Regression:    #
#       Model Assessment      #
#                             #
#        Dr Aric LaBarr       #
#                             #
###############################

# Needed Libraries for Analysis #
install.packages("MASS")
install.packages("car")
install.packages("DescTools")
install.packages("ggplot2")
install.packages("ROCR")
install.packages("InformationValue")

library(MASS)
library(car)
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)

# Load Needed Data Sets #
# Replace the ... below with the file location of the data sets #
setwd("...")

bwt <- read.csv(file = "lowbwt.csv", header = TRUE)

# Generalized R-squared #
logit.model <- glm(low ~ lwt + factor(smoke) + factor(race), 
                   data = bwt, family = binomial(link = "logit"))
summary(logit.model)

AIC(logit.model)
BIC(logit.model)
PseudoR2(logit.model, which = "Nagelkerke")

# Brier Score #
BrierScore(logit.model)

# Discrimination Slope #
bwt$p_hat <- predict(logit.model, type = "response")

p1 <- bwt$p_hat[bwt$low == 1]
p0 <- bwt$p_hat[bwt$low == 0]
coef_discrim <- mean(p1) - mean(p0)

ggplot(bwt, aes(p_hat, fill = factor(low))) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability",
       fill = "Outcome",
       title = paste("Coefficient of Discrimination = ",
                     round(coef_discrim, 3), sep = ""))

# Rank-order Statistics #
Concordance(bwt$low, bwt$p_hat)

somersD(bwt$low, bwt$p_hat)

# Classification Table & Youden Index #
confusionMatrix(bwt$low, bwt$p_hat, threshold = 0.5)

sens <- NULL
spec <- NULL
youden <- NULL
cutoff <- NULL

for(i in 1:49){
  cutoff = c(cutoff, i/50)
  sens <- c(sens, sensitivity(bwt$low, bwt$p_hat, threshold = i/50))
  spec <- c(spec, specificity(bwt$low, bwt$p_hat, threshold = i/50))
  youden <- c(youden, youdensIndex(bwt$low, bwt$p_hat, threshold = i/50))
}

ctable <- data.frame(cutoff, sens, spec, youden)

ctable

# ROC Curve - InformationValue Package #
plotROC(bwt$low, bwt$p_hat)
AUROC(bwt$low, bwt$p_hat)

# ROC Curve - ROCR Package #
pred <- prediction(fitted(logit.model), factor(bwt$low))
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
  reca <- c(reca, sensitivity(bwt$low, bwt$p_hat, threshold = i/50))
  prec <- c(prec, precision(bwt$low, bwt$p_hat, threshold = i/50))
  f1 <- c(f1, 2*((prec[i]*reca[i])/(prec[i]+reca[i])))
}

ctable <- data.frame(cutoff, reca, prec, f1)

ctable

# Lift Chart #
perf <- performance(pred, measure = "lift", x.measure = "rpp")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)),
     main = "Lift Chart for Training Data")
abline(h = 1, lty = 3)

# K-S Statistics #
ks_plot(bwt$low, bwt$p_hat)
ks_stat(bwt$low, bwt$p_hat)

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
