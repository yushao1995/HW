library(gmodels)
library(vcd)
library(smbinning)
library(haven)
library(dplyr)
library(stringr)
library(plyr)

# Load Needed Data Sets #
accepts <- read_sas("~/Desktop/Homework1_FA/accepted_customers.sas7bdat")
rejects <- read_sas("~/Desktop/Homework1_FA/rejected_customers.sas7bdat")
set.seed(12345)
rejects=rejects[sample(nrow(rejects), 1000), ]
names(accepts)[26] <- "weight"
names(accepts)[25] <- "bad"
accepts$good <- abs(accepts$bad - 1)


# Setting Categorical Variables as Factors #
accepts$TITLE <- as.factor(accepts$TITLE)
accepts$TEL <- as.factor(accepts$TEL)
accepts$BUREAU <- as.factor(accepts$BUREAU)
accepts$NAT <- as.factor(accepts$NAT)
accepts$PRODUCT <- as.factor(accepts$PRODUCT)
accepts$PROF <- as.factor(accepts$PROF)
accepts$REGN <- as.factor(accepts$REGN)
accepts$RESID <- as.factor(accepts$RESID)



#table(accepts$CAR,accepts$good)
#accepts$CAR[accepts$CAR=="Car and Motor bi"]="Car"

#table(accepts$CARDS,accepts$good)
#accepts$CARDS[accepts$CARDS=="American Express"]="own credit cards"
#accepts$CARDS[accepts$CARDS=="Cheque card"]="own credit cards"
#accepts$CARDS[accepts$CARDS=="Mastercard/Euroc"]="own credit cards"
#accepts$CARDS[accepts$CARDS=="Other credit car"]="own credit cards"
#accepts$CARDS[accepts$CARDS=="VISA mybank"]="own credit cards"
#accepts$CARDS[accepts$CARDS=="VISA Others"]="own credit cards"

# Create Training and Validation #
set.seed(12345)
train_id <- sample(seq_len(nrow(accepts)), size = floor(0.7*nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]
train <- as.data.frame(train)
train <- as.data.frame(test)

table(train$good)
table(test$good)


# Information Value for Each Variable #
iv_summary <- smbinning.sumiv(df = train, y = "good")
smbinning.sumiv.plot(iv_summary)
iv_summary

# Binning of Continuous Variables - IV >= 0.1 #
num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

result_all_sig <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train, y = "good", x = num_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig[[num_names[i]]] <- check_res
  }
}



# Generating Variables of Bins and WOE Values #
for(i in 1:length(result_all_sig)) {
  train <- smbinning.gen(df = train, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(train)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(train[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

#Build Initial Logistic Regression #
#factor(CAR)+ factor(CARDS)
initial_score <- glm(good ~ CHILDREN_WOE+ PERS_H_WOE+ AGE_WOE+ TMJOB1_WOE+ INCOME_WOE,
                     weights = train$weight, family = "binomial", data = train)

summary(initial_score)



# Evaluate the Initial Model - Training Data #
train$pred <- initial_score$fitted.values

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", report = 1)
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", plot = "ks")
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", plot = "auc")


#Evaluate the Initial Model - Testing Data #
for(i in 1:length(result_all_sig)) {
  test <- smbinning.gen(df = test, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(test)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(test[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

test$pred <- predict(initial_score, newdata=test, type='response')

smbinning.metrics(dataset = test, prediction = "pred", actualclass = "good", report = 1)
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "good", plot = "ks")
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "good", plot = "auc")



# Add Scores to Initial Model #
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])


for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  train[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(train)-nvar + 1)
colend <- ncol(train)
train$Score <- rowSums(train[, colini:colend])

hist(train$Score, breaks = 50, xlim = c(475,725), main = "Distribution of Train Scores", xlab = "Score")

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  test[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(test)-nvar + 1)
colend <- ncol(test)
test$Score <- rowSums(test[, colini:colend])

hist(test$Score, breaks = 50, xlim = c(475,725), main = "Distribution of Test Scores", xlab = "Score")

accepts_scored <- rbind(train, test)
hist(accepts_scored$Score, breaks = 50, xlim = c(475,725), main = "Distribution of Scores", xlab = "Score")
