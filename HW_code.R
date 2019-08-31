# 1.1 Explore all predictor variables
#     Basic logit model
logit.model_1 <- glm(INS ~ ACCTAGE, data = insurance_t, family = binomial(link = "logit"))
summary(logit.model_1)
#     Find the p-value
#     Summary only significant

#################################################################
# 1.2 Indentify type of variables (self-learning)
#     Gen function to tell whether numeric var is continous or binary
checkBinaryTrait <- function(v, naVal = NULL) { 
  if( !is.numeric(v) ) stop("Only numeric vectors are accepted.")
  # remove NA's
  v2 <- na.omit(v)
  # get unique values
  v_unique <- unique(v2)
  # remove 'naVal's
  v_unique2 <- v_unique[! v_unique %in% naVal]
  # count number of unique values and check whether all values are integers
  if ( length(unique(v_unique2)) > 2L || 
       any(as.integer(v_unique2) != v_unique2) ) "con" else "bin"
}

#     Gen function to tell whether string var is nominal or ordinal
checkstring <- function(v, naVal = NULL) { 
  # remove NA's
  v2 <- na.omit(v)
  # get unique values
  v_unique <- unique(v2)
  # remove 'naVal's
  v_unique2 <- v_unique[! v_unique %in% naVal]
  # count number of unique values and check whether all values are integers
  if ( length(unique(v_unique2)) > 2L || 
       any(as.integer(v_unique2) != v_unique2) ) "nom" else "ord"
}

#     Gen to null vector
list=colnames(insurance_t) 
type <- c()

#     For loop to indentify type of each value
#     Not the final version, this code using the fact that numeric variables put in the begining in dataset
for (i in colnames(insurance_t)){
  type=append(type,checkBinaryTrait(insurance_t[[i]]))}

#     Cheating code to add value manually   
type=append(type,"nom")
type=append(type,"nom")

datasummary=data.frame(list,type)
#################################################################


# 2   Binary variables' odds ratios

# 2.1 Gen binary set of dataframe "binvar"
binvar=subset(datasummary,type=="bin")
#     remove dependent var "INS"
binvar=subset(binvar, list!="INS")

# 2.2 Gen loop to run logit model and get p-values and odd ratio

#     Gen list of dependent variable and independent variables
dep_vars <- c("INS") 
ind_vars <- droplevels(binvar$list)
# pair with dep_vars:
var_comb <- expand.grid(dep_vars, ind_vars ) 
# formulas for all combinations
formula_vec <- sprintf("%s ~ %s", var_comb$Var1, var_comb$Var2)

#     loopppppp for gen list of "pvalue" and "odd"
pvalue <- c()
odd <- c()
for (model in formula_vec){
  logit.model<- glm(model, data = insurance_t, family = binomial(link = "logit"))
  pvalue=append(pvalue,coef(summary(logit.model))[2,4])
  odd=append(odd,exp(coef(logit.model))[2])
}
#     Combine list to dataframe
binvar["pvalue"] <- NA
binvar$pvalue <- pvalue
binvar["odd"] <- NA
binvar$odd <- odd

# 2.3 Rank these odds ratios by magnitude.
#################################################################
# 3   Continuous variables' linearity assumption
#################################################################
# 4.1 Data Consideration_missing value
#     visualization option 1
testvar <- c("ACCTAGE", "DDA","DDABAL","DEP","DEPAMT")
testset <- insurance_t[testvar]
library(naniar)
library(ggplot2)
gg_miss_var(testset) + labs(y = "Look at all the missing ones")
#     visualization option 2
library(visdat)
vis_miss(testset)

# 4.2 Data Consideration_redundant information
# 4.3 Interestingfindings

#################################################################