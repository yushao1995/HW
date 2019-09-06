# 1
# 1-1 
# Check variables type
checktype <- function(v, naVal = NULL) { 
  if( !is.numeric(v) ) stop("Only numeric vectors are accepted.")
  # remove NA's
  v2 <- na.omit(v)
  # get unique values
  v_unique <- unique(v2)
  # remove 'naVal's
  v_unique2 <- v_unique[! v_unique %in% naVal]
  # count number of unique values and check whether all values are integers
  if ( length(unique(v_unique2)) > 10) "continous"
  else if ( length(unique(v_unique2)) > 2) "ordinal"
  else "binary"
}

list=colnames(insurance_t) 
type <- c()

for (i in list){
  type=append(type,checktype(insurance_t[[i]]))}

type=append(type,"nom")
type=append(type,"nom")

datasummary=data.frame(list,type)


# 1-2
# p-value for continous variables
convar=subset(datasummary,type=="continous")
dep_vars <- c("INS") 
ind_vars <- droplevels(convar$list)
var_comb <- expand.grid(dep_vars, ind_vars ) 
formula_vec <- sprintf("%s ~ %s", var_comb$Var1, var_comb$Var2)
pvalue <- c()
for (model in formula_vec){
  logit.model<- glm(model, data = insurance_t, family = binomial(link = "logit"))
  pvalue=append(pvalue,coef(summary(logit.model))[2,4])
}
convar["pvalue"] <- NA
convar$pvalue <- pvalue


# 1-3
# p-value for binary variables
x=insurance_t$DDA
y=insurance_t[,"DDA"]

class(insurance_t[,"DDA"])


library(vcdExtra)
ctest <- function(x){CMHtest(table(x,insurance_t$INS))$table[1,3]}
ctest(insurance_t$DDA)
ctest(insurance_t$DIRDEP)
ctest(insurance_t$SAV)
ctest(insurance_t$ATM)
ctest(insurance_t$CD)
ctest(insurance_t$IRA)
ctest(insurance_t$LOC)
ctest(insurance_t$INV)
ctest(insurance_t$ILS)
ctest(insurance_t$MM)
ctest(insurance_t$MTG)
ctest(insurance_t$CC)
ctest(insurance_t$SDB)
ctest(insurance_t$HMOWN)
ctest(insurance_t$MOVED)
ctest(insurance_t$INAREA)
# Helpppppppppppp, I don't know how to loop this


# 1-4
# p-value for oridnal variables
ctest(insurance_t$CASHBK)
ctest(insurance_t$MMCRED)
ctest(insurance_t$CCPURC)


# 1-5
# p-value for nominal variables
chisq.test(table(insurance_t$BRANCH,insurance_t$INS))[3]
chisq.test(table(insurance_t$RES,insurance_t$INS))[3]

#################################################################
# 2 
# Odd Ratio for Binary
binvar=subset(datasummary,type=="binary")
binvar=subset(binvar, list!="INS")

dep_vars <- c("INS") 
ind_vars <- droplevels(binvar$list)
var_comb <- expand.grid(dep_vars, ind_vars ) 
formula_vec <- sprintf("%s ~ %s", var_comb$Var1, var_comb$Var2)
odd <- c()
for (model in formula_vec){
  logit.model<- glm(model, data = insurance_t, family = binomial(link = "logit"))
  odd=append(odd,exp(coef(logit.model))[2])
}
binvar["odd"] <- NA
binvar$odd <- odd
#################################################################
# 3
# Missing Value and Visualizaion

newdata <- na.omit(insurance_t)

library(tidyr)
library(dplyr)
library(ggplot2)

missing.values <- insurance_t %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 


missing.values <- insurance_t %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)


levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('#F3EBDD','#002C54'), labels = c("Present", "Missing")) +
  theme(text=element_text(size=14,  family="Verdana"))+
  coord_flip() +
  theme(panel.background = element_blank())+
  labs(title = "Percentage of Missing Values", x =
         'Variable', y = "% of Missing Values")

percentage.plot

row.plot <- insurance_t %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  ggplot(aes(key, id, fill = isna)) +
  geom_raster(alpha=0.8) +
  scale_fill_manual(name = "",
                    values = c('#F3EBDD','#002C54'),
                    labels = c("Present", "Missing")) +
  scale_x_discrete(limits = levels) +
  labs(x = "Variable",
       y = "Row Number", title = "Missing Values in Rows") +
  theme(text=element_text(size=14,  family="Verdana"))+
  theme(panel.background = element_blank())+
  coord_flip()

row.plot

#################################################################
# 4
# Find Redundant Set
library(regclass)
logit.model<- glm(INS~POSAMT+POS+PHONE+INVBAL+INV+CCPURC+CCBAL+CC, data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~HMOWN+LORES+INCOME+HMVAL+AGE, data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~DDABAL+DDA,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~NSFAMT+NSF,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~SAVBAL+SAV,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~ATMAMT+ATM,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~CDBAL+CD,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~IRABAL+IRA,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~LOCBAL+LOC,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~INVBAL+INV,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~ILSBAL+ILS,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~MMBAL+MMCRED+MM,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~MTGBAL+MTG,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~CCBAL+CCPURC+CC,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)
VIF(logit.model)

logit.model<- glm(INS~INVBAL,data = insurance_t, family = binomial(link = "logit"))
summary(logit.model)

logit_1<- glm(INS~CCBAL+CCPURC+CC,data = insurance_t, family = binomial(link = "logit"))
logit_2<- glm(INS~CC,data = insurance_t, family = binomial(link = "logit"))
anova(logit_1,logit_2,test='LRT')

logit_1<- glm(INS~CCBAL+CCPURC+CC,data = insurance_t, family = binomial(link = "logit"))
logit_2<- glm(INS~CCPURC,data = insurance_t, family = binomial(link = "logit"))
anova(logit_1,logit_2,test='LRT')

logit_1<- glm(INS~NSF+NSFAMT,data = insurance_t, family = binomial(link = "logit"))
logit_2<- glm(INS~NSFAMT,data = insurance_t, family = binomial(link = "logit"))
anova(logit_1,logit_2,test='LRT')

logit_1<- glm(INS~CCPURC+CCBAL+CC,data = insurance_t, family = binomial(link = "logit"))
logit_2<- glm(INS~CC,data = insurance_t, family = binomial(link = "logit"))
anova(logit_1,logit_2,test='LRT')