library(tidyr)
library(dplyr)
library(ggplot2)

# Step 0: Looking at missing pattern
miss=na.omit(insurance_t_bin)

missing.values <- insurance_t_bin %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 


missing.values <- insurance_t_bin %>%
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

row.plot <- insurance_t_bin %>%
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
# Step 0 wrapup: VAR with missing value (INV, CCPURC, CC), HMOWN


# Step 1: Recode all NA to missing
insurance_t_bin[is.na(insurance_t_bin)] <- "Missing"


# Step 2: Loop for all two-way tables
for (i in seq_along(insurance_t_bin)){
  print(colnames(insurance_t_bin)[i])
  print(table(insurance_t_bin[[i]],insurance_t_bin[["INS"]]))  
}

viz=insurance_t_bin %>%
  select(INS,CASHBK,MMCRED)

write.csv(viz,"/Users/shao/Desktop/HW/Homework2_LR/sep.csv", row.names = FALSE)

# Step 2 wrapup: Two Way Table: CASHBK, MMCRED, CCPURC have complete or quasi-separation concerns



# Step 3: Recode vars have complete or quasi-separation concerns
insurance_t_bin$CASHBK[insurance_t_bin$CASHBK==2]=1
table(insurance_t_bin[["CASHBK"]],insurance_t_bin[["INS"]])

insurance_t_bin$MMCRED[insurance_t_bin$MMCRED==5]=3
table(insurance_t_bin[["MMCRED"]],insurance_t_bin[["INS"]])

# This is optional
# Look at whether variables are binary or catagorical
# This effect whether put factor() or not
checktype <- function(v, naVal = NULL) { 
  # get unique values
  v_unique <- unique(v)
  # remove 'naVal's
  v_unique2 <- v_unique[! v_unique %in% naVal]
  # count number of unique values and check whether all values are integers
  if ( length(unique(v_unique2)) == 2) "binary"
  else "others"
}

list=colnames(insurance_t_bin) 
type <- c()

for (i in list){
  type=append(type,checktype(insurance_t_bin[[i]]))}

vartype=data.frame(list,type)



# Step 4: Define full.model
full.model <- glm(INS~
DDA+ CASHBK+ DIRDEP+ NSF+ ATM+ CD+ IRA+ LOC+ ILS+MM+MTG+SDB+MOVED+INAREA+ NSFAMT_Bin+ IRABAL_Bin+ ILSBAL_Bin+ MMBAL_Bin+ LORES_Bin
+factor(INV)+factor(MMCRED)+factor(CC)+factor(CCPURC)+factor(HMOWN)+factor(BRANCH)+factor(RES)+factor(DDABAL_Bin)+factor(ACCTAGE_Bin)+factor(DEPAMT_Bin)+factor(CHECKS_Bin)+factor(PHONE_Bin)+factor(TELLER_Bin)+factor(SAVBAL_Bin)+factor(ATMAMT_Bin)+factor(POS_Bin)+factor(POSAMT_Bin)+factor(CDBAL_Bin)+factor(LOCBAL_Bin)+factor(INVBAL_Bin)+factor(MTGBAL_Bin)+factor(CCBAL_Bin)+factor(INCOME_Bin)+factor(HMVAL_Bin)+factor(AGE_Bin)+factor(CRSCORE_Bin),
data = insurance_t_bin, family = binomial(link = "logit"))

# Set alpha level to 0.002
qchisq(0.002,1,lower.tail=FALSE)

# Step 5: Main model selection
back.model <- step(full.model, direction = "backward", k=9.549536)

# Step 6: Model with interaction
int.model <- step(back.model, scope = . ~ .^2, direction = 'forward', k=9.549536)

# Step 7: P-values
# Main
main=glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
  factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
  factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_DDA=glm(INS ~  NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_NSF=glm(INS ~ DDA +  IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_IRA=glm(INS ~ DDA + NSF + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_ILS=glm(INS ~ DDA + NSF + IRA + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_MM=glm(INS ~ DDA + NSF + IRA + ILS + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_MTG=glm(INS ~ DDA + NSF + IRA + ILS + MM + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))



main_INV=glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + factor(CC) + 
             factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
             factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_CC=glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_DDABAL_Bin=glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_CHECKS_Bin=glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_TELLER_Bin=glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_SAVBAL_Bin=glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_ATMAMT_Bin=glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_CDBAL_Bin=glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) ,data = insurance_t_bin, family = binomial(link = "logit"))


anova(main,main_DDA,test="LRT")[2,5]
anova(main,main_NSF,test="LRT")[2,5]
anova(main,main_IRA,test="LRT")[2,5]
anova(main,main_ILS,test="LRT")[2,5]
anova(main,main_MM,test="LRT")[2,5]
anova(main,main_MTG,test="LRT")[2,5]
anova(main,main_INV,test="LRT")[2,5]
anova(main,main_CC,test="LRT")[2,5]
anova(main,main_DDABAL_Bin,test="LRT")[2,5]
anova(main,main_CHECKS_Bin,test="LRT")[2,5]
anova(main,main_TELLER_Bin,test="LRT")[2,5]
anova(main,main_SAVBAL_Bin,test="LRT")[2,5]
anova(main,main_ATMAMT_Bin,test="LRT")[2,5]
anova(main,main_CDBAL_Bin,test="LRT")[2,5]

# Final
main=glm(INS ~ DDA:IRA + DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
           factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
           factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

summary(main)

main_DDA=glm(INS ~  NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
               factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
               factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_NSF=glm(INS ~ DDA:IRA +DDA +  IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
               factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
               factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_IRA=glm(INS ~ DDA:IRA +DDA + NSF + ILS + MM + MTG + factor(INV) + factor(CC) + 
               factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
               factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_ILS=glm(INS ~ DDA:IRA +DDA + NSF + IRA + MM + MTG + factor(INV) + factor(CC) + 
               factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
               factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_MM=glm(INS ~ DDA:IRA +DDA + NSF + IRA + ILS + MTG + factor(INV) + factor(CC) + 
              factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
              factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_MTG=glm(INS ~ DDA:IRA +DDA + NSF + IRA + ILS + MM + factor(INV) + factor(CC) + 
               factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
               factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))



main_INV=glm(INS ~ DDA:IRA +DDA + NSF + IRA + ILS + MM + MTG + factor(CC) + 
               factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
               factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_CC=glm(INS ~ DDA:IRA +DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + 
              factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
              factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_DDABAL_Bin=glm(INS ~ DDA:IRA +DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
                      factor(CHECKS_Bin) + factor(TELLER_Bin) + 
                      factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_CHECKS_Bin=glm(INS ~ DDA:IRA +DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
                      factor(DDABAL_Bin) + factor(TELLER_Bin) + 
                      factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_TELLER_Bin=glm(INS ~ DDA:IRA +DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
                      factor(DDABAL_Bin) + factor(CHECKS_Bin) + 
                      factor(SAVBAL_Bin) + factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_SAVBAL_Bin=glm(INS ~ DDA:IRA +DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
                      factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
                      factor(ATMAMT_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_ATMAMT_Bin=glm(INS ~ DDA:IRA +DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
                      factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
                      factor(SAVBAL_Bin) + factor(CDBAL_Bin),data = insurance_t_bin, family = binomial(link = "logit"))

main_CDBAL_Bin=glm(INS ~ DDA:IRA +DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
                     factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
                     factor(SAVBAL_Bin) + factor(ATMAMT_Bin) ,data = insurance_t_bin, family = binomial(link = "logit"))

main_DDAIRA=glm(INS ~ DDA + NSF + IRA + ILS + MM + MTG + factor(INV) + factor(CC) + 
                     factor(DDABAL_Bin) + factor(CHECKS_Bin) + factor(TELLER_Bin) + 
                     factor(SAVBAL_Bin) + factor(ATMAMT_Bin) ,data = insurance_t_bin, family = binomial(link = "logit"))


anova(main,main_DDA,test="LRT")[2,5]
anova(main,main_NSF,test="LRT")[2,5]
anova(main,main_IRA,test="LRT")[2,5]
anova(main,main_ILS,test="LRT")[2,5]
anova(main,main_MM,test="LRT")[2,5]
anova(main,main_MTG,test="LRT")[2,5]
anova(main,main_INV,test="LRT")[2,5]
anova(main,main_CC,test="LRT")[2,5]
anova(main,main_DDABAL_Bin,test="LRT")[2,5]
anova(main,main_CHECKS_Bin,test="LRT")[2,5]
anova(main,main_TELLER_Bin,test="LRT")[2,5]
anova(main,main_SAVBAL_Bin,test="LRT")[2,5]
anova(main,main_ATMAMT_Bin,test="LRT")[2,5]
anova(main,main_CDBAL_Bin,test="LRT")[2,5]
anova(main,main_DDAIRA,test="LRT")[2,5]



#Final Exploration
insurance_t=insurance_t$DDABAL 
insurance_t_DDABAL=insurance_t_bin$DDABAL_Bin

insurance_t =insurance_t%>%
  sort()

insurance_t_DDABAL =insurance_t_DDABAL%>%
  sort()

df_DDABAL <- data.frame(CAT=insurance_t_DDABAL, VAL=insurance_t, stringsAsFactors = FALSE)

df_DDABAL_1 = df_DDABAL %>%
  filter(CAT==1)
summary(df_DDABAL_1)

df_DDABAL_8 = df_DDABAL %>%
  filter(CAT==8)
summary(df_DDABAL_8)

unique(insurance_t_bin$INV)



