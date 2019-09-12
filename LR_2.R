newdata <- na.omit(insurance_t_bin)

library(tidyr)
library(dplyr)
library(ggplot2)

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


### VAR with missing value (INV, CCPURC, CC), HMOWN
insurance_t_bin[is.na(insurance_t_bin)] <- "Missing"

### Two Way Table: CASHBK, MMCRED, CCPURC have complete or quasi-separation concerns
for (i in seq_along(insurance_t_bin)){
  print(colnames(insurance_t_bin)[i])
  print(table(insurance_t_bin[[i]],insurance_t_bin[["INS"]]))  
}

###
insurance_t_bin$CASHBK[insurance_t_bin$CASHBK==2]=1
table(insurance_t_bin[["CASHBK"]],insurance_t_bin[["INS"]])

insurance_t_bin$MMCRED[insurance_t_bin$MMCRED==3]=2
insurance_t_bin$MMCRED[insurance_t_bin$MMCRED==5]=2
table(insurance_t_bin[["MMCRED"]],insurance_t_bin[["INS"]])

insurance_t_bin$CCPURC[insurance_t_bin$CCPURC==3]=2
insurance_t_bin$CCPURC[insurance_t_bin$CCPURC==4]=2
table(insurance_t_bin[["CCPURC"]],insurance_t_bin[["INS"]])


# Build Model
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


full.model <- glm(INS~
DDA+ CASHBK+ DIRDEP+ NSF+ ATM+ CD+ IRA+ LOC+ ILS+MM+MTG+SDB+MOVED+INAREA+ NSFAMT_Bin+ IRABAL_Bin+ ILSBAL_Bin+ MMBAL_Bin+ LORES_Bin
+factor(INV)+factor(MMCRED)+factor(CC)+factor(CCPURC)+factor(HMOWN)+factor(BRANCH)+factor(RES)+factor(DDABAL_Bin)+factor(ACCTAGE_Bin)+factor(DEPAMT_Bin)+factor(CHECKS_Bin)+factor(PHONE_Bin)+factor(TELLER_Bin)+factor(SAVBAL_Bin)+factor(ATMAMT_Bin)+factor(POS_Bin)+factor(POSAMT_Bin)+factor(CDBAL_Bin)+factor(LOCBAL_Bin)+factor(INVBAL_Bin)+factor(MTGBAL_Bin)+factor(CCBAL_Bin)+factor(INCOME_Bin)+factor(HMVAL_Bin)+factor(AGE_Bin)+factor(CRSCORE_Bin),
data = insurance_t_bin, family = binomial(link = "logit"))

qchisq(0.002,1,lower.tail=FALSE)

back.model <- step(full.model, direction = "backward", k=9.549536)

