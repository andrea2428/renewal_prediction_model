#install.packages('corrplot')
#install.packages('ggplot2')
#install.packages('GGally')
#install.packages('readxl')
#install.packages('nortest')
#install.packages("randomForest")
#install.packages("arules")
#-----------------------Ucitavanje biblioteka-----------------------------------

library('corrplot')
library('ggplot2')
library("readxl")
# library(nortest) ##JJ: ovaj paket se ne koristi
library('GGally')
library("plyr")
## JJ: paketi u nastavku su nepotrebni za ovaj skript
## treba ih prebaciti u onaj skript gde se stvarno koriste
# library(randomForest)
# library(caret)
# library(e1071)
# library(ROCR)
# library(pROC)
# library(rpart)
# library(rpart.plot)
# library(arules)

#Ucitavanje dataseta
dataset <- read_excel("Dataset - Copy.xlsx")

#------------------------------Nazivi kolona------------------------------------

# names(dataset)[names(dataset) == "HWSimFlag_before_rnw"] <- "HwSimFlagBeforeRnw"
names(dataset)[names(dataset) == "HWSimFlag_before_rnw"] <- "HwSimFlagBeforeRnw"
names(dataset)[names(dataset) == "mnp_flag"] <- "MNP" #Mobile number portability
names(dataset)[names(dataset) == "value_segment"] <- "Value_segment" 
names(dataset)[names(dataset) == "T_Num"] <- "Tariff_Num" 
names(dataset)[names(dataset) == "sales_region"] <- "Sales_region" 
names(dataset)[names(dataset) == "age"] <- "Age" 
names(dataset)[names(dataset) == "avg_bill_amt_last_20m"] <- "Avg_Bill_Amt_Last20m" 
names(dataset)[names(dataset) == "tenure"] <- "Tenure" 
names(dataset)[names(dataset) == "kb_scoring"] <- "Kb_scoring" 


#Kategoricke promenljive, raspodela
kategoricke<-dataset[, sapply(dataset, class) != "numeric"]


numericke<-dataset[, sapply(dataset, class) == "numeric"]
numericke 


#------------------Sredjivanje tipa podataka------------------------------------


#Analiza varijabli
summary(dataset)
str(dataset)


#Kategoricke u faktorske

dataset$Kb_scoring <- as.numeric(dataset$Kb_scoring)

dataset$max_months_to_contract_expiry <-as.numeric(dataset$max_months_to_contract_expiry)

table(dataset$CB)  
dataset$CB<-as.numeric(dataset$CB)

table(dataset$Tariff_Type)
revalue(dataset$Tariff_Type, c("NOVA Tariffs" = "NEO Tariffs")) -> dataset$Tariff_Type

table(dataset$Value_segment)
revalue(dataset$Value_segment, c("NA" = "grey")) -> dataset$Value_segment

table(dataset$Tariff_Num)
revalue(dataset$Tariff_Num, c("T3a" = "T3")) -> dataset$Tariff_Num

table(dataset$Channel)
revalue(dataset$Channel, c("Business Sales" = "Other")) -> dataset$Channel
revalue(dataset$Channel, c("Internal use" = "Other")) -> dataset$Channel
revalue(dataset$Channel, c("Tactical channel" = "Other")) -> dataset$Channel

table(dataset$MigrationFlag)

table(dataset$OnlineBillFlag)

table(dataset$Tariff_Num_Prev)
revalue(dataset$Tariff_Num_Prev, c("NA" = "T3")) -> dataset$Tariff_Num_Prev

dataset<-dataset[dataset$Tariff_Num_Prev != "T7",]

table(dataset$Tariff_Type_Prev)
revalue(dataset$Tariff_Type_Prev, c("Internal Tariffs" = "Other")) -> dataset$Tariff_Type_Prev
revalue(dataset$Tariff_Type_Prev, c("Residential Tariffs" = "Other")) -> dataset$Tariff_Type_Prev
revalue(dataset$Tariff_Type_Prev, c("Try&Buy Tariffs" = "Other")) -> dataset$Tariff_Type_Prev
revalue(dataset$Tariff_Type_Prev, c("Youth Tariffs" = "Other")) -> dataset$Tariff_Type_Prev
revalue(dataset$Tariff_Type_Prev, c("Business Tariffs" = "Other")) -> dataset$Tariff_Type_Prev
revalue(dataset$Tariff_Type_Prev, c("Smart Tariffs" = "Other")) -> dataset$Tariff_Type_Prev
##JJ: Trebalo bi i Smart Tariffs prebaciti u Other, ima svega 20 opservacija
#ISPRAVLJENO

table(dataset$Sales_region)
revalue(dataset$Sales_region, c("Dummy" = NA)) -> dataset$Sales_region

dataset<-dataset[!is.na(dataset$Sales_region),]
#--------------------Izlazna varijabla------------------------------------------

dataset$renewal_target<-as.factor(dataset$renewal_target)

table(dataset$renewal_target)
prop.table(table(dataset$renewal_target))

#----------------------------Numericke transformacije---------------------------

summary(dataset$Age)
dataset <- dataset[dataset$Age < 100,]  #mladji od 100 godina

table(dataset$BILL_CYCLE) #treba u faktorsku
dataset$BILL_CYCLE <- as.factor(dataset$BILL_CYCLE) 

dataset<-dataset[dataset$Fixed_fees>0,]

dataset$Tenure <- as.numeric(dataset$Tenure)

table(dataset$NoSubsIn_CB)
dataset$NoSubsIn_CB[dataset$NoSubsIn_CB==0] <- 1

table(dataset$NoMBBSubs)
dataset$NoMBBSubs[dataset$NoMBBSubs!=0] <- 1

table(dataset$NoSuspensions)
dataset$NoSuspensions[dataset$NoSuspensions!=0] <- 1

table(dataset$NoFraudSuspensions)
dataset$NoFraudSuspensions[dataset$NoFraudSuspensions!=0] <- 1

table(dataset$NoAfterSales)
dataset$NoAfterSales[dataset$NoAfterSales!=0] <- 1

table(dataset$NoComplaints)
dataset$NoComplaints[dataset$NoComplaints!=0] <- 1

table(dataset$NoTransactionsInShops)
dataset$NoTransactionsInShops[dataset$NoTransactionsInShops>6] <- 7


#--------------------------Missing values---------------------------------------


number_of_NA <-apply(X = dataset,2,FUN = function(x) length(which(is.na(x))))
number_of_NA  

sum(!complete.cases(dataset))
dataset<-dataset[complete.cases(dataset),]


#---------------------Korelacija------------------------------------------------

#Analiza zavisnosti numerickih varijabli

kruskal.test(renewal_target ~ Fixed_fees, data = dataset)
kruskal.test(renewal_target ~ Customer_value, data = dataset)
kruskal.test(renewal_target ~ Tenure, data = dataset)
kruskal.test(renewal_target ~ Age, data = dataset)
kruskal.test(renewal_target ~ max_months_to_contract_expiry, data = dataset)
kruskal.test(renewal_target ~ NoSubsIn_CB, data = dataset)
kruskal.test(renewal_target ~ NoMBBSubs, data = dataset)
kruskal.test(renewal_target ~ NoSuspensions, data = dataset)
kruskal.test(renewal_target ~ NoFraudSuspensions, data = dataset)
kruskal.test(renewal_target ~ Kb_scoring, data = dataset)
kruskal.test(renewal_target ~ NoComplaints, data = dataset)
kruskal.test(renewal_target ~ NoAfterSales, data = dataset)
kruskal.test(renewal_target ~ NoTransactionsInShops, data = dataset)
kruskal.test(renewal_target ~ Avg_Bill_Amt_Last20m, data = dataset)

#Uklanjanje nezavisnih varijabli

dataset<-subset(dataset, select=-c(Customer_value,NoMBBSubs,NoFraudSuspensions,NoComplaints,Avg_Bill_Amt_Last20m))


#Analiza zavisnosti kategorickih varijabli

chisq.test(dataset$HwSimFlagBeforeRnw, dataset$renewal_target)
chisq.test(dataset$MNP, dataset$renewal_target)
chisq.test(dataset$CB, dataset$renewal_target)
chisq.test(dataset$BILL_CYCLE, dataset$renewal_target)
chisq.test(dataset$Value_segment, dataset$renewal_target)
chisq.test(dataset$Tariff_Type , dataset$renewal_target)
chisq.test(dataset$Tariff_Num, dataset$renewal_target)
chisq.test(dataset$Sales_region, dataset$renewal_target)
chisq.test(dataset$Channel, dataset$renewal_target)
chisq.test(dataset$MigrationFlag, dataset$renewal_target)
chisq.test(dataset$OnlineBillFlag, dataset$renewal_target)
chisq.test(dataset$Tariff_Num_Prev, dataset$renewal_target)
chisq.test(dataset$Tariff_Type_Prev, dataset$renewal_target)

#Sve su medjusobno zavisne sa izlaznom promenljivom osim Bill_Cycle

dataset<-as.data.frame(dataset)
dataset<-subset(dataset, select=-c(BILL_CYCLE))

#Cuvanje pripremljenih podataka, tako da se mogu koristiti u narednim skriptama u kojima ce se kreirati modeli
saveRDS(dataset, "preprocessed_data.RDS")

