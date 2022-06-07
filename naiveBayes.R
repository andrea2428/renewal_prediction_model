#-------------------------------------------------------------------------------
#                             Naivni Bajes
#-------------------------------------------------------------------------------

library("bnlearn")
library(e1071)
library(caret)
library(pROC)
library(nortest)

source("util.R")

dataset = readRDS("preprocessed_data.RDS")
##JJ: dodato nekoliko prethodnih redova


#Provera da li podlezu Normalnoj raspodeli numericke varijable 
numericke<-dataset[, sapply(dataset, class) == "numeric"]
# numericke 

lapply(1:12, function(x) ad.test(numericke[,x]$p.value))


#Zakljucak: Nijedna ne podleze normanoj raspodeli i potrebna je diskretizacija

#Analiza i transformacija numerickih varijabli

summary(dataset$NoSubsIn_CB) #1,2,3,4,5
ggplot(data = dataset, mapping = aes(x = NoSubsIn_CB)) + geom_histogram(bins = 20) 
dataset$NoSubsIn_CB <- ifelse(test = dataset$NoSubsIn_CB > 1, yes = "Two or more", no = "One")
table(dataset$NoSubsIn_CB)
dataset$NoSubsIn_CB<-as.factor(dataset$NoSubsIn_CB)

summary(dataset$NoVoiceSubs) #1,2,3,4,5
ggplot(data = dataset, mapping = aes(x = NoVoiceSubs)) + geom_histogram(bins = 20) 
dataset$NoVoiceSubs <- ifelse(test = dataset$NoVoiceSubs > 1, yes = "Two or more", no = "One")
table(dataset$NoVoiceSubs)
dataset$NoVoiceSubs<-as.factor(dataset$NoVoiceSubs)

summary(dataset$NoSuspensions) #0,1
ggplot(data = dataset, mapping = aes(x = NoSuspensions)) + geom_histogram(bins = 20) 
table(dataset$NoSuspensions)
dataset$NoSuspensions<-as.factor(dataset$NoSuspensions)

summary(dataset$NoAfterSales) #0,1
ggplot(data = dataset, mapping = aes(x = NoAfterSales)) + geom_histogram(bins = 20) 
table(dataset$NoAfterSales)
dataset$NoAfterSales<-as.factor(dataset$NoAfterSales)

summary(dataset$NoTransactionsInShops) #0,1,2,3,4,5,6
ggplot(data = dataset, mapping = aes(x = NoTransactionsInShops)) + geom_histogram(bins = 20) 
dataset$NoTransactionsInShops <- ifelse(test = dataset$NoTransactionsInShops > 0, yes = "One or more", no = "None")
table(dataset$NoTransactionsInShops)
dataset$NoTransactionsInShops<-as.factor(dataset$NoTransactionsInShops)

#-------------------------------------------------------------------------------

summary(dataset$CB) #23-36
class(dataset$CB)
ggplot(data = dataset, mapping = aes(x = CB)) + geom_histogram(bins = 20) 
dataset$CB <- ifelse(test = dataset$CB <= 24, yes = "2 years", no = "More than 2y")
table(dataset$CB)
dataset$CB<-as.factor(dataset$CB)


summary(dataset$Tenure) #21-155
class(dataset$Tenure)
table(dataset$Tenure)
dataset[, c("Tenure", "Tenure_new")]
ggplot(data = dataset, mapping = aes(x = Tenure)) + geom_histogram(bins = 20) 
dataset$Tenure_new <- ifelse(test = dataset$Tenure >= 141, yes = "12 years", no = 
                           ifelse(test=dataset$Tenure>=117, yes="10 years", no =
                                    ifelse(test=dataset$Tenure>=93, yes="8 years", no=
                                             ifelse(test=dataset$Tenure>=69, yes="6 years", no=
                                                      ifelse(test=dataset$Tenure>=45, yes="4 years", no=
                                                               ifelse(test=dataset$Tenure>=21, yes="2 years", no= "1 year"))))))
dataset$Tenure<-dataset$Tenure_new
dataset$Tenure_new<-NULL

table(dataset$Tenure)
dataset$Tenure<-as.factor(dataset$Tenure)

summary(dataset$Age) #20-94
ggplot(data = dataset, mapping = aes(x = Age)) + geom_histogram(bins = 20) 
dataset$Age <- ifelse(test = dataset$Age >= 60, yes = "Seniors", no = 
                        ifelse(test=dataset$Age>=26, yes="Adults", no= "Youths"))
table(dataset$Age)
dataset$Age<-as.factor(dataset$Age)

summary(dataset$max_months_to_contract_expiry) #3-34
ggplot(data = dataset, mapping = aes(x = max_months_to_contract_expiry)) + geom_histogram(bins = 20) 
dataset$max_months_to_contract_expiry <- ifelse(test = dataset$max_months_to_contract_expiry <= 6, yes = "Less than 6m", no = "More than 6m")
table(dataset$max_months_to_contract_expiry)
dataset$max_months_to_contract_expiry<-as.factor(dataset$max_months_to_contract_expiry)


summary(dataset$Kb_scoring) 
ggplot(data = dataset, mapping = aes(x = Kb_scoring)) + geom_histogram(bins = 20) 


summary(dataset$Fixed_fees)
ggplot(data = dataset, mapping = aes(x = Fixed_fees)) + geom_histogram(bins = 20) 


#Diskretizacija 

to_discretize <- c("Kb_scoring","Fixed_fees")
discretized <- discretize(data =dataset[,to_discretize],
                          method = "quantile",
                          breaks = c(3,5))
str(discretized)
summary(discretized)

dataset.new <- cbind(discretized, dataset[,c(1:6,8:17,19:24)])
dataset.new <- dataset.new[,names(dataset)]
str(dataset.new )


#Podela na train i test

set.seed(123)
train.indices <- createDataPartition(dataset.new$renewal_target,
                                     p = .80, list = FALSE) 
train <- dataset.new[train.indices,]
test <- dataset.new[-train.indices,]

train <-train[, names(train) != "ID"] 
test <-test[, names(test) != "ID"] 


#Kreiranje prvog modela

nb1 <- naiveBayes(renewal_target ~ ., data = train)
print(nb1)
nb1.pred <- predict(nb1, newdata = test, type = 'class')
nb1.cm <- table(true = test$renewal_target, predicted = nb1.pred)
nb1.cm
nb1.eval <- compute.eval.metrics(nb1.cm)
nb1.eval

#Auc
nb1.pred.prob <- predict(nb1, newdata = test, type = "raw") 
nb1.roc <- roc(response = as.numeric(test$renewal_target),
               predictor = nb1.pred.prob[,2],
               levels = c(1, 2))
plot(nb1.roc)
nb1.roc$auc


#Drugi model sa znacajnim atributima

nb2 <- naiveBayes(renewal_target ~ ., data = subset(train, select=-c( Sales_region, MigrationFlag, OnlineBillFlag)))
print(nb2)
nb2.pred <- predict(nb2, newdata = test, type = 'class')
nb2.cm <- table(true = test$renewal_target, predicted = nb2.pred)
nb2.cm
nb2.eval <- compute.eval.metrics(nb2.cm)
nb2.eval

#Auc
nb2.pred.prob <- predict(nb2, newdata = test, type = "raw") 
nb2.roc <- roc(response = as.numeric(test$renewal_target),
               predictor = nb2.pred.prob[,2],
               levels = c(1, 2))
plot(nb2.roc)
nb2.roc$auc


#Odabir optimalnog praga znacajnosti
nb2.coords <- coords(nb2.roc,
                     ret = c("accuracy", "spec", "sens", "thr"),
                     x = "local maximas",  
                     transpose = FALSE)
nb2.coords
nb2.coords[nb2.coords$sensitivity > 0.65 & nb2.coords$specificity > 0.5,]


coordsresult.coords <- coords(nb2.roc, "best", 
                              best.method="youden", #closest.topleft
                              ret=c("threshold", "accuracy"))
#result.coords
coordsresult.coords
threshold = coordsresult.coords$threshold

#Kreiranje treceg modela sa optimalnim pragom
nb2.pred2 <- ifelse(test = nb2.pred.prob[,2] >= threshold, 
                    yes = "Yes", 
                    no = "No") 
nb2.pred2 <- as.factor(nb2.pred2)

nb2.cm2 <- table(actual = test$renewal_target, predicted = nb2.pred2)
nb2.cm2

nb2.pred2.eval <- compute.eval.metrics(nb2.cm2)
nb2.pred2.eval
