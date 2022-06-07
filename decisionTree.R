#-------------------------------------------------------------------------------
#                           Drvo odlucivanja
#-------------------------------------------------------------------------------

##JJ: dodato:

library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(pROC)

source("util.R")

dataset = readRDS("preprocessed_data.RDS")

#------------------------Train i test-------------------------------------------


set.seed(123)
train.indices <- createDataPartition(dataset$renewal_target,
                                     p = .80, list = FALSE)
train.data <- dataset[train.indices,]
test.data <- dataset[-train.indices,]

train.data <-train.data[, names(train.data) != "ID"]
test.data <-test.data[, names(test.data) != "ID"]


#Pocetno stablo sa predefinisanim parametrima
tree1 <- rpart(renewal_target ~ ., data = train.data, method = "class",
               control = rpart.control(minsplit = 350, cp = 0.001))
print(tree1)
rpart.plot(tree1)


#Predikcija
tree1.pred <- predict(object = tree1, newdata = test.data, type = "class")
tree1.cm <- table(true=test.data$renewal_target, predicted=tree1.pred)
tree1.cm

#Evaluacione metrike i rezultati
tree1.eval <- compute.eval.metrics(tree1.cm)
tree1.eval

#Auc

prob<-predict(tree1,test.data,type="prob")
prob
auc<-auc(test.data$renewal_target, prob[,2])
auc
plot(roc(test.data$renewal_target, prob[,2]))
roc<-roc(test.data$renewal_target, prob[,2])
roc

#Cross validation
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001, to = 0.05, by = 0.0025)) 

which( colnames(train.data)=="renewal_target" )

set.seed(123)
dataset.cv <- train(x = train.data[,-3], 
                    y = train.data$renewal_target, 
                    method = "rpart", 
                    trControl = numFolds, 
                    tuneGrid = cpGrid)
dataset.cv
plot(dataset.cv)
optimal_cp <- dataset.cv$bestTune$cp

#Kreiranje drugog stabla sa optiamalnim parametrima iz kros-validacije
set.seed(123)
tree2 <- rpart(renewal_target ~ ., data = train.data, method = "class",
               control = rpart.control(cp = optimal_cp))
			   
#Predikcija
tree2.pred <- predict(object = tree2, newdata = test.data, type = "class")
tree2.cm <- table(true = test.data$renewal_target, predicted = tree2.pred)
tree2.cm

#Evaluacione metrike i rezultati
tree2.eval <- compute.eval.metrics(tree2.cm)
tree2.eval

#Auc
prob<-predict(tree2,test.data,type="prob")
auc<-auc(test.data$renewal_target, prob[,2])
auc


#Kreiranje treceg stabla odlucivanja sa znacajnim varijablama
tree3 <- rpart(renewal_target ~ ., 
               data = subset(train.data, select=-c(Sales_region, MigrationFlag, OnlineBillFlag)), 
               method = "class",
               control = rpart.control(minsplit = 350, cp = 0.001))
print(tree3)
rpart.plot(tree3)

#Predikcija
tree3.pred <- predict(object = tree3, newdata = test.data, type = "class")
tree3.cm <- table(true=test.data$renewal_target, predicted=tree3.pred)
tree3.cm

#Evaluacione metrike i rezultati
tree3.eval <- compute.eval.metrics(tree3.cm)
tree3.eval

#Auc
prob<-predict(tree3,test.data,type="prob")
auc<-auc(test.data$renewal_target, prob[,2])
auc
