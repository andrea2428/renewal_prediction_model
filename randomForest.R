#-------------------------------------------------------------------------------
#                         Random Forest
#-------------------------------------------------------------------------------

library(randomForest)
library(caret)
library(e1071)
library(pROC)

source("util.R")

dataset = readRDS("preprocessed_data.RDS")

set.seed(123)
train.indices <- createDataPartition(dataset$renewal_target,
                                     p = .80, list = FALSE)
train <- dataset[train.indices,]
test <- dataset[-train.indices,]

train <-train[, names(train) != "ID"]
test <-test[, names(test) != "ID"]

#Kreiranje prvog modela sa default parametrima
rf1 <- randomForest(renewal_target~., data=train,  
                    ntree = 500,
                    mtry = 3,
                    importance = TRUE,
                    proximity = TRUE)

rf1.pred <- predict(rf1, test)
rf1.pred
rf1.cm <- table(true = test$renewal_target, predicted = rf1.pred)
rf1.cm 
rf1.eval <- compute.eval.metrics(rf1.cm)
rf1.eval
print(rf1) #OOB = 33,4

rf1.pred.prob <- predict(object = rf1, newdata = test,type="prob")
rf1.roc <-roc(response = as.numeric(test$renewal_target),
              predictor = rf1.pred.prob[,2] ,  
              levels = c(1, 2))

rf1.auc <- rf1.roc$auc 
rf1.auc  #0,696

# Variable Importance
importance(rf1)
varImpPlot(rf1,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")


#Kreiranje drugog modela sa optimalnim parametrima

tunemtry <-tuneRF(train[,-c(3)],
                  train[,3],
                  stepFactor = 2,   
                  plot = TRUE,
                  ntreeTry = 1000, 
                  trace = TRUE,
                  improve = 0.01)
#mtry=2
tunemtry
bestmtry<-2

rf2 <- randomForest(renewal_target~., data=train,  
                    ntree = 1500,
                    mtry = bestmtry, 
                    importance = TRUE,
                    proximity = TRUE)
rf2
rf2.pred <- predict(rf2, test)
rf2.cm <- table(true = test$renewal_target, predicted = rf2.pred)
rf1.cm 
rf2.eval <- compute.eval.metrics(rf2.cm)
rf2.eval
print(rf2) #OOB = 33,4
rf2.pred.prob <- predict(object = rf2, newdata = test,type="prob")
rf2.roc <-roc(response = as.numeric(test$renewal_target),
              predictor = rf2.pred.prob[,2] ,  
              levels = c(1, 2))

rf2.auc <- rf2.roc$auc 
rf2.auc  #0,693


# Variable Importance
varplot<-varImpPlot(rf2,
                    sort = T,
                    n.var = 10,
                    main = "Top 10 - Variable Importance")
varplot
importance(rf2)

