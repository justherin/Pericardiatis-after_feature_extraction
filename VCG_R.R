library(MASS)
 library(randomForest)
library(data.table)
library(plyr)
library(rpart)
library(readxl)


Excel_Data_MI <- read_excel("C:/Users/Herin/Desktop/Excel Data MI.xlsx")
View(Excel_Data_MI)
Excel_Data_MI=Excel_Data_MI[-c(36:39),-c(1,2,3)]
Excel_Data_MI$MI=factor(Excel_Data_MI$MI)
set.seed(123)
rf.coeff2 =randomForest(MI~.,data=Excel_Data_MI, mtry=sqrt(ncol(Excel_Data_MI)),importance =TRUE,ntree=1500,forest=TRUE)
rf.coeff2
importance(rf.coeff2)
varImpPlot(rf.coeff2)
rf.coeff =randomForest(MI~.-Oct6MaxN-Oct5AvgN-Oct6Ratio-Oct4Ratio-Oct3MaxN
                       -Oct6VarN-Oct2Ratio-Oct0MaxN-Oct2VarN-Oct2MaxN,
                       data=Excel_Data_MI, mtry=sqrt(ncol(Excel_Data_MI)),
                      importance =TRUE,ntree=1500,forest=TRUE)
rf.coeff
varImpPlot(rf.coeff)
set.seed(123)

folds <- split(Excel_Data_MI, cut(sample(1:nrow(Excel_Data_MI)),10))
errs <- rep(NA, length(folds))
for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.table)
  train <-ldply(folds[-i], data.table)
  train=train[,-c(1)]
  test=test[,-c(1)]
  rf.coeff =randomForest(train$MI~.-Oct6MaxN-Oct5AvgN-Oct6Ratio-Oct4Ratio-Oct3MaxN
                         -Oct6VarN-Oct2Ratio-Oct0MaxN-Oct2VarN-Oct2MaxN,
                         data=train, mtry=sqrt(ncol(train)),
                         importance =TRUE,ntree=2000,forest=TRUE)
  #rf.coeff2 <- randomForest(train$MI~.,data=train, mtry=sqrt(ncol(train)),importance =TRUE,ntree=1000,forest=TRUE)
  tmp.predict <- predict(rf.coeff, newdata = test, type = "response")
  conf.mat <- table(test$MI, tmp.predict)
  errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
}

print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))

