source("~/wind/sourceFiles/sourceFile.R")
library(pROC)
library(randomForest)
library(caret)

##################################################################################################
#################### caret train Data
trainx<-copy(dataModel[1:(floor(nrow(dataModel)*0.75)), ])
trainx[, c("tDate","tHour","tMin","target"):=NULL]
trainy<-copy(dataModel[1:(floor(nrow(dataModel)*0.75)), "target", with=F])


testx<-copy(dataModel[(floor(nrow(dataModel)*0.75)):nrow(dataModel),])
testx[, c("tDate","tHour","tMin","target"):=NULL]
testy<-copy(dataModel[(floor(nrow(dataModel)*0.75)):nrow(dataModel), "target", with=F])

#################### caret train
trainCaret<-copy(dataModel[1:(floor(nrow(dataModel)*0.75)), ])
trainCaret[, c("tDate","tHour","tMin"):=NULL]
trainCaret<-trainCaret[!is.na(target),]
trainCaret[, target:=ifelse(target=="down","flat", target)]

testCaret<-copy(dataModel[floor(nrow(dataModel)*0.75):nrow(dataModel), ])
testCaret<-testCaret[!is.na(target),]
testCaret[, target:=ifelse(target=="down","flat", target)]


##################################################################################################
##################################################################################################
# Up Model
rfUp <- train(target~., data=trainCaret, method="rf", tuneGrid=expand.grid(.mtry=seq(5, 20, 3)), metric="ROC", 
               trControl=trainControl(method="repeatedcv", number=3, repeats=1, classProbs=T, summaryFunction=twoClassSummary), do.trace=T)


testCaret$predict<-predict(rfFitUp, testCaret)


auc(testCaret$target, testCaret$target)

# Down Model
