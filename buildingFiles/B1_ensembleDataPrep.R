source("~/R/sourceFile.R")

dataModel <- copy(data)
# dataModel[, c("Symbol","Name","Date","time"):=NULL]

########### Model data preparation
# target
# target<-holdTargetGen(dataModel, 2, 1, 0.01)
# target<-target$PnLNextdayClass

# features
factor_accDist<-AccumulationDistributionGen(dataModel, 3)
factor_asi<-ASIGen(dataModel, 0.005)
factor_macd<-MACDGen(dataModel)
factor_rsi<-RSIGen(dataModel)
factor_bband<-BBandGen(dataModel)

dataModel<-cbind(dataModel, factor_accDist)
dataModel<-cbind(dataModel, factor_asi)
dataModel<-cbind(dataModel, factor_macd)
dataModel<-cbind(dataModel, factor_rsi)
dataModel<-cbind(dataModel, factor_bband)

dataModel<-cbind(dataModel, target)

########## Feature Analysis
featureAnal<-featureAnalysis(dataModel, "")

dataModel<-dataModel[!is.na(MACDPriceIndicator),]
dataModel<-dataModel[!is.na(target),]



