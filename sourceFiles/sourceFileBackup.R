tax <- 0.001
commission <- 0.0003
liquidityImpact <- 0.002

source("~/R/wind/sourceFiles/featureSource.R")
source("~/R/wind/sourceFiles/targetSource.R")
source("~/R/wind/sourceFiles/backTestSource.R")
source("~/R/wind/sourceFiles/z_generalFunction.R")
source("~/R/wind/sourceFiles/z_woeFunction.R")

windbox <- "E:/BaiduDropbox/wind/"
library(XML)
library(httr)
library(data.table)
library(lubridate)
library(zoo)
library(TTR)
library(EMD)
# library(WindR)
# w.start()

library(pROC)
library(ROCR)
library(PRROC)
library(caret)
options(sqldf.driver="SQLite")

# rf
library(randomForest)
library(varSelRF)

# logistic & elastic net
library(glmnet)
library(Information)
# need min tick/price <=0.001

# remember to sync workspace after work
# file.copy("~/wind/.RData", "E:/BaiduDropbox/wind/.RData",overwrite=T)

