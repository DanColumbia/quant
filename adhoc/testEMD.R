ndata <- 3000
tt2k <- seq(0, 9, length=ndata)
xt2k <- sin(pi * tt2k) + sin(2* pi * tt2k) + sin(6 * pi * tt2k)  + 0.5 * tt2k
tryk <- emd(xt2k, tt2k, boundary="wave")


tabdata <- read.csv(paste0(windbox, "tab6.csv"))
tabdata <- data.table(tabdata)

tabdata<-copy(IF)
setnames(tabdata, "date_trade", "dtime")
tabdata[, dt:=as.POSIXct(dtime)]
tabdata[, dt:=as.Date(dt)]


tabdata[, sanshifenzhong:=paste0(dt,"-",hour(dtime),"-",floor(minute(dtime)/30))]
head(tabdata)


setnames(tabdata, c("dt","sanshifenzhong"), c("olddt","dt"))

result <- data.frame()


for(i in unique(tabdata$dt)){
  print(i)
  tempDF <- tabdata[dt==i, ]
  if(nrow(tempDF)<10)
    next
  tempEMD <- emd(tempDF$priceDiff, max.imf=4, boundary="wave")
  tempCalc <- data.table(cbind(tempDF$priceDiff, tempEMD$residue))
  maxTick <- nrow(tempCalc)
  decisionTick <- maxTick * 0.15
  tempCalc[, diff:=V1-V2]
  tempCalc[, RtradeUp:=runSD(diff, n=1, cumulative = T)]
  tempCalc[, RtradeDown:=runSD(V2, n=1, cumulative = T)]
  tempCalc[, R:=log(RtradeUp/RtradeDown)]
  tempCalc[, Rmean:=runMean(R, n=1, cumulative = T)]
  tempCalc[, strongSignal:=ifelse(R<Rmean, 1, 0)]
  open <- ifelse(tempCalc[decisionTick, "strongSignal", with=F]==1, 1, 0)
  direction <- ifelse(tempCalc[decisionTick, "V1", with=F] > tempCalc[1, "V1", with=F], 1, 
                      ifelse(tempCalc[decisionTick, "V1", with=F] < tempCalc[1, "V1", with=F], -1, 0))
  profit <- tempCalc[maxTick, "V1", with=F] - tempCalc[decisionTick, "V1", with=F]
  tempResult <- as.data.frame(c(i, open, direction, profit))
  names(tempResult) <- c("time","openDecision","openDirection","profit")
  result <- rbind(result, tempResult)
}

result <- data.table(result)
names(result) <- c("time", "openDecision", "openDirection", "profit")
result[, dailyProfit:=openDecision*openDirection*profit]
result[, PnL:=runSum(dailyProfit, n=1, cumulative=T)]
plot(result$PnL)


