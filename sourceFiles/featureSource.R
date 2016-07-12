# 生成用于signals generation的features和人工hard rules的functions

## General comments and notes

# Calculate features (technical indicators based on past prices or other quantified human information) which are
# then used to predict target calculated elsewhere, i.e. generate trading signals

# Adaptability: Remove by=() clauses, change mins=15 to days=10 (10 trading days) to adapt the codes to daily level


####################################################################################################################


# Accumulation/Distribution
# Note: was used to identify divergence of price and volume, e.g. accumulation/distribution is up
# and price is down, price will follow accumulation/distribution

AccumulationDistributionGen <- function(dataProc, lookBack=3) {
  result<-copy(dataProc)
  result[, tickValue:=(close*2-low-high)/(high-low+0.0000000001)*volume]
  # result[, AccDis:=rollapply(tickValue, lookBack, sum, fill=NA, align="right"), by=c("tDate","flgAfternoon")]
  result[, accDis:=rollapply(tickValue, lookBack, sum, fill=NA, align="right")]
  result[, accDisMom:=momentum(accDis)]
  result[, priceMom:=momentum(close)]
  result[, accDisDiverge:=ifelse(sign(accDisMom)==-1*sign(priceMom),0,1)]
  result[, accDisMomSign:=sign(accDisMom)]
  return(result[, c("accDisMom","accDisMomSign","accDisDiverge"), with=F])
} 



# Accumulating Swing Index (through the calculation of Swing Index)
# Note: was used to bring ohlc back to its true price, then breakthroughs can be identified accurately

ASIGen <- function(dataProc, pctgMoveBtwTicks=0.005) {
  # dataProc<-copy(data)
  dataProcOffset <- dataProc[, c("tDate", "tTimeSequence", "open", "high", "low", "close"), with=F]
  setnames(dataProcOffset, c("open", "high", "low", "close"), c("openPrev", "highPrev", "lowPrev", "closePrev"))
  dataProcOffset[, tTimeSequence:=tTimeSequence + 1]
  dataMaster <- merge(dataProc, dataProcOffset, by=c("tDate", "tTimeSequence"), all.x=T)
  dataMaster[, K:=abs(pmax(high-closePrev, low-closePrev))]
  dataMaster[, R:=abs(ifelse(high-closePrev >= pmax(low-closePrev, high-low), 1, 0)* (high-closePrev-0.5*(low-closePrev)+0.25*(closePrev-openPrev)) +
               ifelse(low-closePrev >= pmax(high-closePrev, high-low), 1, 0)* (low-closePrev-0.5*(high-closePrev)+0.25*(closePrev-openPrev)) +
               ifelse(high-low >= pmax(high-closePrev, low-closePrev), 1, 0)* (high-low+0.25*(closePrev-openPrev)))+0.0000000001]
  dataMaster[, L:=closePrev*pctgMoveBtwTicks+0.0000000001]
  dataMaster[, SI:=50*(close-closePrev+0.5*(close-open)+0.25*(closePrev-openPrev))/R*(K/L)]
  # dataMaster[, ASI:=cumsum(SI), by=c("tDate", "flgAfternoon")]
  dataMaster[is.na(SI), "SI"]<-0
  dataMaster[, ASI:=cumsum(SI)]
  return(dataMaster[, "ASI", with=F])
}

# MACD, from TTR package

MACDGen <- function(dataProc, nFastParam=12, nSlowParam=26, nSigParam=9, maTypeParam="EMA", percentParam=TRUE) {
  resultPrice<-data.table(MACD(dataProc[,c("close"),with=F], nFast=nFastParam, nSlow=nSlowParam, nSig=nSigParam, maType=maTypeParam, percent=percentParam))
  resultPrice[, MACDPriceIndicator:=ifelse(macd>signal,1,0)]
  # resultPrice[, MACDPriceIndicator:=as.factor(MACDPriceIndicator)]
  resultVolume<-data.table(MACD(dataProc[,c("volume"),with=F], nFast=nFastParam, nSlow=nSlowParam, nSig=nSigParam, maType=maTypeParam, percent=percentParam))
  resultVolume[, MACDVolumeIndicator:=ifelse(macd>signal,1,0)]
  # resultVolume[, MACDVolumeIndicator:=as.factor(MACDVolumeIndicator)]
  result<-data.table(cbind(resultPrice$MACDPriceIndicator, resultVolume$MACDVolumeIndicator))
  setnames(result, c("V1","V2"), c("MACDPriceIndicator","MACDVolumeIndicator"))
  return(result)
}



# RSI, from TTR package, can be 9, 14, 25
RSIGen <- function(dataProc, nParam=14, maTypeParam="EMA") {
  result <- data.table(RSI(dataProc[, c("close"), with=F], n=nParam, maType = maTypeParam))
  setnames(result, c("V1"), c("RSI"))
  result[, RSILevel:=ifelse(RSI>=70, 1, ifelse(RSI<=30, -1, 0))]
  return(result)
}


# Bollinger Bands, from TTR package, 20 recommended by Bollinger
BBandGen <- function(dataProc, nParam=20, maTypeParam="EMA") {
  result <- data.table(BBands(dataProc[, c("high","low","close"), with=F], n=nParam, maType = maTypeParam))
  setnames(result, c("dn","mavg","up","pctB"), c("BBdn","BBmavg","BBup","BBpctB"))
  return(result)
}


# ROC, Momentum, from TTR package
# ROC(x, n = 1, type = c("continuous", "discrete"), na.pad = TRUE)
# 
# momentum(x, n = 1, na.pad = TRUE)


# ADX, Welles Wilder's Directional Movement Index, from TTR package
# ADX(HLC, n = 14, maType, ...)


# Stochastic Oscillator / Stochastic Momentum Index, from TTR package
# stoch(HLC, nFastK = 14, nFastD = 3, nSlowD = 3, maType, bounded = TRUE,
#       smooth = 1, ...)
# 
# SMI(HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9, maType,
#     bounded = TRUE, ...)






## Advancing issues, declining issues related (not readily available)
# Absolute Breadth Index (ABI)

# abs(#advancing stocks - #declining stocks)

# A-D Issues
# #advancing - #declining
# A/D line
# running total of A-D Issues
# A/D Ratio
# #advancing / #declining

# Advancing, Declining, Unchanged Volume
# Total volume of advancing/declining issues. 
# example: 3-day MA of advancing volume is down, while price is up, then price would correct down
######



# Andrews' Pitchfork
# AndrewsPitchforkGen <- function(dataProc, lookBack=30) {
#   dataProcAP <- 
# }






########################################################################################
########################################################################################
# 一些复杂的人工hard rules. 是加在model signal中的其他非模型训练得到的买卖规则
# 比如是否是早晚时段







