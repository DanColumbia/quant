# 1. Produce final buy/sell signals
# Combine ensembled signals and hard rules list from featuresSource to form a final buy/sell signal




# 2. Back test PnL using the above signal. Or the signal can come from singal factors
# Will add conditional position management paramters LATER
backTest <- function(priceSeries, signalSeries, orderSizeSeries){
  if(length(priceSeries)!=length(signalSeries) | length(priceSeries)!=length(orderSizeSeries))
    return("Series not in same length!")
  
  tradeLog<-data.frame()
  tradeLog<-rbind(tradeLog, c(0,0,0,0))
  names(tradeLog)<-c("TimeSpot","Action","OrderSize","Avg.Price")
  
  positionLast<-data.frame()
  positionLast<-rbind(positionLast, c(0,0,0))
  names(positionLast)<-c("Position","Avg.Price","Float.Profit")
  
  for(i in 1:1000){
    print(i)
    if(signalSeries[i]!=0 & !is.na(signalSeries[i])){
      newTrade<-c(i, signalSeries[i], orderSizeSeries[i], priceSeries[i])
      tradeLog<-rbind(tradeLog, newTrade)
      
      positionLast$Avg.Price<- (positionLast$Avg.Price * positionLast$Position + newTrade[4]*newTrade[3]*newTrade[2])/(positionLast$Position + newTrade[3]*newTrade[2])
      positionLast$Position <- positionLast$Position + newTrade[3]*newTrade[2]
      positionLast$Float.Profit<-positionLast$Position*(priceSeries[i]-positionLast$Avg.Price)
    }
    print(positionLast$Position*(priceSeries[i]-positionLast$Avg.Price))
  }
}


#3 PnL curve to statistics
PnL_Stats <- function(PnLCurve){
  n <- length(PnLCurve)
  return <- (PnLCurve[n] - PnLCurve[1])/PnLCurve[1]
  sharpeRatio <- (PnLCurve[n] - PnLCurve[1])/sd(PnLCurve)
  maxDrawdown <- max(runMax(PnLCurve, n=1, cumulative = T) - PnLCurve, na.rm = T)/PnLCurve[1]
  return(as.list(c(return=return, sharpeRatio=sharpeRatio, maxDrawdown=maxDrawdown)))
}

