# 生成用于ensembled signals模型训练的target. 可自定义hold的长度

# 如果在watchwindow内在某一方向达到stopProfit前在另一方向不到stopLoss,则为1;
# 如果在watchwindow内在对面方向首先达到stopLoss,则为0;
# 其余为灰名单,赋值-1
conditionStopPnLTarget <- function(data, watchWindow=10, stopProfit=10, stopLoss=5) {
  data[, maxPrice:=rollapply(price, watchWindow, max, fill=NA, align="left")]
  data[, maxIndex:=rollapply(price, watchWindow, which.max, fill=NA, align="left")]
  data[, maxDiff :=maxPrice-price]
  data[, minPrice:=rollapply(price, watchWindow, min, fill=NA, align="left")]
  data[, minIndex:=rollapply(price, watchWindow, which.min, fill=NA, align="left")]
  data[, minDiff :=price-minPrice]
  
  data[, longTunnel:=ifelse(maxDiff>=stopProfit & !(minDiff>=stopLoss & minIndex<maxIndex), 1, 
                            ifelse(minDiff>=stopLoss & !(maxDiff>=stopProfit & maxIndex<minIndex), 0, -1))]
  data[, shortTunnel:=ifelse(minDiff>=stopProfit & !(maxDiff>=stopLoss & maxIndex<minIndex), 1, 
                            ifelse(maxDiff>=stopLoss & !(minDiff>=stopProfit & minIndex<maxIndex), 0, -1))]
  
}



fixedHoldTimeTarget <- function (data, holdWindow=3, executionWindow=1, threshold=0.01) {
  dataOffset <- data[, c("tDate","tTimeSequence","close"), with=F]

  # next tick close execution price
  dataOffsetNextTick <- copy(dataOffset)
  setnames(dataOffsetNextTick, c("close"), c("nexttickExecution"))
  dataOffsetNextTick[, tTimeSequence:=tTimeSequence-1]
  dataMaster <- merge(data, dataOffsetNextTick, by=c("tDate","tTimeSequence"), all.x=T)

  #################################### exit price calculation
  # PnL calculation
  dataMasterOffset <- dataMaster[, c("tDate","tTimeSequence","nexttickExecution"), with=F]
  setnames(dataMasterOffset, c("nexttickExecution"), c("nexttickExecutionHold"))
  dataMasterOffset[, tTimeSequence:=tTimeSequence-holdWindow]
  
  dataMaster <- merge(dataMaster, dataMasterOffset, by=c("tDate","tTimeSequence"), all.x=T)
  dataMaster[, PnLNextday:=((1-tax-commission-liquidityImpact)*nexttickExecutionHold-(1+commission+liquidityImpact)*nexttickExecution)/nexttickExecution]
  
  ################################### profitability classification
  
  dataMaster[, PnLNextdayClass:=ifelse(PnLNextday>threshold, "up", ifelse(PnLNextday < threshold*-1, "down", "flat"))]
  
  return(dataMaster[, c("PnLNextday", "PnLNextdayClass"), with=F])
}