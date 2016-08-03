# 数据要求有
# tTimesequence, buy, sell, buySize, sellSize

# 伪造数据
# zz500 <- data.table(zz500)
# 
# k1 <- ceiling(runif(2864)*5)/100
# zz500[, buy:=price-k1]
# k2 <- ceiling(runif(2864)*5)/100
# zz500[, sell:=price+k2]
# 
# k3 <- ceiling(runif(2864)*100)*100
# k4 <- ceiling(runif(2864)*100)*100
# 
# zz500[, buySize:=k3]
# zz500[, sellSize:=k4]
# 
# 
# 

zz500 <- read.csv(paste0(windbox, "SH600125.csv"), header=F)

colnames(zz500)<-c("sym","dtime","trade_price","trade_vol","trade_amount","val_sp1","val_sp2","val_sp3","val_sp4","val_sp5","val_sv1","val_sv2","val_sv3","val_sv4","val_sv5","val_bp1","val_bp2","val_bp3","val_bp4","val_bp5","val_bv1","val_bv2","val_bv3","val_bv4","val_bv5","flg_buy","sc_vol","sc_amount","val_OI")

zz500 <- data.table(zz500)

zz500 <- zz500[trade_price>0, ]

setnames(zz500, c("val_sp1", "val_sv1", "val_bp1", "val_bv1"), c("sell", "sellSize", "buy", "buySize"))

zz500[, date:=as.Date(dtime)]

oneday <- zz500[date=='2015-08-10',]

DT <- oneday[, c("dtime", "sell", "sellSize", "buy", "buySize"), with=F]
DT[, tTimeSequence:=seq(1:nrow(DT))]

###########################################################
# 函数处理数据
###########################################################

# 聚类

performClustering <- function(DT, P_min){
  # 计算S1, S2, S3
  DT[, S1:=buySize/100]
  DT[, S2:=sellSize/100]
  DT[, S3:=ceiling(sell*100-buy*100)]

  # 把S1, S2, S3做成一个hash值
  # rangeS2 <- 10 ^ (nchar(ceiling(max(DT$S2) - min(DT$S2))) + 1)
  # rangeS3 <- 10 ^ (nchar(ceiling(max(DT$S3) - min(DT$S3))) + 1)

  # DT[, hashState:=as.character(S1*rangeS2*rangeS3 + S2*rangeS3 + S3)]
  DT[, hashState:=paste0("B", S1, "S", S2, "D", S3)]
  
  # 算每个state的出现频次, 以下每个state均以上一步计算的hash值代表
  stateList <- DT[, .("Occurrence"=.N, 
                      "S1_Centroid"=mean(S1), 
                      "S2_Centroid"=mean(S2), 
                      "S3_Centroid"=mean(S3)), 
                  by="hashState"]
  stateList[, Probability:=Occurrence/nrow(DT)]
  stateList[, hashCluster:=hashState]
  

  clusterList <- stateList[, c("hashCluster","S1_Centroid","S2_Centroid","S3_Centroid","Probability","Occurrence"), with=F]
  
  minProbability <- min(clusterList$Probability)
  while(minProbability < P_min){
    
    updateCluster <- clusterList[Probability == minProbability & !duplicated(Probability), ]

    # 拿这个cluster去和所有的其他cluster比较
    compareVector <- updateCluster[, c("S1_Centroid","S2_Centroid","S3_Centroid"), with=F]
    compareList <- clusterList[hashCluster!=updateCluster$hashCluster, ]
    compareList[, c("compareS1", "compareS2", "compareS3"):=compareVector]
    compareList[, distance:= (S1_Centroid-compareS1)^2 + (S2_Centroid-compareS2)^2 + (S3_Centroid-compareS3)^2]
    
    # 把与之相距最短的那个cluster作为这个state的新cluster, 并
    # 1. 在stateList中更新所有当前cluster的hashCluster为新的cluster
    # 2. 更新本cluster内所有state的centroid和probability
    minDist <- min(compareList$distance)
    
    # 1.
    clusterHash <- compareList[distance==minDist & !duplicated(distance), ]$hashCluster
    clusterList[hashCluster==updateCluster$hashCluster, hashCluster:=clusterHash]
    stateList[hashCluster==updateCluster$hashCluster, hashCluster:=clusterHash]

    # 2.
    clusterList <- clusterList[, .("S1_Centroid"=weighted.mean(S1_Centroid, Occurrence), 
                                   "S2_Centroid"=weighted.mean(S2_Centroid, Occurrence), 
                                   "S3_Centroid"=weighted.mean(S3_Centroid, Occurrence),
                                   "Probability"=sum(Probability),
                                   "Occurrence"=sum(Occurrence)), 
                               by="hashCluster"]
    # 更新min probability
    minProbability <- min(clusterList$Probability)
  }

  return(as.list(clusterList, stateList))
  
}

updateClustering <- function(newData, clusterList, stateList){
    # 计算S1, S2, S3
    newData[, S1:=buySize/100]
    newData[, S2:=sellSize/100]
    newData[, S3:=ceiling(sell*100-buy*100)]
    
    # 把S1, S2, S3做成一个hash值
    newData[, hashState:=paste0("B", S1, "S", S2, "D", S3)]

    # 检测是否已存在于现有的stateList
    notInCurrentStateList <- ifelse(newData$hashState %in% stateList$hashState, FALSE, TRUE)
    
    if(notInCurrentStateList){
      # 拿这个newData去和所有的其他cluster比较
      compareVector_update <- newData[, c("S1","S2","S3"), with=F]
      compareList_update <- clusterList
      compareList_update[, c("compareS1", "compareS2", "compareS3"):=compareVector_update]
      compareList_update[, distance:= (S1_Centroid-compareS1)^2 + (S2_Centroid-compareS2)^2 + (S3_Centroid-compareS3)^2]
      
      
      # 把与之相距最短的那个cluster作为这个state的新cluster, 并
      # 1. 在stateList中加入当前newData
      # 2. 更新newData并入的cluster的所有state的centroid和probability
      minDist <- min(compareList_update$distance)
      
      # 1.
      clusterHash <- compareList_update[distance==minDist & !duplicated(distance), ]$hashCluster
      
        # 先使得newData满足stateList的格式
      newData[, Occurrence:=1]
      newData[, Probability:=1]
      newData[, hashCluster:=clusterHash]
      setnames(newData, c("S1","S2","S3"), c("S1_Centroid","S2_Centroid","S3_Centroid"))
      stateList <- rbind(stateList, newData[, c("hashState", "Occurrence", "S1_Centroid", "S2_Centroid", "S3_Centroid", "Probability", "hashCluster"), with=F])

      # 2.
      clusterList[hashCluster==clusterHash, S1_Centroid:= (S1_Centroid * Occurrence + newData$S1_Centroid)/(Occurrence + 1)]
      clusterList[hashCluster==clusterHash, S2_Centroid:= (S2_Centroid * Occurrence + newData$S2_Centroid)/(Occurrence + 1)]
      clusterList[hashCluster==clusterHash, S3_Centroid:= (S3_Centroid * Occurrence + newData$S3_Centroid)/(Occurrence + 1)]
      clusterList[hashCluster==clusterHash, Occurrence:=Occurrence+1]
    }
    return(as.list(clusterList, stateList))
}


# 分类估计价格变动可能性及期望值
featureEstimating <- function(DT, stateList, clusterList, delta_predict){
  DT[, midPrice:=(buy+sell)/2]
  priceChange <- DT[, c("tTimeSequence", "midPrice", "hashState"), with=F] 
  priceChange[, tTimeSequence:=tTimeSequence + delta_predict]
  setnames(priceChange, "midPrice", "midPricePast")
  priceChange <- merge(priceChange, DT[, c("tTimeSequence", "midPrice"), with=F], by="tTimeSequence")
  priceChange[, midPriceChange:=midPrice - midPricePast]
  priceChange <- merge(priceChange, stateList[, c("hashState","hashCluster"), with=F], 
                       by="hashState")

  # 1. 在各个cluster内有价格变动的条件概率
  P_DS <- priceChange[, .(nzChangeSum = sum(ifelse(midPriceChange != 0, 1, 0)), 
                          inCluster = .N), 
                      by="hashCluster"]
  
  # 2. 在各个cluster估计条件期望值
  E_DS <- priceChange[, .(changeMean = mean(midPriceChange)), 
                      by="hashCluster"]
  
  # 3. 整体期望
  # 不用cluster，直接从DT算就行了
  E_D <- mean(priceChange$midPriceChange)
  
  return(as.list(P_DS, E_DS, E_D))
  
}


# 根据预测的方向产生交易建议











###########################################################
# 主流程
###########################################################

HFT <- function(DT, P_min, delta_predict, theta, pi){
  
}




