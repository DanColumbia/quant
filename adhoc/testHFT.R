# 数据要求有
# tTimesequence, bid, ask, bidSize, askSize

# 伪造数据
zz500 <- data.table(zz500)

k1 <- ceiling(runif(2864)*5)/100
zz500[, bid:=price-k1]
k2 <- ceiling(runif(2864)*5)/100
zz500[, ask:=price+k2]

k3 <- ceiling(runif(2864)*100)*100
k4 <- ceiling(runif(2864)*100)*100

zz500[, bidSize:=k3]
zz500[, askSize:=k4]



DT <- copy(zz500)

###########################################################
# 函数处理数据
###########################################################

# 聚类

performClustering <- function(DT, P_min){
  # 计算S1, S2, S3
  DT[, S1:=bidSize/100]
  DT[, S2:=askSize/100]
  DT[, S3:=ceiling(ask*100-bid*100)]

  # 把S1, S2, S3做成一个hash值
  rangeS2 <- 10 ^ (nchar(ceiling(max(DT$S2) - min(DT$S2))) + 1)
  rangeS3 <- 10 ^ (nchar(ceiling(max(DT$S3) - min(DT$S3))) + 1)

  DT[, hashState:=S1*rangeS2*rangeS3 + S2*rangeS3 + S3]
  
  # 算每个state的出现频次, 以下每个state均以上一步计算的hash值代表
  stateList <- DT[, .("Occurrence"=.N, 
                      "S1_Centroid"=mean(S1), 
                      "S2_Centroid"=mean(S2), 
                      "S3_Centroid"=mean(S3)), 
                  by="hashState"]
  stateList[, Probability:=Occurrence/nrow(DT)]
  stateList[, hashCluster:=hashState]
  

  clusterList <- stateList[, c("hashCluster","S1_Centroid","S2_Centroid","S3_Centroid","Probability"), with=F]
  
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
    clusterList <- clusterList[, .("S1_Centroid"=mean(S1_Centroid), 
                                   "S2_Centroid"=mean(S2_Centroid), 
                                   "S3_Centroid"=mean(S3_Centroid),
                                   "Probability"=sum(Probability)), 
                               by="hashCluster"]
    # 更新min probability
    minProbability <- min(clusterList$Probability)
  }

  return(as.list(clusterList, stateList))
  
}

updateCluster <- function(newData, clusterList){}


# 分类估计价格变动可能性及期望值
featureEstimating <- function(DT, stateList, clusterList, delta_predict){
  DT[, midPrice:=(bid+ask)/2]
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
  
  
}


# 根据预测的方向产生交易建议


###########################################################
# 主流程
###########################################################

HFT <- function(DT, P_min, delta_predict, theta, pi){
  
}




