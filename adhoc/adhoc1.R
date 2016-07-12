varT <- function(df, bigT){
  ts <- data.table(t=c(1:length(df)), price=df)
  ts[, tVar:=rollapply(price, bigT, mean, fill=NA, align="right")]
  stability <- sd(ts$tVar, na.rm=T)
  return(stability)
  # return(ts)
}


bestT <- function(df){
  allT <- data.frame()
  for(i in 2:30){
    allT <- rbind(allT, c(i, varT(df, i)))
  }
  names(allT) <- c("T", "Stability")
  # smallestVar <- min(allT$Stability)
  # allT<-data.table(allT)
  # result<-allT[Stability==smallestVar, "T", with=F]
  return(allT)
}


dfcsv <- read.csv(paste0(wind, "IF1603.csv"))
dfcsv <- dfcsv[133:225,]
df<-dfcsv$CLOSE

