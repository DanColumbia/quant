backT <- function(df, bigT){
  ts <- data.table(t=c(1:length(df)), price=df)
  ts[, matched:=0]
  ts[, matchWindow:=0]
  for(i in 1:(nrow(ts)-1)){
    shifted <- shift(ts$price, i, type="lead")
    ts$matchWindow <- ifelse(!(ts$matched==0 & abs(ts$price-shifted)<=1) | is.na(ts$price-shifted), ts$matchWindow, i)
    ts$matched <- ifelse(!(ts$matched==0 & abs(ts$price-shifted)<=1) | is.na(ts$price-shifted), ts$matched, 1)
  }
  
  # ts[, tBack:=rollapply(price, bigT, mean, fill=NA, align="right")]
  # stability <- sd(ts$tVar, na.rm=T)
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

