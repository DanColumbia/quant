data <- read.csv(paste0(windbox, "zz500/201602/000905_20160219.csv"))

data <- data.table(data)
data<-data[, 1:6, with=F]
names(data) <- c("date","time","price","volume","aggVolume","amount")

data[, modePrice:=rollapply(price, 270, Mode, fill=NA, align="right")]
data[, stdPrice:=rollapply(price, 270, sd, fill=NA, align="right")]
data[, signal:=ifelse((price-modePrice)/(2*stdPrice)>1,-1,ifelse((price-modePrice)/(2*stdPrice)< -1,1,0))]

