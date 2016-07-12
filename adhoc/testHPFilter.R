IF00 <- read.csv(paste0(windbox, "PriceDiff/IF00_201501.csv"))
IF02 <- read.csv(paste0(windbox, "PriceDiff/IF02_201501.csv"))
IF00 <- data.table(IF00)
IF02 <- data.table(IF02)

head(IF00)
head(IF02)

setnames(IF02, "val_price", "val_price02")

IF <- merge(IF00, IF02[,c("date_trade","val_price02"), with=F], by="date_trade")

IF[, date_trade:=as.character(date_trade)]

IF[, dtTrade:=as.Date(date_trade)]

max(IF$date_trade)

IF[, priceDiff:=val_price02-val_price]

writeMat(con=paste0(windbox, "PriceDiff/IF.mat"), y=IF)

plot(IF$priceDiff)

IF <- IF[priceDiff!=0,]
for(i in unique(IF$dtTrade)){
  DT <- IF[dtTrade==i,]
  
}