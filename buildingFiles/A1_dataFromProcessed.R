
fileNames<-list.files(paste0(windbox, "Stock15/"))
length(fileNames)
# 5665
for(i in 0:56){
  print(i)
  assign(paste0("data",i), data.frame())
  for(j in 1:100){
    file<-fileNames[i*100+j]
    print(file)
    newData<-read.csv(paste0(windbox, "Stock15/", file))
    ticker<-substr(file, 1, 8)
    newData$Ticker <- ticker
    assign(paste0("data",i),rbind(get(paste0("data",i)), newData))
  }
}

data<-rbind(data0, data1)
data2<-rbind(data50,data51)
for(i in 52:56){
  data2<-rbind(data2, get(paste0("data",i)))
}

remove(data41)

saveRDS(data, paste0(windbox, "data.RDS"))
saveRDS(data2, paste0(windbox, "data2.RDS"))

allData<-rbind(data, data2)

saveRDS(allData, paste0(windbox, "allData.RDS"))

data<-data.table(allData)

remove(allData)


dataStock<-data[substr(Ticker, 1,3) %in% c("SH6","SZ0"),]
dataOther<-data[!(Ticker %in% dataStock$Ticker), ]
remove(data)
saveRDS(dataOther, paste0(windbox, "dataNotStocks.RDS"))
remove(dataOther)
saveRDS(dataStock, paste0(windbox, "stockData.RDS"))
