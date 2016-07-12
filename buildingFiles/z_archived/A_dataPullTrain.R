################################# Initial Training
# now<-as.character(as.Date(Sys.time()))
tickInterval<-15

theurl <- paste0("http://db2015.wstock.cn/wsDB_API/kline.php?r_type=2&symbol=SFIF0001&desc=1&return_t=3&qt_type=15&q_type=0&fq=1&num=99999&stime=2000-01-01&etime=",
                 "2015-04-02","&u=test&p=8e6a")
stock<-GET(theurl)
dataAppend<-jsonlite::fromJSON(content(stock, "text"))

dataAppend<-data.table(dataAppend)
setnames(dataAppend, c("Open","Low","High","Close","Volume","Amount"), c("open","low","high","close","volume","amount"))
dataAppend<-dataAppend[!is.na(open),]

# general processing
dataAppend[, time:=as.character(Date)]
dataAppend[, tDate:=as.Date(time)]
dataAppend[, tHour:=as.numeric(substr(time, 12, 13))]
dataAppend[, tMin:=as.numeric(substr(time, 15, 16))]
dataAppend[, flgAfternoon:=ifelse(tHour<12, 0, 1)]
dataAppend[, tTimeSequence:=((tHour-9)*60+(tMin-15)-flgAfternoon*90)/15]

min(dataAppend$Date)
max(dataAppend$Date)
min(data$Date)
data <- rbind(data,dataAppend)


data[, volume:=as.numeric(volume)]
data[, amount:=as.numeric(amount)]

data<-data[ order(tDate, tHour, tMin), ]

saveRDS(data, "E:/BaiduDropbox/wind/data.RDS")

############################## Retrain
data <- readRDS("E:/BaiduDropbox/wind/data.RDS")


# 李博的数据
# libodata<-readMat("E:/BaiduDropbox/wind/intraday_15m_zz500_updated.mat")
