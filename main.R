source("~/wind/sourceFile.R")
source("~/wind/targetGen.R")
source("~/wind/featureGen.R")

# Signal Update
theurl <- "http://db2015.wstock.cn/wsDB_API/kline.php?r_type=2&symbol=SFIF0001&desc=1&return_t=3&qt_type=15&q_type=0&desc=1&fq=2&num=1&u=test&p=8e6a"
dataInsert<-jsonlite::fromJSON(content(GET(theurl), "text"))
### Insert signal generation here

