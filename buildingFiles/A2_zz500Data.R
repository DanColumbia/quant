zz500 <- read.csv(paste0(windbox, "zz500/201602/000905_20160203.csv"))
zz500 <- data.table(zz500)
zz500 <- zz500[, 1:6, with=F]
names(zz500) <- c("tDate","tTime","price","volume","cumVolume","er")

zz500[, tTimeSequence:=1:nrow(zz500)]
