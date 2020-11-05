setwd("C:/myR1027/Youbike")
install.packages('jsonlite')
library(jsonlite)
file <- "https://quality.data.gov.tw/dq_download_json.php?nid=123026&md5_url=4d8de527a0bcd8a7b1aeae91120f021d"
download.file(file,"ubikeNTP.json")
jdata <- fromJSON("ubikeNTP.json")
str(jdata)
jdata$sarea
x=table(jdata$sarea)
y=sort(x,decreasing = TRUE)
z=as.data.frame(y)
summary(z$Fr)
boxplot(z$Freq)

install.packages('data.table')
library(data.table)
file <- "https://tcgbusfs.blob.core.windows.net/blobyoubike/YouBikeTP.json"
download.file(file,"ubikeTP.json")
m <- fromJSON("ubikeTP.json")
n <- rbindlist(m$retVal)