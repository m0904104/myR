#量表之測驗分析 2017/07/30 有分量表
#檔名CH03_1.R 資料檔CH03_1.csv
#設定工作目錄
setwd("D:/DATA/CH03/")
#讀取資料檔
library(readr)
sdata0 <- read_csv("CH03_1.csv")
#檢視前後六筆資料
head(sdata0)
tail(sdata0)
#計算題數與人數
pnum <- ncol(sdata0)-1  
snum <- nrow(sdata0)
#檢視題數以及人數
pnum
snum
#讀取資料,去除編號欄位
sdata1 <- sdata0[,-1]
head(sdata1)
#計算信度
library(psych)
alpha(sdata1)
alpha(sdata1)$total$raw_alpha
#假如有分量表可以利用list一次計算
subdata <- list(x0=sdata1, x1=sdata1[,1:6], x2=sdata1[,7:12], x3=sdata1[,13:18], x4=sdata1[,19:24])
#計算總量表及分量表信度
palpha <- lapply(subdata, alpha)
#總量表信度
palpha$x0$total$raw_alpha
#分量表1信度
palpha$x1$total$raw_alpha
#分量表2信度
palpha$x2$total$raw_alpha
#分量表3信度
palpha$x3$total$raw_alpha
#分量表4信度
palpha$x4$total$raw_alpha