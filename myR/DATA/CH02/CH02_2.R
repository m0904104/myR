#量表之試題分析 2017/07/30
#檔名CH02_2.R 資料檔CH02_2.csv
#設定工作目錄
setwd("D:/DATA/CH02/")
#讀取資料檔
library(readr)
sdata0 <- read_csv("CH02_2.csv")
#檢視前後六筆資料
head(sdata0)
tail(sdata0)
#計算題數與人數
pnum <- ncol(sdata0)-1  #題數
snum <- nrow(sdata0)  #人數
#檢視題數以及人數
print(pnum)
print(snum)
#去除第一欄編號欄位
sdata1 <- sdata0[,-1]
head(sdata1)
#定義my_stats函數，平均數、標準差、偏態與峰度
library(moments)
my_stats <- function(x) {
  funs <- c(mean, sd, skewness, kurtosis)
  sapply(funs, function(f) f(x, na.rm = TRUE))
}

#計算平均數、標準差、偏態以及峰度
sdata1_desc <- apply(sdata1, 2, my_stats)
print(sdata1_desc)
rownames(sdata1_desc) <- c("平均數", "標準差", "偏態", "峰度")
result1 <- as.data.frame(t(sdata1_desc))
row.names(result1) <- names(sdata0[,2:(pnum+1)])
head(round(result1,3))

#計算分組難度、鑑別度以及CR值
sdata1$tot <- apply(sdata1, 1, sum)
sdata1$grp <- NA
LB=0
HB=0
LB=quantile(sdata1$tot,probs=c(0.27))
HB=quantile(sdata1$tot,probs=c(0.73))
print(LB)
print(HB)
sdata1$grp[sdata1$tot <= LB] <- "L"
sdata1$grp[sdata1$tot >= HB] <- "H"
sdata1$grp <- factor(sdata1$grp)
sdata2 <- aggregate(sdata1[,1:pnum], by=list(sdata1$grp), mean)
sdata2 <- t(sdata2[,-1])
item_t <- sapply(sdata1[,1:pnum], function(x) c(t.test(x ~ sdata1$grp, var.equal=TRUE)$statistic,t.test(x ~ sdata1$grp, var.equal=TRUE)$p.value))
result2 <- data.frame(Item=rownames(sdata2),m.l=sdata2[,2], m.h=sdata2[,1], m.a=(sdata2[,2]+sdata2[,1])/2, m.dif=sdata2[,1]-sdata2[,2], t.stat=item_t[1,], t.p=item_t[2,])
head(result2)
result2 <- result2[,-1]
names(result2) <- c('低分組平均','高分組平均','差異', '鑑別度','CR值','p值')
row.names(result2) <- names(sdata0[,2:(pnum+1)])
head(round(result2,3))

#利用psychometrics套件，計算題目與總分相關
library(psychometric)
itotr <- item.exam(sdata1[, 1:pnum], discrim = TRUE)
head(itotr)

#結合分析結果
result3 <- t(rbind(itotr$Item.Tot.woi))
result3 <- data.frame(result3)
names(result3) <- c('題目總分相關')
row.names(result3) <- names(sdata0[,2:(pnum+1)])
head(round(result3,3))

#計算刪題後信度
detach("package:psychometric", unload = TRUE)

#利用psych套件，計算刪題後信度
library(psych)
itotalpha <- alpha(sdata1[, 1:pnum],check.keys=TRUE)$alpha.drop[,'raw_alpha']
result4 <- as.data.frame(t(rbind(itotalpha, itotr$Item.Rel.woi)))
names(result4) <- c('總量表信度(刪題)','題目信度')
row.names(result4) <- names(sdata0[,2:(pnum+1)])
head(round(result4, 3))

#合併刪題後信度
resultall <- cbind(result2, result3, result4)
round(resultall, 3)

#將結果存至檔案
write.csv(round(resultall,3), "多元試題分析結果.csv")