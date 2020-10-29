#二元試題之試題分析 2017/07/29
#檔名CH02_1.R 資料檔CH02_1.csv
#設定工作目錄
setwd("C:/myR/DATA/CH02/")
#讀取資料檔
library(readr)
sdata0 <- read_csv("CH02_1.csv")
#檢視前後六筆資料
head(sdata0)
tail(sdata0)
str(sdata0)
#因為第一行是答案，所以要計算時需減1
pnum <- ncol(sdata0)-1  #題數
snum <- nrow(sdata0)-1  #人數
#檢視題數以及人數
print(pnum)
print(snum)
#計分，利用第2行的答案來計算，並存成資料檔sdata1<- pnumxsnum
sdata1 <- matrix(0, nrow=snum, ncol=pnum)
i <- 1
while (i <= pnum){
  j <- 1
  while (j <= snum) {
    if (sdata0[1,i+1] == sdata0[j+1,i+1]) sdata1[j,i] <- 1
    j <- j+1
  }
  i <- i+1
}
#sdata1即計分結果
sdata1 <- as.data.frame(as.matrix(sdata1))
colnames(sdata1)<-names(sdata0[,2:(pnum+1)])
#檢視計分結果
head(sdata1)
#利用CTT的packages來進行誘答力分析
library(CTT)
#反應資料
data0 <- sdata0[-1,-1]
#正確答案
data1 <- sdata0[1,-1]
#誘答力分析
result0 <- distractor.analysis(data0,data1)
#顯示誘答力分析結果
head(result0,2)
#進行試題的描述性統計，分別是平均數、標準差、偏態與峰度的計算
#定義my_stats函數，平均數、標準差、偏態與峰度
library(moments)
my_stats <- function(x) {
  funs <- c(mean, sd, skewness, kurtosis)
  sapply(funs, function(f) f(x, na.rm = TRUE))
}
#計算平均數、標準差、偏態以及峰度
sdata1_desc <- apply(sdata1, 2, my_stats)
#顯示計算結果，第一列是平均數、第二列是標準差、第三列是偏態、第四列是峰度
sdata1_desc
#為方便識別，加上中文識別
rownames(sdata1_desc) <- c("難度值", "標準差", "偏態", "峰度")
#將資料予以轉置並儲存
result1 <- as.data.frame(t(sdata1_desc))
row.names(result1) <- names(sdata0[,2:(pnum+1)])
head(round(result1,3))

#計算分組難度、鑑別度以及CR值
sdata1$tot <- apply(sdata1, 1, sum)
head(sdata1)
sdata1$grp <- NA
LB=0
HB=0
LB=quantile(sdata1$tot,probs=c(0.27))
HB=quantile(sdata1$tot,probs=c(0.73))
sdata1$grp[sdata1$tot <= LB] <- "L"
sdata1$grp[sdata1$tot >= HB] <- "H"
head(sdata1)
sdata1$grp <- factor(sdata1$grp)
sdata2 <- aggregate(sdata1[,1:pnum], by=list(sdata1$grp), mean)
sdata2 <- t(sdata2[,-1])
item_t <- sapply(sdata1[,1:pnum], function(x) c(t.test(x ~ sdata1$grp)$statistic,t.test(x ~ sdata1$grp)$p.value))
result2 <- data.frame(Item=rownames(sdata2),m.l=sdata2[,2], m.h=sdata2[,1], m.a=(sdata2[,2]+sdata2[,1])/2, m.dif=sdata2[,1]-sdata2[,2], t.stat=item_t[1,], t.p=item_t[2,])
head(result2)
#將題目的編號去除
result2 <- result2[,-1]
#中文命名欄位
names(result2) <- c('低分組難度','高分組難度','分組難度', '鑑別度','CR值','p值')
row.names(result2) <- names(sdata0[,2:(pnum+1)])
#取三位小數
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
#這是題目信度
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
head(round(resultall, 3))

#將結果存至檔案
write.csv(round(resultall,3), "試題分析結果.csv")