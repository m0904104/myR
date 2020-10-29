#量表之試題分析 2017/07/30 有分量表
#檔名CH02_3.R 資料檔CH02_2.csv
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
#計分，利用第2行的答案來計算，並存成資料檔sdata1<- pnumxsnum
sdata1 <- sdata0[,-1]

#定義my_stats函數，平均數、標準差、偏態與峰度
library(moments)
my_stats <- function(x) {
  funs <- c(mean, sd, skewness, kurtosis)
  sapply(funs, function(f) f(x, na.rm = TRUE))
}

#計算平均數、標準差、偏態以及峰度
sdata1_desc <- apply(sdata1, 2, my_stats)
rownames(sdata1_desc) <- c("平均數", "標準差", "偏態", "峰度")
result1 <- as.data.frame(t(sdata1_desc))
row.names(result1) <- names(sdata0[,2:(pnum+1)])
round(result1,3)

#計算分組難度、鑑別度以及CR值
sdata1$tot <- apply(sdata1, 1, sum)
sdata1$grp <- NA
LB=0
HB=0
LB=quantile(sdata1$tot,probs=c(0.27))
HB=quantile(sdata1$tot,probs=c(0.73))
sdata1$grp[sdata1$tot <= LB] <- "L"
sdata1$grp[sdata1$tot >= HB] <- "H"
sdata1$grp <- factor(sdata1$grp)
sdata2 <- aggregate(sdata1[,1:pnum], by=list(sdata1$grp), mean)
sdata2 <- t(sdata2[,-1])
item_t <- sapply(sdata1[,1:pnum], function(x) c(t.test(x ~ sdata1$grp, var.equal=TRUE)$statistic,t.test(x ~ sdata1$grp, var.equal=TRUE)$p.value))
result2 <- data.frame(Item=rownames(sdata2),m.l=sdata2[,2], m.h=sdata2[,1], m.a=(sdata2[,2]+sdata2[,1])/2, m.dif=sdata2[,1]-sdata2[,2], t.stat=item_t[1,], t.p=item_t[2,])
result2 <- result2[,-1]
names(result2) <- c('低分組平均','高分組平均','差異', '鑑別度','CR值','p值')
row.names(result2) <- names(sdata0[,2:(pnum+1)])
head(round(result2,3))

#利用psychometrics套件，計算題目與總分相關
library(psychometric)
itotr <- item.exam(sdata1[, 1:pnum], discrim = TRUE)
head(itotr)
#以下是需要計算分量表的相關時才需要(4分量表)
#B01 6 1-6
#B02 6 7-12
#B03 6 13-18
#B04 6 19-24
#將資料轉換為包含4個資料框架的列
ldta <- list(x1=sdata1[,1:6],x2=sdata1[,7:12],x3=sdata1[,13:18],x4=sdata1[,19:24])
#題目與分量表總分相關
isubr <- lapply(ldta, item.exam, discrim = TRUE)
#檢視第1個分量
head(isubr,1)
#結合分析結果
result3 <- t(rbind(itotr$Item.Tot.woi,
c(isubr$x1$Item.Tot.woi,isubr$x2$Item.Tot.woi,isubr$x3$Item.Tot.woi,
  isubr$x4$Item.Tot.woi)))
result3 <- data.frame(result3)
names(result3) <- c('題目總分相關','題目分量表相關')
row.names(result3) <- names(sdata0[,2:(pnum+1)])
head(round(result3,3))

#計算刪題後信度
isubrel <- c(isubr$x1$Item.Rel.woi,isubr$x2$Item.Rel.woi,isubr$x3$Item.Rel.woi,
             isubr$x4$Item.Rel.woi)
detach("package:psychometric", unload = TRUE)

#利用psych套件，計算刪題後信度
library(psych)
itotalpha <- alpha(sdata1[, 1:pnum],check.keys=TRUE)$alpha.drop[,'raw_alpha']
#計算刪題後分量表
isubalpha <- lapply(ldta, alpha)
ialpha <- c(isubalpha$x1$alpha.drop[,'raw_alpha'],
            isubalpha$x2$alpha.drop[,'raw_alpha'],
            isubalpha$x3$alpha.drop[,'raw_alpha'],
            isubalpha$x4$alpha.drop[,'raw_alpha']
            )
result4 <- as.data.frame(t(rbind(itotalpha, ialpha, itotr$Item.Rel.woi)))
names(result4) <- c('總量表信度(刪題)','分量表信度(刪題)','題目信度')
row.names(result4) <- names(sdata0[,2:(pnum+1)])
head(round(result4, 3))

#合併刪題後信度
resultall <- cbind(result2, result3, result4)
round(resultall, 3)

#將結果存至檔案
write.csv(round(resultall,3), "多元試題分析結果.csv")
