#量表之測驗分析 2017/07/30 探索式因素分析
#檔名CH03_2.R 資料檔CH03_1.csv
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
print(pnum)
print(snum)
#讀取資料,去除編號欄位
sdata1 <- sdata0[,-1]
head(sdata1)
#因素分析計算效度
library(psych)
#探索式因素分析
#KMO
KMO(sdata1)
#Bartlett球形檢定
cortest.bartlett(sdata1)
#陡坡圖
fa.parallel(sdata1, fa="pc", show.legend = FALSE)
print.psych(fa(sdata1, fm="pa", nfactor=4, rotate="varimax"),cut=0.35,sort=TRUE)
#刪除B302(14),301(13),204(10)
print.psych(fa(sdata1[-c(10,13,14)], fm="pa", nfactor=4, rotate="varimax"),cut=0.35,sort=TRUE)
#第3次，刪除205(11)
print.psych(fa(sdata1[-c(10,11,13,14)], fm="pa", nfactor=4, rotate="varimax"),cut=0.35,sort=TRUE)

#交叉負荷情形(cross loading)採用斜交轉軸
print.psych(fa(sdata1[-c(10,11,13,14)], fm="pa", nfactor=4, rotate="promax"),cut=0.35,sort=TRUE)
#因素分析結果繪圖
fit1 <-fa(sdata1[-c(10,11,13,14)], fm="pa", nfactor=4, rotate="promax")
fa.diagram(fit1,cut=0.35,sort=TRUE,digits=2)

#計算刪題後各分量表的信度
#開始計算
#刪除10,11,13,14
#X1:1:6
#X2:7:9,12
#X3:15:18
#X4:19:24
alpha(sdata1[-c(10,11,13,14)])
alpha(sdata1[-c(10,11,13,14)])$total$raw_alpha
#假如有分量表可以利用list一次計算
subdata <- list(x0=sdata1[-c(10,11,13,14)], x1=sdata1[,1:6], x2=sdata1[,c(7,8,9,12)], x3=sdata1[,15:18], x4=sdata1[,19:24])

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

#主成份分析
library(psych)
fit2 <- principal(sdata1, nfactors=4, rotate="varimax")
print(fit2, digits=2, cut=0.35, sort=TRUE)

fit3 <- principal(sdata1, nfactors=4, rotate="promax")
print(fit3, digits=2, cut=0.35, sort=TRUE)
fit4<- principal(sdata1[-c(9,10,13,14)],nfactors=4,rotate="promax")
print(fit4, digits=2, cut=0.35, sort=TRUE)
fit5<- principal(sdata1[-c(9,10,11,13,14)],nfactors=4,rotate="promax")
print(fit5, digits=2, cut=0.35, sort=TRUE)
#顯示因素分析結果中的因素負荷量
print(loadings(fit5),digits=2,cut=0.35, sort=TRUE)
#繪製圖形
plot(fit5)
print(fit5$scores,digits=2)