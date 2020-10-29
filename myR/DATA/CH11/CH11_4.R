#多元計分IRT分析利用mirt套件
#試題反應理論之分析 2017/08/14
#檔名CH011_4.R
library(mirt)
library(psych)
#開啟所需資料檔
data(bfi)
head(bfi)
IRTgrm.items <- bfi[,1:25]
IRTgrm3=mirt(data=IRTgrm.items, model=1, itemtype="graded")
summary(IRTgrm3)
coef(IRTgrm3, IRTpars = T)
#繪製試題特徵曲線
itemplot(IRTgrm3,1,type="trace")
#繪製試題訊息曲線
itemplot(IRTgrm3,5,type="info")
#繪製所有試題的特徵曲線
plot(IRTgrm3, type="trace")
#繪製測驗訊息曲線
plot(IRTgrm3, type="info")