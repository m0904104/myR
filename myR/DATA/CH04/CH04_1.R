#t考驗 2017/08/04
#檔名CH04_1.R 資料檔CH04_1.csv
#設定工作目錄
setwd("D:/DATA/CH04/")
#讀取資料檔
library(readr)
sdata0 <- read_csv("CH04_1.csv")
#檢視前後六筆資料
head(sdata0)
tail(sdata0)
#要將GENDER以及JOB轉為字串
sdata0$GENDER <- factor(sdata0$GENDER)
print(sdata0$GENDER)
#計算分量表總和
#1:ID
#2:GENDER
#3:JOB
#4-9:A0101-A0106    參與校務決策
#10-15:A0201-A0206  展現教室領導
#16-19:A0301-A0304  促進同儕合作
#20-22:A0401-A0403  提升專業成長
sdata0$A01 <- apply(sdata0[4:9],1,sum)
sdata0$A02 <- apply(sdata0[10:15],1,sum)
sdata0$A03 <- apply(sdata0[16:19],1,sum)
sdata0$A04 <- apply(sdata0[20:22],1,sum)
sdata0$A00 <- apply(sdata0[4:22],1,sum)
sdata0$SA01 <- apply(sdata0[4:9],1,mean)
sdata0$SA02 <- apply(sdata0[10:15],1,mean)
sdata0$SA03 <- apply(sdata0[16:19],1,mean)
sdata0$SA04 <- apply(sdata0[20:22],1,mean)
sdata0$SA00 <- apply(sdata0[4:22],1,mean)
head(sdata0)
#獨立樣本t考驗
#1.先列出樣本摘要
library(Rmisc)
summarySE(data=sdata0, groupvars="GENDER", measurevar="SA01")
#2.變異數同質性檢定
library(DescTools)
#變異數同質性檢定，內定值是中位數，SPSS是採用平均數，為了比較起見，範例以mean示範
LeveneTest(SA01 ~ GENDER, data=sdata0, center=mean)
#3.t考驗(變異數不同質的方法)
mgender1 <- t.test(SA01 ~ GENDER, data=sdata0)
print(mgender1)
#3.t考驗(變異數同質的方法)
mgender2 <-t.test(SA01 ~ GENDER, data=sdata0, var.equal=TRUE)
print(mgender2)