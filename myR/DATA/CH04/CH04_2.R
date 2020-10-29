#單因子變異數分析 2017/08/04
#檔名CH04_2.R 資料檔CH04_1.csv
#設定工作目錄
setwd("D:/DATA/CH04/")
#讀取資料檔
library(readr)
sdata0 <- read_csv("CH04_1.csv")
#檢視前後六筆資料
head(sdata0)
tail(sdata0)
#要將GENDER以及JOB轉為字串
sdata0$JOB <- factor(sdata0$JOB)
print(sdata0$JOB)
sdata0$JOB <- factor(sdata0$JOB, levels = c(1,2,3,4), labels = c("主任", "組長","科任","級任"))
print(head(sdata0$JOB))
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

library(Rmisc)
#單因子變異數分析
#1.列出樣本摘要
summarySE(data=sdata0, groupvars="JOB", measurevar="SA01")
#2.變異數同質性檢定
library(DescTools)
LeveneTest(SA01 ~ JOB, data=sdata0, center=mean)
#3.變異數分析(摘要表)
mjob <- aov(SA01 ~ JOB, data=sdata0)
anova(mjob)
#4-1.因為達顯著需要事後比較
#LSD
library(agricolae)
mLSD <- LSD.test(mjob, "JOB")
print(mLSD)
#Turkey
mTUKEY <- HSD.test(mjob, "JOB")
print(mTUKEY)
#Scheffe
mSCHEFFE <- scheffe.test(mjob, "JOB")
print(mSCHEFFE)

#事後比較的另一個方法
#LSD另一種方法
library(DescTools)
mLSD <- PostHocTest(mjob, method="lsd")
print(mLSD)
#Tukey另一種方法
mTUKEY <- TukeyHSD(mjob)
print(mTUKEY)
plot(mTUKEY)
#scheffe另一種方法
mSCHEFFE <- ScheffeTest(mjob)
#另一種語法
mSCHEFFE <- PostHocTest(mjob, method="scheffe")
print(mSCHEFFE)
plot(mSCHEFFE)

#4-2.變異數不同質的事後比較方法
#利用Dunnnett C
library(DTK)
mDTK <- with(sdata0, DTK.test(SA01, JOB))
print(mDTK)
DTK.plot(mDTK)
abline(v=0)

#5.計算效果量
library(DescTools)
mETA <- EtaSq(mjob, anova=TRUE)
print(mETA)