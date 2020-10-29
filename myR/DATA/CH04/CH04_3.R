#相依樣本平均數考驗 2017/08/04
#檔名CH04_3.R 資料檔CH04_1.csv
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

#相依樣本單因子變異數分析
#1.列出樣本摘要
library(Rmisc)
summarySE(data=sdata0, groupvars="JOB", measurevar="SA01")
summarySE(data=sdata0, groupvars="JOB", measurevar="SA02")
summarySE(data=sdata0, groupvars="JOB", measurevar="SA03")
summarySE(data=sdata0, groupvars="JOB", measurevar="SA04")

#相依樣本變異數分析
head(sdata0[c("ID","GENDER","SA01","SA02","SA03","SA04")])
make.rm<-function(constant,repeated,data,contrasts) {
  if(!missing(constant) && is.vector(constant)) {
    if(!missing(repeated) && is.vector(repeated)) {
      if(!missing(data)) {
        dd<-dim(data)
        replen<-length(repeated)
        if(missing(contrasts))
          contrasts<-
          ordered(sapply(paste("T",1:length(repeated),sep=""),rep,dd[1]))
        else
          contrasts<-matrix(sapply(contrasts,rep,dd[1]),ncol=dim(contrasts)[2])
        if(length(constant) == 1) cons.col<-rep(data[,constant],replen)
        else cons.col<-lapply(data[,constant],rep,replen)
        new.df<-data.frame(cons.col,
                           repdat=as.vector(data.matrix(data[,repeated])),
                           contrasts)
        return(new.df)
      }
    }
  }
}

pdata <- make.rm(constant=c("ID","JOB"),repeated=c("SA01","SA02","SA03","SA04"),data=sdata0)
head(pdata)
presult1 <-aov(repdat~contrasts+ID,pdata)
summary(presult1)
#進行事後比較
library(multcomp)
presult3 <- glht(presult1, linfct=mcp(contrasts="Tukey"))
summary(presult3)
#計算效果量
library(DescTools)
mETA <- EtaSq(presult1, anova=TRUE)
print(mETA)